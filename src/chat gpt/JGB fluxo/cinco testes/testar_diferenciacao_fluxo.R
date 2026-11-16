# Teste isolado autorizado: transformacoes do fluxo. Nao altera o BVAR ou o satelite ativo.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco testes/00_configuracao.R',encoding='UTF-8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
library(fpp3)
library(tidyverse)
source(file.path(pasta_testes,'01_dados.R'),encoding='UTF-8')
source(file.path(pasta_testes,'09_selecao_satelite.R'),encoding='UTF-8')
destino <- file.path(pasta_testes,'resultados','teste diferenciacao fluxo')
dir.create(destino,recursive=TRUE,showWarnings=FALSE)
d <- ler_insumos()
banco <- d$aux |> arrange(date) |>
  mutate(d_fluxo=fluxo-lag(fluxo),d_ust30=ust30-lag(ust30)) |>
  preparar_lags_satelite(c('ust30','d_ust30')) |>
  filter(date>=yearmonth(INICIO),date<=d$origem,
    if_all(c(fluxo,d_fluxo,ust30_l12,d_ust30_l12),is.finite)) |>
  as_tsibble(index=date)
variantes <- list(
  nivel_d0=list(y='fluxo',x='ust30',d=0L,acumular=FALSE),
  nivel_d1=list(y='fluxo',x='ust30',d=1L,acumular=FALSE),
  delta_fluxo=list(y='d_fluxo',x='ust30',d=0L,acumular=TRUE),
  delta_ambos=list(y='d_fluxo',x='d_ust30',d=0L,acumular=TRUE))

# Em cada transformacao: mesmas familias do IP, p/q por AICc, mesma amostra.
# NAO comparar AICc entre dependentes ou ordens de diferenciacao diferentes.
ajustar_variante <- function(treino,nome) {
  v <- variantes[[nome]]
  modelos <- list()
  tabela <- map_dfr(names(specs_satelite),function(spec) {
    termos <- paste0(v$x,'_l',specs_satelite[[spec]])
    f <- as.formula(paste(v$y,'~',paste(termos,collapse=' + '),
      '+ pdq(p=0:3,d=',v$d,',q=0:3) + PDQ(0,0,0)'))
    m <- treino |> model(arima=ARIMA(!!f,stepwise=FALSE,approximation=FALSE))
    if(any(vapply(m$arima,inherits,logical(1),'null_mdl')))
      return(tibble(spec,n_termos=length(termos),menor_lag=min(specs_satelite[[spec]]),AICc=Inf))
    modelos[[spec]] <<- m
    tibble(spec,n_termos=length(termos),menor_lag=min(specs_satelite[[spec]]),AICc=glance(m)$AICc)
  }) |> filter(is.finite(AICc)) |> mutate(delta_AICc=AICc-min(AICc))
  escolha <- tabela |> filter(delta_AICc<2) |>
    arrange(n_termos,desc(menor_lag),AICc) |> slice_head(n=1)
  stopifnot(nrow(escolha)==1L)
  list(modelo=modelos[[escolha$spec]],escolha=escolha,tabela=tabela)
}

arquivo_ajustes <- file.path(destino,'ajustes.rds')
ajustes <- if(file.exists(arquivo_ajustes)) readRDS(arquivo_ajustes) else NULL
cache_compativel <- !is.null(ajustes) && identical(names(ajustes),names(variantes)) &&
  all(vapply(names(variantes),function(nome) {
    v <- variantes[[nome]]
    antigo <- augment(ajustes[[nome]]$modelo) |> as_tibble()
    cols <- c(v$y,grep(paste0('^',v$x,'_l'),names(antigo),value=TRUE))
    identical(antigo$date,banco$date) && all(cols %in% names(banco)) &&
      all(vapply(cols,function(x) isTRUE(all.equal(antigo[[x]],banco[[x]])),logical(1)))
  },logical(1)))
if(!cache_compativel) ajustes <- set_names(lapply(names(variantes),function(nome) {
    message('AMOSTRA COMPLETA | ',nome)
    ajustar_variante(banco,nome)
  }),names(variantes)) else message('Reutilizando os quatro ajustes ja concluidos e conferidos.')
diagnosticos <- imap_dfr(ajustes,function(a,nome)
  diagnosticar_satelite(a$modelo) |> mutate(variante=nome,spec=a$escolha$spec))
coeficientes <- imap_dfr(ajustes,~tidy(.x$modelo) |> mutate(variante=.y))
escolhas <- imap_dfr(ajustes,~mutate(.x$escolha,variante=.y))
write_csv(diagnosticos,file.path(destino,'diagnosticos.csv'))
write_csv(coeficientes,file.path(destino,'coeficientes.csv'))
write_csv(escolhas,file.path(destino,'escolhas.csv'))
write_csv(imap_dfr(ajustes,~mutate(.x$tabela,variante=.y)),file.path(destino,'candidatos.csv'))
saveRDS(ajustes,file.path(destino,'ajustes.rds'))
print(diagnosticos,width=Inf,n=Inf)
print(escolhas,width=Inf)

# CV apenas do fluxo: mesmos 12 meses e origens; lags e p/q reescolhidos em cada treino.
# UST futuro realizado: previsao condicional, nao real time. Nenhum fluxo futuro no treino.
# Delta de ambos e a verificacao explicita do d=1 interno; CV concentra nos tres caminhos.
origens <- seq(yearmonth(CV_INICIO),max(banco$date)-12L,by=1L)
nomes_cv <- c('nivel_d0','nivel_d1','delta_fluxo')
previsoes <- list()
arquivo_parcial <- file.path(destino,'CV previsoes parciais.csv')
origens_concluidas <- as.Date(character())
if(cache_compativel && file.exists(arquivo_parcial)) {
  parcial <- read_csv(arquivo_parcial,show_col_types=FALSE)
  completas <- parcial |> group_by(origem) |> summarise(n=n(),
    modelos=n_distinct(modelo),.groups='drop') |> filter(n==72L,modelos==6L)
  parcial <- parcial |> filter(origem %in% completas$origem)
  if(nrow(parcial)) {
    stopifnot(all(parcial$observado==banco$fluxo[match(yearmonth(parcial$date),banco$date)]))
    previsoes[['salvo']] <- parcial
    origens_concluidas <- completas$origem
    message('Retomada: ',length(origens_concluidas),' origens completas preservadas.')
  }
}
for(i in seq_along(origens)) {
  o <- origens[i]
  if(as.Date(o) %in% origens_concluidas) next
  treino <- banco |> filter(date<=o)
  janela <- banco |> filter(date>o,date<=o+12L)
  stopifnot(nrow(janela)==12L)
  observado <- janela$fluxo
  # Retira a resposta e o fluxo futuro dos insumos enviados ao forecast.
  novos <- janela |> as_tibble() |>
    select(date,starts_with('ust30_l'),starts_with('d_ust30_l')) |> as_tsibble(index=date)
  for(nome in nomes_cv) {
    a <- ajustar_variante(treino,nome)
    media <- forecast(a$modelo,new_data=novos)$.mean
    if(variantes[[nome]]$acumular) media <- tail(treino$fluxo,1)+cumsum(media)
    previsoes[[paste(i,nome)]] <- tibble(origem=as.Date(o),date=as.Date(janela$date),
      h=1:12,modelo=nome,spec=a$escolha$spec,previsto=media,observado=observado)
    message('CV FLUXO | ',i,'/',length(origens),' | ',format(o),' | ',nome)
  }
  previsoes[[paste(i,'benchmarks')]] <- bind_rows(
    tibble(modelo='Naive',previsto=rep(tail(treino$fluxo,1),12)),
    tibble(modelo='Drift',previsto=tail(treino$fluxo,1)+(1:12)*mean(diff(treino$fluxo))),
    tibble(modelo='Media historica',previsto=rep(mean(treino$fluxo),12))) |>
    mutate(origem=as.Date(o),date=rep(as.Date(janela$date),3),h=rep(1:12,3),
      observado=rep(observado,3),spec=NA_character_)
  write_csv(bind_rows(previsoes),file.path(destino,'CV previsoes parciais.csv'))
}
cv <- bind_rows(previsoes)
metricas <- cv |> group_by(modelo) |> summarise(origens=n_distinct(origem),
  rmse_todos=sqrt(mean((previsto-observado)^2)),mae_todos=mean(abs(previsto-observado)),
  rmse_h12=sqrt(mean((previsto[h==12]-observado[h==12])^2)),.groups='drop') |>
  arrange(rmse_todos)
write_csv(cv,file.path(destino,'CV previsoes.csv'))
write_csv(metricas,file.path(destino,'CV metricas.csv'))
writeLines(c('Teste isolado de diferenciacao do fluxo; nao altera o modelo ativo.',
  'AICc so e comparado dentro de cada transformacao, nunca entre d ou dependentes diferentes.',
  'CV usa fluxo reconstruido em nivel, em trilhoes de ienes por mes, para todos os modelos.',
  'UST futuro realizado: teste condicional. Estruturas e ARMA selecionados em cada origem.',
  'Benchmarks auxiliares de fluxo: naive, drift e media historica. Nao e o CV do spread/BVAR.'),
  file.path(destino,'LEIA-ME.txt'))
print(metricas,width=Inf)
