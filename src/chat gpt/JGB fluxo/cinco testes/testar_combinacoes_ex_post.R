# Experimento isolado: macros realizadas; fluxo realizado no ARIMA e previsto no VAR.
# Alvo do CV: nivel do spread 30Y-5Y EXATAMENTE 12 meses a frente.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco testes/00_configuracao.R',encoding='UTF-8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(fpp3))
suppressPackageStartupMessages(library(tidyverse))
source(file.path(pasta_testes,'01_dados.R'),encoding='UTF-8')
source(file.path(pasta_testes,'09_selecao_satelite.R'),encoding='UTF-8')
destino <- file.path(pasta_testes,'resultados','combinacoes ex post')
dir.create(destino,recursive=TRUE,showWarnings=FALSE)
d <- ler_insumos()
boj <- jsonlite::fromJSON(file.path(fonte_dados,'BOJ_STRACLUCON_2026-11-11.json'))
curto <- tibble(date=yearmonth(ym(boj$RESULTSET$VALUES$SURVEY_DATES[[1]])),
  call_jp=as.numeric(boj$RESULTSET$VALUES$VALUES[[1]]))
fed <- read_csv(file.path(fonte_dados,'FEDFUNDS_FRED_2026-11-11.csv'),show_col_types=FALSE) |>
  transmute(date=yearmonth(observation_date),fed=FEDFUNDS)
banco <- d$historico |> as_tibble() |> arrange(date) |>
  transmute(date,spread=100*inclinacao,d_spread=100*(inclinacao-lag(inclinacao)),
    cambio=usdjpy-lag(usdjpy),inflacao=100*(exp((cpi_core-lag(cpi_core,12))/100)-1)) |>
  left_join(d$aux |> select(date,fluxo,ois_pos,fluxo_l2),by='date') |>
  left_join(curto,by='date') |> left_join(fed,by='date') |>
  mutate(diferencial=fed-call_jp) |>
  preparar_lags_satelite(c('diferencial','cambio','inflacao'))
for(v in c('fluxo','d_spread')) for(j in 1:3)
  banco[[paste0(v,'_l',j)]] <- lag(banco[[v]],j)
colunas <- c('spread','d_spread','fluxo','ois_pos','fluxo_l2',
  unlist(lapply(c('diferencial','cambio','inflacao'),function(x) paste0(x,'_l',0:12))),
  paste0('fluxo_l',1:3),paste0('d_spread_l',1:3))
banco <- banco |> filter(date>=yearmonth(INICIO),date<=d$origem,
  if_all(all_of(colunas),is.finite)) |> as_tsibble(index=date)
combos <- c(list(base=character()),set_names(unlist(lapply(1:3,function(k)
  combn(c('diferencial','cambio','inflacao'),k,simplify=FALSE)),recursive=FALSE),
  paste0('C',1:7)))
BASE <- c('ois_pos','fluxo_l2')

fit_arma <- function(dados,termos) {
  f <- as.formula(paste('d_spread ~',paste(termos,collapse=' + '),
    '+ pdq(p=0:3,d=0,q=0:3) + PDQ(0,0,0)'))
  dados |> model(arima=ARIMA(!!f,stepwise=FALSE,approximation=FALSE))
}
escolher_lag <- function(x,dados) {
  tab <- map_dfr(names(specs_satelite),function(s) {
    m <- fit_arma(dados,c(BASE,paste0(x,'_l',specs_satelite[[s]])))
    tibble(spec=s,AICc=glance(m)$AICc,n=length(specs_satelite[[s]]),
      menor=min(specs_satelite[[s]]))
  }) |> filter(is.finite(AICc)) |> mutate(delta=AICc-min(AICc)) |>
    filter(delta<2) |> arrange(n,desc(menor),AICc) |> slice_head(n=1)
  stopifnot(nrow(tab)==1L)
  paste0(x,'_l',specs_satelite[[tab$spec]])
}
ajustar_arima <- function(comb,dados,escolhas) {
  termos <- c(BASE,unlist(escolhas[comb],use.names=FALSE))
  m <- fit_arma(dados,termos)
  co <- tidy(m)
  teste <- co |> filter(term %in% termos) |> select(term,p.value)
  list(modelo=m,termos=termos,teste=teste,
    aprovado=nrow(teste)==length(termos) && all(is.finite(teste$p.value) & teste$p.value<=.1))
}
ajustar_var <- function(comb,dados) {
  ex <- c('ois_pos',if(length(comb)) paste0(comb,'_l0') else character())
  modelos <- lapply(1:3,function(p) {
    lags <- c(paste0('fluxo_l',1:p),paste0('d_spread_l',1:p))
    rhs <- c(ex,lags)
    f <- as.formula(paste('cbind(fluxo,d_spread) ~',paste(rhs,collapse=' + ')))
    m <- lm(f,data=dados)
    E <- residuals(m); n <- nrow(E); k <- length(coef(m)) + 3L
    sig <- crossprod(E)/n
    ll <- -n*(2*(log(2*pi)+1)+as.numeric(determinant(sig,logarithm=TRUE)$modulus))/2
    aicc <- -2*ll+2*k+2*k*(k+1)/(n-k-1)
    list(modelo=m,p=p,AICc=aicc,rhs=rhs)
  })
  a <- modelos[[which.min(vapply(modelos,`[[`,numeric(1),'AICc'))]]
  co <- summary(a$modelo)[[2]]$coefficients
  # Exogenas individuais e bloco das defasagens do fluxo na equacao do spread.
  teste <- tibble(term=ex,p.value=co[ex,4])
  alvo <- lm(as.formula(paste('d_spread ~',paste(a$rhs,collapse=' + '))),data=dados)
  ix <- paste0('fluxo_l',1:a$p); b <- coef(alvo)[ix]; V <- vcov(alvo)[ix,ix,drop=FALSE]
  F <- as.numeric(t(b)%*%solve(V,b))/length(ix)
  teste <- bind_rows(teste,tibble(term='fluxo: bloco de lags',
    p.value=pf(F,length(ix),df.residual(alvo),lower.tail=FALSE)))
  a$teste <- teste
  a$aprovado <- all(is.finite(teste$p.value) & teste$p.value<=.1)
  a
}
prever_var <- function(a,treino,janela) {
  hist <- treino |> as_tibble() |> select(fluxo,d_spread) |> as.matrix()
  out <- matrix(NA_real_,nrow(janela),2)
  for(t in seq_len(nrow(janela))) {
    linha <- as.list(janela[t,])
    for(j in 1:a$p) {
      linha[[paste0('fluxo_l',j)]] <- unname(hist[nrow(hist)-j+1L,1])
      linha[[paste0('d_spread_l',j)]] <- unname(hist[nrow(hist)-j+1L,2])
    }
    X <- c('(Intercept)'=1,unlist(linha[a$rhs]))
    out[t,] <- drop(X[rownames(coef(a$modelo))]%*%coef(a$modelo))
    hist <- rbind(hist,out[t,])
  }
  stopifnot(all(is.finite(out)))
  out
}

# Executar uma janela por vez; dentro de cada janela os ajustes sao independentes.
cl <- parallel::makePSOCKcluster(3L)
parallel::clusterEvalQ(cl,{
  .libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
  suppressPackageStartupMessages(library(fpp3)); suppressPackageStartupMessages(library(tidyverse)); NULL
})
parallel::clusterExport(cl,c('specs_satelite','BASE','fit_arma','escolher_lag','ajustar_arima','ajustar_var'))
ajustar_janela <- function(dados) {
  xs <- c('diferencial','cambio','inflacao')
  escolhas <- set_names(parallel::parLapply(cl,xs,escolher_lag,dados=dados),xs)
  ar <- parallel::parLapply(cl,combos,ajustar_arima,dados=dados,escolhas=escolhas)
  va <- lapply(combos,ajustar_var,dados=dados)
  c(set_names(ar,paste0('ARIMA_',names(combos))),set_names(va,paste0('VAR_',names(combos))))
}
message('Ajustes na amostra completa: ',nrow(banco),' meses.')
arq_modelos <- file.path(destino,'modelos.rds')
modelos <- if(file.exists(arq_modelos)) readRDS(arq_modelos) else NULL
cache_ok <- !is.null(modelos) && length(modelos)==16L &&
  all(vapply(modelos[startsWith(names(modelos),'ARIMA')],function(a) {
    old <- augment(a$modelo) |> as_tibble()
    identical(old$date,banco$date) && isTRUE(all.equal(old$d_spread,banco$d_spread)) &&
      all(vapply(intersect(a$termos,names(old)),function(x)
        isTRUE(all.equal(old[[x]],banco[[x]])),logical(1)))
  },logical(1)))
if(!cache_ok) modelos <- ajustar_janela(banco) else message('Reutilizando ajustes completos ja conferidos.')
saveRDS(modelos,file.path(destino,'modelos.rds'))
triagem <- imap_dfr(modelos,function(a,id) tibble(modelo=id,aprovado=a$aprovado,
  reprovadas=paste(a$teste$term[!is.finite(a$teste$p.value) | a$teste$p.value>.1],collapse='; ')))
pvalores <- imap_dfr(modelos,~mutate(.x$teste,modelo=.y))
write_csv(triagem,file.path(destino,'triagem significancia.csv'))
write_csv(pvalores,file.path(destino,'p valores.csv'))
write_csv(imap_dfr(combos,~tibble(combinacao=.y,variaveis=paste(.x,collapse=' + '))),
  file.path(destino,'combinacoes.csv'))
print(triagem,n=Inf)

diagnosticos <- list()
for(id in names(modelos)) {
  a <- modelos[[id]]; pasta <- file.path(destino,'todos modelos',id)
  dir.create(pasta,recursive=TRUE,showWarnings=FALSE)
  if(startsWith(id,'ARIMA')) {
    diagnosticos[[id]] <- diagnosticar_satelite(a$modelo) |> mutate(modelo=id,equacao='spread')
    g <- grafico_residuos_satelite(a$modelo,'spread')
    g[[1]] <- g[[1]]+labs(title=paste(id,if(a$aprovado) '| Significancia aprovada' else '| Significancia reprovada'))
    ggsave(file.path(pasta,'residuos spread.png'),g,width=12,height=8,dpi=140,bg='white')
  } else for(j in 1:2) {
    eq <- c('fluxo','spread')[j]; e <- residuals(a$modelo)[,j]
    diag <- map_dfr(c(12L,24L),function(k) tibble(lag_teste=k,
      p_ljung_box=Box.test(e,lag=k,type='Ljung-Box',fitdf=0)$p.value))
    # VAR: teste marginal descritivo sem ajuste univariado p+q; nao e Portmanteau multivariado.
    diagnosticos[[paste(id,eq)]] <- mutate(diag,modelo=id,equacao=eq)
    z <- tibble(date=banco$date,erro=e) |> as_tsibble(index=date)
    g <- ggtime::gg_tsdisplay(z,erro,plot_type='histogram',lag_max=24)
    g[[1]] <- g[[1]]+labs(title=paste(id,'|',eq),
      subtitle='Residuos VAR; ACF marginal descritiva, sem teste estrutural de causalidade')
    ggsave(file.path(pasta,paste0('residuos ',eq,'.png')),g,width=12,height=8,dpi=140,bg='white')
  }
}
write_csv(bind_rows(diagnosticos),file.path(destino,'diagnosticos residuos.csv'))

origens <- seq(yearmonth('2024 Oct'),max(banco$date)-12L,by=1L)
cv <- list()
for(i in seq_along(origens)) {
  o <- origens[i]; treino <- banco |> filter(date<=o)
  janela <- banco |> filter(date>o,date<=o+12L)
  stopifnot(nrow(janela)==12L)
  fits <- ajustar_janela(treino)
  for(id in names(fits)) {
    a <- fits[[id]]
    if(startsWith(id,'ARIMA')) {
      novos <- janela |> as_tibble() |> select(date,all_of(a$termos)) |> as_tsibble(index=date)
      delta <- forecast(a$modelo,new_data=novos)$.mean
    } else delta <- prever_var(a,treino,janela)[,2]
    cv[[paste(i,id)]] <- tibble(origem=as.Date(o),modelo=id,
      previsto=tail(treino$spread,1)+sum(delta),observado=tail(janela$spread,1),
      significativo_no_treino=a$aprovado)
  }
  cv[[paste(i,'bench')]] <- tibble(origem=as.Date(o),modelo=c('Naive','Drift'),
    previsto=c(tail(treino$spread,1),tail(treino$spread,1)+12*mean(treino$d_spread)),
    observado=tail(janela$spread,1),significativo_no_treino=NA)
  write_csv(bind_rows(cv),file.path(destino,'CV parcial.csv'))
  message('CV 12 MESES: ',i,'/',length(origens),' | ',format(o))
}
parallel::stopCluster(cl)
previsoes <- bind_rows(cv)
metricas <- previsoes |> group_by(modelo) |> summarise(origens=n(),
  rmse_pb=sqrt(mean((previsto-observado)^2)),mae_pb=mean(abs(previsto-observado)),
  fracao_treinos_significativos=mean(significativo_no_treino),.groups='drop') |>
  left_join(triagem,by='modelo') |> arrange(rmse_pb)
write_csv(previsoes,file.path(destino,'CV previsoes.csv'))
write_csv(metricas,file.path(destino,'CV todos.csv'))
aprovados <- metricas |> filter(aprovado %in% TRUE | modelo %in% c('Naive','Drift'))
write_csv(aprovados,file.path(destino,'CV significativos.csv'))
g <- ggplot(aprovados,aes(reorder(modelo,-rmse_pb),rmse_pb))+geom_col(fill='#365A78')+
  geom_text(aes(label=round(rmse_pb,1)),vjust=-.4)+theme_minimal()+
  labs(title='CV do spread: horizonte de 12 meses',subtitle='Apenas modelos com variaveis significativas na amostra completa e benchmarks',
    x=NULL,y='RMSE (pb)',caption='ARIMA: fluxo realizado. VAR: fluxo previsto. Macros realizadas em ambos. Triagem exploratoria; informacoes futuras diferentes.')+
  theme(axis.text.x=element_text(angle=35,hjust=1))
ggsave(file.path(destino,'CV significativos.png'),g,width=12,height=6,dpi=150,bg='white')
for(id in triagem$modelo[triagem$aprovado]) {
  p <- file.path(destino,'significativos',id); dir.create(p,recursive=TRUE,showWarnings=FALSE)
  file.copy(list.files(file.path(destino,'todos modelos',id),full.names=TRUE),p,overwrite=TRUE)
}
writeLines(c('CV h=12 exato; origens out/2024 a ultimo horizonte completo; nivel do spread em pb.',
  'Diferencial = Fed Funds efetivo - call rate BoJ (%). Cambio = 100*dlog USDJPY. Inflacao = CPI core 12m (%).',
  'ARIMA: fluxo realizado lag 2 e OIS pos-YCC realizados, mais subconjuntos de macros realizadas.',
  'VAR: fluxo e Delta spread conjuntos; macros e OIS realizados, fluxo futuro NUNCA usado na recursao.',
  'ARIMA: estruturas A/B por regressor; p/q 0:3 por AICc; d=0 sobre Delta spread. VAR: p=1:3 por AICc.',
  'Corte p<=10% nominal: termos economicos, sem intercepto/ARMA. VAR: lags do fluxo testados conjuntamente na equacao do spread.',
  'Triagem feita na amostra completa; CV de todas as opcoes, sem excluir janelas que perdem significancia.',
  'A selecao final e exploratoria, com multiplas comparacoes. Conjuntos de informacao ARIMA/VAR diferem.',
  'Graficos VAR e Ljung-Box marginais sao descritivos, nao Portmanteau multivariado. Nenhum BVAR alterado.'),
  file.path(destino,'LEIA-ME.txt'))
print(metricas,n=Inf,width=Inf)
