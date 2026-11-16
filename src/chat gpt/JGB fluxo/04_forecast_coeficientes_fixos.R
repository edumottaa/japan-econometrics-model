# 1. Congelar coeficientes, covariancias e o forecast ORIGINAL salvo.
# O efeito adicional vem do satelite ja estimado, nao do BVAR-X reestimado.
arquivo_modelo <- file.path(fonte_modelo,'modelo.rds')
arquivo_trajetorias <- file.path(fonte_forecast,'trajetorias modelos.rds')
hash_antes <- tools::md5sum(c(arquivo_modelo,arquivo_trajetorias))
original <- readRDS(arquivo_modelo)
config <- readRDS(file.path(fonte_forecast,'forecast_bvar.rds'))
trajetorias <- readRDS(arquivo_trajetorias)
fit <- readRDS(file.path(saida,'satelite.rds'))
dados_boj <- readRDS(file.path(fonte_modelo,'dados.rds'))
y <- dados_boj |> as_tibble() |> select(all_of(variaveis)) |> as.matrix()
datas <- sort(unique(config$resumo$date))
h <- length(datas); N <- ncol(y); p <- as.integer(original$last_draw$p)
S <- dim(original$posterior$A)[3]
stopifnot(identical(config$dados_modelo$date,dados_boj$date),
          all(vapply(trajetorias,function(x) identical(dim(x),c(N,h,S)),logical(1))))
saida_fixa <- file.path(saida,'so forecast')
dir.create(saida_fixa,recursive=TRUE,showWarnings=FALSE)

# 2. Reutilizar a trajetoria do fluxo salva pelo satelite selecionado.
# Delta spread (pb) = beta_fluxo * (fluxo_lag2 projetado - referencia neutra).
# OIS e constante cancelam: comparamos dois caminhos do satelite mudando so fluxo.
beta_fluxo <- tidy(fit$estagio2) |> filter(term=='fluxo_l2') |> pull(estimate)
stopifnot(length(beta_fluxo)==1L,is.finite(beta_fluxo))
if(!is.null(fit$fluxo_futuro)) {
  fluxo_proj <- fit$fluxo_futuro
} else if(!is.null(fit$estagio1)) {
  stop('O satelite salvo usa a configuracao antiga. Execute 01_modelo_satelite.R para atualizar.')
} else stop('Trajetoria do fluxo ausente no satelite salvo.')
fluxos <- bind_rows(jsda |> filter(mes<=fim_amostra),fluxo_proj) |> arrange(mes) |>
  mutate(fluxo_l2=lag(fluxo,2))
media_fluxo <- fluxos |> filter(mes %in% yearmonth(dados_boj$date)) |>
  summarise(media=mean(fluxo_l2,na.rm=TRUE)) |> pull(media)

# Referencia = media historica na amostra BVAR, como na proposta inicial.
# E uma HIPOTESE de cenario: o BVAR sem fluxo nao identifica essa referencia.
# Ajustar so apos o snapshot de outubro/2026; setembro/outubro ficam intactos.
efeito_fluxo <- tibble(mes=datas) |> left_join(select(fluxos,mes,fluxo_l2),by='mes') |>
  mutate(referencia_tri=media_fluxo,
         diferenca_tri=if_else(mes>origem_mercado,fluxo_l2-referencia_tri,0),
         adicional_mensal_pb=beta_fluxo*diferenca_tri,
         adicional_spread_pb=cumsum(adicional_mensal_pb))
stopifnot(all(is.finite(efeito_fluxo$adicional_spread_pb)))
write_csv(efeito_fluxo |> mutate(mes=as.Date(mes)),file.path(saida_fixa,'efeito do fluxo.csv'))

# 3. Alterar apenas as trajetorias futuras, conservando os 2500 sorteios originais.
# Delta_y(t) = soma A_l ORIGINAL * Delta_y(t-l).
# Impor Delta_spread(t) = adicional acumulado do satelite. As demais domesticas
# respondem somente pelas defasagens originais. Nenhum coeficiente e reestimado.
# Nao se soma OIS/constante do satelite, nem se incorpora o BVAR-X anterior.
ajuste <- array(0,c(N,h,S))
for(s in seq_len(S)) for(t in seq_len(h)) {
  if(t>1L) for(l in seq_len(min(p,t-1L)))
    ajuste[,t,s] <- ajuste[,t,s]+drop(original$posterior$A[,N*(l-1L)+seq_len(N),s]%*%ajuste[,t-l,s])
  ajuste[1:3,t,s] <- 0
  ajuste[7,t,s] <- efeito_fluxo$adicional_spread_pb[t]/100
}
stopifnot(max(abs(ajuste[1:3,,]))==0,
          max(abs(apply(ajuste[7,,,drop=FALSE],2,mean)*100-efeito_fluxo$adicional_spread_pb))<1e-9)

# 4. Centros dos quatro cenarios e impactos nas oito variaveis.
# Reutilizar a distribuicao original; o ajuste nao incorpora incerteza do satelite.
# A banda terminal do BoJ NAO e reimposta: isso exigiria novo condicionamento.
# Reportamos os centros e a fracao dos sorteios ajustados fora da banda original.
resumir <- function(F,cenario,caminho) {
  Z <- F
  for(j in c(2,3,4,5,8)) Z[j,,] <- exp(Z[j,,]/100)
  purrr::map_dfr(seq_len(N),function(j) {
    valores <- matrix(Z[j,,],h,S)
    grafico <- valores
    if(j %in% c(4,5)) {
      passado <- matrix(rep(exp(tail(y[,j],12)/100),S),12,S)
      completo <- rbind(passado,valores)
      grafico <- 100*(completo[12+seq_len(h),]/completo[seq_len(h),]-1)
    }
    tibble(cenario,caminho,mes=datas,variavel=variaveis[j],
           valor=rowMeans(valores),valor_plot=rowMeans(grafico))
  })
}
resultados <- list(); diagnosticos <- list()
for(cen in names(trajetorias)) {
  F0 <- trajetorias[[cen]]; F1 <- F0+ajuste
  stopifnot(max(abs(F1[1:3,,]-F0[1:3,,]))<1e-12,all(is.finite(F1)))
  resultados[[cen]] <- bind_rows(resumir(F0,cen,'Original salvo'),
                                resumir(F1,cen,'Cenario adicional de fluxo'))
  diagnosticos[[cen]] <- tibble(cenario=cen,sorteios=S,
    fracao_fora_banda_boj=mean(F1[6,h,]<min(config$banda_juro) | F1[6,h,]>max(config$banda_juro)))
}
comparacao_fixa <- bind_rows(resultados)
ref <- comparacao_fixa |> filter(caminho=='Original salvo') |>
  select(cenario,mes,variavel,referencia=valor,referencia_plot=valor_plot)
comparacao_fixa <- comparacao_fixa |> left_join(ref,by=c('cenario','mes','variavel')) |>
  mutate(efeito=if_else(variavel %in% c('ust2y','juro_curto','inclinacao'),100*(valor-referencia),
                       100*(valor/referencia-1)),
         efeito_anual_pp=if_else(variavel %in% c('atividade','cpi_core'),valor_plot-referencia_plot,NA_real_))

# Controle e exatamente o forecast salvo, sem recalcular a referencia do BVAR.
controle <- comparacao_fixa |> filter(caminho=='Original salvo') |>
  inner_join(config$resumo |> transmute(cenario,mes=date,variavel,media),
             by=c('cenario','mes','variavel'))
stopifnot(max(abs(controle$valor-controle$media))<1e-8,
          identical(hash_antes,tools::md5sum(c(arquivo_modelo,arquivo_trajetorias))))
write_csv(comparacao_fixa |> mutate(mes=as.Date(mes)),file.path(saida_fixa,'comparacao.csv'))
write_csv(filter(comparacao_fixa,mes==max(mes)) |> mutate(mes=as.Date(mes)),
          file.path(saida_fixa,'impactos finais.csv'))
write_csv(bind_rows(diagnosticos),file.path(saida_fixa,'diagnosticos.csv'))
saveRDS(list(comparacao=comparacao_fixa,efeito_fluxo=efeito_fluxo,
             beta_fluxo=beta_fluxo,media_fluxo=media_fluxo,hash_modelo=hash_antes),
        file.path(saida_fixa,'forecast somente fluxo.rds'))
print(comparacao_fixa |> filter(cenario=='Base',mes==max(mes),
                               variavel %in% c('inclinacao','juro_curto','cpi_core','usdjpy')),width=Inf)
