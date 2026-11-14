Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse));suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/choque unico Treasury'
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
antigos <- readRDS(file.path(raiz,'src/Outputs/Forecast BVAR estrutural/pesos exponenciais/forecast_bvar.rds'))$caminhos |>
  as_tibble() |> arrange(cenario,date)
linhas <- list()
for(tipo in c('original','producao')) {
  pasta <- file.path(trabalho,tipo)
  novos <- readRDS(file.path(pasta,'caminhos_novos.rds')) |> arrange(cenario,date)
  stopifnot(identical(novos$brent,antigos$brent),identical(novos$date,antigos$date),
    identical(novos$cenario,antigos$cenario),identical(is.na(novos$ust2y),is.na(antigos$ust2y)))
  comuns <- novos$cenario!='Adverso estrutural'
  stopifnot(identical(novos$ust2y[comuns],antigos$ust2y[comuns]))
  diag <- read_csv(file.path(pasta,'diagnostico_choques.csv'),show_col_types=FALSE)
  stopifnot(sum(diag$meses_com_novo_choque)==1L,max(diag$meses_com_novo_choque)==1L)
  for(cen in unique(novos$cenario)) {
    a <- readRDS(file.path(pasta,paste0(cen,'.rds')))
    c <- novos |> filter(cenario==cen)
    stopifnot(all(is.finite(a$amostras)),all(is.finite(a$medias)),dim(a$medias)[3]==500L)
    stopifnot(identical(as.numeric(a$condicoes[,'brent']),as.numeric(c$brent)))
    if(tipo=='producao') {
      ip <- readRDS(file.path(pasta,paste0('producao ARIMA ',cen,'.rds')))
      stopifnot(max(abs(c$ip_mundial-ip$.mean))<1e-10)
    }
  }
  r <- readRDS(file.path(pasta,'resumo.rds'))
  stopifnot(nrow(r)==4L*5L*24L,all(is.finite(r$media)),
    all(r$inferior95<=r$inferior80),all(r$inferior80<=r$superior80),all(r$superior80<=r$superior95))
  g <- readRDS(file.path(pasta,'PIB/forecast_pib.rds'))
  stopifnot(max(g$resumo$date)==yearquarter('2028 Q2'),all(is.finite(g$resumo$media)),
    all(g$simulacoes$pib>0))
  linhas[[tipo]] <- r |> filter(date==max(date)) |>
    select(modelo,cenario,variavel,media) |> bind_rows(g$resumo |>
      filter(date==max(date),variavel=='crescimento_anual') |>
      transmute(modelo=tipo,cenario,variavel='PIB_YoY_2028T2',media))
  if(tipo=='original') {
    previa <- readRDS('C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/cenarios por modelo/original/resumo.rds')
    a <- r |> filter(cenario=='Base') |> arrange(variavel,date)
    b <- previa |> filter(cenario=='Base') |> arrange(variavel,date)
    stopifnot(max(abs(a$media-b$media))<1e-10)
  }
  cat('OK',tipo,': Brent preservado; Treasury comum identico; um unico choque; 500 sorteios; PIB ate 2028T2.\n')
}
write_csv(bind_rows(linhas),file.path(trabalho,'resultados finais.csv'))
cat('VERIFICACAO_CONCLUIDA\n')
