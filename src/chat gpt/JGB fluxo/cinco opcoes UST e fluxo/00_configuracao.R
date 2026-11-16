# Nova rodada: apenas definicoes. Nenhum modelo e executado por este arquivo.
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
pasta_testes <- file.path(raiz,'src/chat gpt/JGB fluxo/cinco opcoes UST e fluxo')
fonte_modelo <- file.path(raiz,'data/processed/bvar_producao_tendencia_jgb30_5_20261112')
fonte_forecast <- file.path(raiz,'src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao')
fonte_dados <- file.path(raiz,'src/chat gpt/spreads JGB modelo satelite/dados')
ARQUIVO_CURVAS <- file.path(fonte_dados,'curvas_bloomberg.csv')

P <- 6L
RHO <- .995
S <- 2500L
HIPER_S <- 5000L
HIPER_BURN <- 2500L
CORES <- 4L
SORTEIOS_FORECAST <- 250L
ESS_MINIMO <- 50
SEMENTE <- 123L
REESTIMAR <- FALSE
BANDA_JURO <- c(1.75,2.25) # Apenas no forecast prospectivo: ultimo mes.
SATELITE_VARIANCIA_MULT <- 1
POS_YCC <- '2024 Apr'
CV_INICIO <- '2024 Oct'
CV_HORIZONTE <- 12L
CV_PASSO <- 1L
CV_S <- 500L
CV_HIPER_S <- 1000L
CV_HIPER_BURN <- 500L
CV_SORTEIOS_FORECAST <- 100L
MOSTRAR_GRAFICOS <- TRUE

variaveis <- c('ust2y','brent','ip_mundial','atividade','cpi_core',
              'juro_curto','inclinacao','usdjpy')
nomes_testes <- c('1: UST30 exogeno','2: UST30 - UST2Y exogeno',
  '3: Satelite suave','4: Satelite rigido','5: Fluxo domestico',
  '6: Fluxo exogeno','7: Fluxo + UST30 exogenos',
  '8: Fluxo + UST30 - UST2Y exogenos')

iniciar_testes <- function() {
  .libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
  suppressPackageStartupMessages({library(fpp3); library(tidyverse); library(bsvarSIGNs)})
  if(as.character(packageVersion('bsvarSIGNs'))!='3.0')
    stop('O amostrador local foi preparado para bsvarSIGNs 3.0. Rever antes de usar outra versao.')
  stopifnot(P==6L,identical(BANDA_JURO,c(1.75,2.25)),CV_HORIZONTE==12L)
  for(a in c('01_dados.R','02_amostragem.R','03_modelos.R','04_forecast.R',
             '05_executar.R','06_comparar.R','07_cv.R','08_satelite_suave.R'))
    source(file.path(pasta_testes,a),local=.GlobalEnv,encoding='UTF-8')
  dir.create(file.path(pasta_testes,'resultados'),recursive=TRUE,showWarnings=FALSE)
  invisible(TRUE)
}

chave_execucao <- function() {
  scripts <- list.files(pasta_testes,pattern='^0[0-9]_.*\\.R$',full.names=TRUE)
  fontes <- c(scripts,file.path(fonte_modelo,c('dados.rds','modelo.rds')),
    file.path(fonte_forecast,c('caminhos_novos.rds','forecast_bvar.rds','trajetorias modelos.rds')),
    file.path(fonte_dados,c('DGS30_2026-11-04.csv','ois2_historico.csv',
      'fluxos_jsda_superlongos.csv')),ARQUIVO_CURVAS)
  if(!all(file.exists(fontes))) stop('Insumo ausente: ',paste(fontes[!file.exists(fontes)],collapse='; '))
  list(hash=tools::md5sum(fontes),P=P,RHO=RHO,S=S,HIPER_S=HIPER_S,
    HIPER_BURN=HIPER_BURN,SORTEIOS_FORECAST=SORTEIOS_FORECAST,
    CV_S=CV_S,CV_HIPER_S=CV_HIPER_S,CV_HIPER_BURN=CV_HIPER_BURN,
    CV_SORTEIOS_FORECAST=CV_SORTEIOS_FORECAST,ESS_MINIMO=ESS_MINIMO,
    SEMENTE=SEMENTE,BANDA_JURO=BANDA_JURO,POS_YCC=POS_YCC,
    CV_INICIO=CV_INICIO,CV_HORIZONTE=CV_HORIZONTE,CV_PASSO=CV_PASSO,
    SATELITE_VARIANCIA_MULT=SATELITE_VARIANCIA_MULT,
    pacote=as.character(packageVersion('bsvarSIGNs')))
}
