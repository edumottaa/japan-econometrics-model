# Apenas configuracao e funcoes. Nao estima nem roda o CV ao carregar.
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
pasta_pib <- file.path(raiz,'src/chat gpt/PIB atividade BVAR ex ante')
pasta_bvar <- file.path(raiz,'src/chat gpt/JGB fluxo/cinco opcoes UST e fluxo')
saida_pib <- file.path(pasta_pib,'resultados')
funcoes_ponte <- file.path(raiz,'src/5- models/PIB (reg de atividade com erros em ARIMA)/0- funcoes ponte PIB.R')
dados_bvar_pib <- file.path(raiz,'data/processed/bvar_producao_tendencia_jgb30_5_20261112/dados.rds')
arquivo_pib <- file.path(raiz,'data/cache/cache_pib.rds')
REESTIMAR_PIB <- FALSE
MOSTRAR_GRAFICOS_PIB <- TRUE

iniciar_pib_cv <- function() {
  .libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
  if(.Platform$OS.type=='windows' && Sys.getlocale('LC_CTYPE')=='C')
    suppressWarnings(try(Sys.setlocale('LC_CTYPE','English_United States.utf8'),silent=TRUE))
  suppressPackageStartupMessages({library(fpp3); library(tidyverse); library(distributional)})
  source(funcoes_ponte,encoding='UTF-8')
  # Carregar somente DEFINICOES para conferir os caches atuais do BVAR.
  source(file.path(pasta_bvar,'00_configuracao.R'),encoding='UTF-8')
  source(file.path(pasta_bvar,'cv_igual_bvar_original.R'),encoding='UTF-8')
  for(a in c('01_cv_pib.R','02_graficos.R'))
    source(file.path(pasta_pib,a),encoding='UTF-8')
  invisible(TRUE)
}
