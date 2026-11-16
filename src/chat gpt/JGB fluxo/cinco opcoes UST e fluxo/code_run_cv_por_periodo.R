# Somente o CV pelo procedimento original e comparacao por periodo.
# Nao reexecuta os forecasts prospectivos.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco opcoes UST e fluxo/00_configuracao.R',encoding='UTF-8')
iniciar_testes()
source(file.path(pasta_testes,'cv_igual_bvar_original.R'),encoding='UTF-8')
REESTIMAR <- FALSE
rodar_lote(testes=1:4,rodar_forecast=FALSE,rodar_cv=TRUE)
rodar_lote(testes=5L,rodar_forecast=FALSE,rodar_cv=TRUE)
rodar_lote(testes=6:8,rodar_forecast=FALSE,rodar_cv=TRUE)
