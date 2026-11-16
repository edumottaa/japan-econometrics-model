# Somente opcoes novas: forecasts 6-8 primeiro; CVs 6-8 depois.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco opcoes UST e fluxo/00_configuracao.R',encoding='UTF-8')
iniciar_testes()
source(file.path(pasta_testes,'cv_igual_bvar_original.R'),encoding='UTF-8')
rodar_lote(testes=6:8,rodar_forecast=TRUE,rodar_cv=TRUE)
