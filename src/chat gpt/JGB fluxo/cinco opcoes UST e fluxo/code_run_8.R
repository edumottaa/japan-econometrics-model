# Executor individual: forecast desta opcao, depois CV de 12 meses.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco opcoes UST e fluxo/00_configuracao.R',encoding='UTF-8')
iniciar_testes()
source(file.path(pasta_testes,'cv_igual_bvar_original.R'),encoding='UTF-8')
rodar_lote(testes=8L,rodar_forecast=TRUE,rodar_cv=TRUE)
