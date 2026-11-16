# 1. Estimar satelite separado; condicionar SUAVEMENTE o BVAR ORIGINAL JA SALVO.
# Nao reestima o BVAR no forecast. CV reestima por origem para nao vazar o futuro.
# Selecao de lags e ARIMA nos DOIS estagios pela mesma estrutura do IP mundial.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco testes/00_configuracao.R',encoding='UTF-8')
source(file.path(pasta_testes,'05_executar.R'),encoding='UTF-8')
RODAR_FORECAST <- TRUE
RODAR_CV <- FALSE # Troque para TRUE depois de conferir o forecast.
MOSTRAR_RESIDUOS <- TRUE # Grafico de residuos do fluxo e do spread selecionados.
if(RODAR_FORECAST) executar_teste(1L)
if(RODAR_CV) {
  source(file.path(pasta_testes,'07_cv.R'),encoding='UTF-8')
  executar_cv_teste(1L)
}
