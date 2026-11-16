# 4. Mesmo modelo do teste 3, condicionado ao fluxo estrangeiro REALIZADO.
# Futuro: fixa apenas meses observados. CV: fixa os 12 meses efetivamente realizados.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco testes/00_configuracao.R',encoding='UTF-8')
source(file.path(pasta_testes,'05_executar.R'),encoding='UTF-8')
RODAR_FORECAST <- TRUE
RODAR_CV <- FALSE # Troque para TRUE depois de conferir o forecast.
if(RODAR_FORECAST) executar_teste(4L)
if(RODAR_CV) {
  source(file.path(pasta_testes,'07_cv.R'),encoding='UTF-8')
  executar_cv_teste(4L)
}
