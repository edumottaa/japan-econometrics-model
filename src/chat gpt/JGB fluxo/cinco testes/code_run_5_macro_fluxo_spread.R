# 5. Versao do print: macro -> fluxo -> spread; demais efeitos via defasagens do spread.
# Restricoes exatas nos lags do fluxo; covariancia contemporanea continua livre.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco testes/00_configuracao.R',encoding='UTF-8')
source(file.path(pasta_testes,'05_executar.R'),encoding='UTF-8')
RODAR_FORECAST <- TRUE
RODAR_CV <- FALSE # Troque para TRUE depois de conferir o forecast.
if(RODAR_FORECAST) executar_teste(5L)
if(RODAR_CV) {
  source(file.path(pasta_testes,'07_cv.R'),encoding='UTF-8')
  executar_cv_teste(5L)
}
