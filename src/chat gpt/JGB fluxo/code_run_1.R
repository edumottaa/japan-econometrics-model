# Modelo 1: estimar o satelite e condicionar o BVAR atual ao seu spread.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/00_dados.R',encoding='UTF-8')
source(file.path(pasta,'01_modelo_satelite.R'),encoding='UTF-8')
comparar_bvar_fluxo <- FALSE
source(file.path(pasta,'03_forecasts_comparacao.R'),encoding='UTF-8')
