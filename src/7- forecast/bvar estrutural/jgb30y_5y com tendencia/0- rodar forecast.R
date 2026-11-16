# Somente cenarios, forecast e graficos; reutilizar o modelo principal ja estimado.
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
pasta_forecast <- "src/7- forecast/bvar estrutural/jgb30y_5y com tendencia"
source(file.path(pasta_forecast, "1- cenario oferta.R"), encoding = "UTF-8")
source(file.path(pasta_forecast, "2- forecast.R"), encoding = "UTF-8")
source("src/7- forecast/Satelite fluxo/0- rodar.R", encoding = "UTF-8")
