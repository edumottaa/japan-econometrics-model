# Somente cenarios, forecast e graficos; reutilizar o modelo principal ja estimado.
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
pasta_forecast <- "src/chat gpt/jgb30y_5y ancora juros"
source(file.path(pasta_forecast, "1- cenario oferta.R"), encoding = "UTF-8")
source(file.path(pasta_forecast, "2- forecast.R"), encoding = "UTF-8")
source(file.path(pasta_forecast, "3- graficos.R"), encoding = "UTF-8")
