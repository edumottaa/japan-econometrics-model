# TESTE BVAR com fluxo estrangeiro so no spread 30-5. Mesma sequencia do forecast de producao.
# Antes: "0- trajetoria do fluxo estrangeiro.R" (entradas) e "1- bvar estrutural com fluxo.R" (estimacao).
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
pasta_forecast <- "src/chat gpt/bvar fluxo estrangeiro 30-5"
source(file.path(pasta_forecast, "2- cenario oferta.R"), encoding = "UTF-8")
source(file.path(pasta_forecast, "3- forecast.R"), encoding = "UTF-8")
source(file.path(pasta_forecast, "4- graficos.R"), encoding = "UTF-8")
source(file.path(pasta_forecast, "5- comparacao com BVAR atual.R"), encoding = "UTF-8")
