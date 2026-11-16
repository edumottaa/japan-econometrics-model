# SOMENTE FORECAST: reutiliza o BVAR original e os modelos satelites ja estimados.
# Nao chama estimate(), model() ou o amostrador do BVAR-X.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/00_dados.R',encoding='UTF-8')
source(file.path(pasta,'04_forecast_coeficientes_fixos.R'),encoding='UTF-8')
source(file.path(pasta,'05_graficos_coeficientes_fixos.R'),encoding='UTF-8')
