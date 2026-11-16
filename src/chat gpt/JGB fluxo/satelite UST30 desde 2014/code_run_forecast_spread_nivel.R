setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/satelite UST30 desde 2014/00_configuracao.R',encoding='UTF-8')
source(file.path(pasta_testes,'05_forecast_spread_nivel.R'),encoding='UTF-8')
forecast_spread_nivel <- executar_forecast_spread_nivel()
# Apenas satelite salvo; nao executa BVAR ou CV.
