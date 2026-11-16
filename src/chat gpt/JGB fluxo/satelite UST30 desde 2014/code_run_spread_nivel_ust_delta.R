setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/satelite UST30 desde 2014/00_configuracao.R',encoding='UTF-8')
source(file.path(pasta_testes,'04_spread_nivel_ust_diferenciado.R'),encoding='UTF-8')
teste_spread_nivel <- executar_spread_nivel_ust_delta()
# Somente ajustes e diagnosticos; nenhum forecast BVAR ou CV.
