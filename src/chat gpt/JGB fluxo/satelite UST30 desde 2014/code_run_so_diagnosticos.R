setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/satelite UST30 desde 2014/00_configuracao.R',encoding='UTF-8')
triagem_ust <- testar_satelites_ust(ler_insumos(),exportar=TRUE)
# Nao executa forecast BVAR nem CV.
