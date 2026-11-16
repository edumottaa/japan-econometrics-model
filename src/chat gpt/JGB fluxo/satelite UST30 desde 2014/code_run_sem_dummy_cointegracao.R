setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/satelite UST30 desde 2014/00_configuracao.R',encoding='UTF-8')
source(file.path(pasta_testes,'03_sem_dummy_cointegracao.R'),encoding='UTF-8')
resultados_sem_dummy <- executar_sem_dummy_cointegracao()
# Apenas regressao satelite e testes. Nao executa forecast BVAR nem CV.
