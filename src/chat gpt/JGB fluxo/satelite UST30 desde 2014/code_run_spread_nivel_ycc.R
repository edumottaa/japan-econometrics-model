setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/satelite UST30 desde 2014/00_configuracao.R',encoding='UTF-8')
source(file.path(pasta_testes,'06_spread_nivel_dummy_ycc.R'),encoding='UTF-8')
teste_ycc <- executar_spread_nivel_ycc()
# Apenas satelite e seu forecast. Nenhum BVAR/CV e executado.
