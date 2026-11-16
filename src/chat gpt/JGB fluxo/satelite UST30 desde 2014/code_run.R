setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/satelite UST30 desde 2014/00_configuracao.R',encoding='UTF-8')

# 1. Satelite: 30 configuracoes, significancia, AICc e residuos.
# Intercepto nao precisa ser significativo. AR/MA nao recebem a regra de 10%.
triagem_ust <- testar_satelites_ust(ler_insumos(),exportar=TRUE)
if(is.null(triagem_ust$escolhido)) stop('Nenhum satelite aprovado. Ver resultados/triagem.')

# 2. Forecast dos quatro cenarios: mesmo BVAR salvo + novo satelite suave.
forecast_ust <- executar_forecast_ust()

# 3. Somente depois, CV de 12 meses, cinco domesticas, Naive e Drift.
cv_ust <- executar_cv_ust()
