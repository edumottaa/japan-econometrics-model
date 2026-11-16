# Executor do modelo selecionado, organizado nas sete etapas do src.
# Reaproveita resultados compativeis. Nao reestima o BVAR estrutural.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
rodar_forecast_satelite <- TRUE
rodar_cv_satelite <- FALSE
reaproveitar_salvos <- TRUE
source('src/1- data_collection/Satelite fluxo/0- configuracao.R',encoding='UTF-8')
source("src/5- models/Satelite fluxo/0- configuracao.R", encoding = "UTF-8")
carregar_satelite()
if(reaproveitar_salvos && !REESTIMAR) reaproveitar_satelite_salvo()
if(rodar_forecast_satelite) {
  resultado_satelite <- executar_forecast_satelite()
  graficos_forecast_satelite(resultado_satelite)
}
if(rodar_cv_satelite) {
  executar_cv(3L,chave_execucao())
  graficos_cv_satelite()
}
