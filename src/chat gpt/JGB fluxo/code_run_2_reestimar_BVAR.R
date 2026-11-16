# EXPERIMENTO ANTERIOR: REESTIMA o BVAR-X. Para so forecast, usar code_run_2.R.
# Roda o satelite para obter o fluxo e compara os dois caminhos no BVAR.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/00_dados.R',encoding='UTF-8')
source(file.path(pasta,'01_modelo_satelite.R'),encoding='UTF-8')
S <- 500L   # Aumentar para 2500 para maior precisao; ESS minimo 50 exigido.
cores <- 4L
permitir_piloto <- FALSE
source(file.path(pasta,'02_modelo_bvar_fluxo.R'),encoding='UTF-8')
comparar_bvar_fluxo <- TRUE
source(file.path(pasta,'03_forecasts_comparacao.R'),encoding='UTF-8')
