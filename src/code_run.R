# Modelo atual: produção mundial, tendência, pesos 0,995 e spread JGB30Y−5Y.
# Executar as etapas na mesma sessão. Os testes demorados ficam opcionais.
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
rodar_coleta <- TRUE
rodar_estimacao <- TRUE
rodar_curva_jgb <- TRUE

# 1. Coleta, incluindo JGB30Y e exportações do MoF antes feitas pelo script 4.
if (rodar_coleta) {
  source("src/1- data_collection/1- data_collection bvar estrutural.R", encoding = "UTF-8")
}

# 2. Estimação e IRFs do modelo atual.
# O modelo ainda lê a base congelada de jan/2000 a ago/2026 declarada nele.
# A coleta acima atualiza as fontes; não estende automaticamente essa base.
if (rodar_estimacao) {
  source("src/5- models/Structural BVAR models/Bvar estrutural producao mundial JGB30Y-5Y.R",
         encoding = "UTF-8")
}

# 3. Caminhos de referencia; depois, satelite de fluxo com tilting entropico.
pasta_forecast <- "src/7- forecast/bvar estrutural/jgb30y_5y com tendencia"
source(file.path(pasta_forecast, "1- cenario oferta.R"), encoding = "UTF-8")
source(file.path(pasta_forecast, "2- forecast.R"), encoding = "UTF-8")
source("src/7- forecast/Satelite fluxo/0- rodar.R", encoding = "UTF-8")

# 4. JGB2Y e spreads primeiro; depois reconstruir JGB5Y, JGB10Y e JGB30Y.
if (rodar_curva_jgb) {
  fonte_bvar <- "src/Outputs/Satelite fluxo suave"
  source("src/JGB30Y/projecoes/rodar.R", encoding = "UTF-8")
}

