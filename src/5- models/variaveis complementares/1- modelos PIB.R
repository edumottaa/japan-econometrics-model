# PIB em função da atividade, com erros ARIMA.

library(fpp3)
library(tidyverse)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
dados_pib <- readRDS("data/processed/variaveis complementares/dados_pib.rds")

# 1. As duas séries já estão em crescimento trimestral: d = D = 0
modelo_pib <- dados_pib |>
  model(arima = ARIMA(pib_crescimento ~ atividade_crescimento + pdq(d = 0) + PDQ(0, 0, 0)))
modelo_pib |> report()

# Armazenamento da saída em outputs
saida_modelos_pib <- "src/Outputs/6- Projeções complementares/PIB/modelos"
dir.create(saida_modelos_pib, recursive = TRUE, showWarnings = FALSE)
saveRDS(modelo_pib, file.path(saida_modelos_pib, "modelos_pib.rds"))
write_csv(tidy(modelo_pib), file.path(saida_modelos_pib, "coeficientes.csv"))
write_csv(glance(modelo_pib), file.path(saida_modelos_pib, "ajuste.csv"))
