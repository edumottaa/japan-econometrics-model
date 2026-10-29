# Base trimestral para projetar o PIB a partir da atividade do BVAR.

library(fpp3)
library(tidyverse)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")

# 1. PIB real com ajuste sazonal e o mesmo índice coincidente do BVAR.
pib <- readRDS("data/cache/cache_pib.rds")
dados_mensais_pib <- readRDS("data/cache/cache_atividade.rds") |>
  mutate(date = yearmonth(date)) |>
  arrange(date) |>
  as_tsibble(index = date)

# 2. Média trimestral do índice em nível; usar os três meses completos.
atividade_trimestral <- dados_mensais_pib |>
  index_by(trimestre = yearquarter(date)) |>
  summarise(meses = n(), atividade = mean(atividade)) |>
  rename(date = trimestre) |>
  filter(meses == 3L) |>
  mutate(atividade_crescimento = 100 * (atividade / lag(atividade) - 1))

# 3. Crescimento trimestral em porcentagem, sem anualização.
dados_pib <- pib |>
  mutate(pib_crescimento = 100 * (pib / lag(pib) - 1)) |>
  left_join(atividade_trimestral |> select(-meses), by = "date") |>
  filter(date >= yearquarter("2000 Q1")) |>
  drop_na(pib_crescimento, atividade_crescimento)
stopifnot(!any(has_gaps(dados_pib)$.gaps),
  all(is.finite(as.matrix(as_tibble(dados_pib) |> select(-date)))))

# 4. Guardar as bases intermediárias em data.
dir.create("data/processed/variaveis complementares", recursive = TRUE, showWarnings = FALSE)
saveRDS(dados_pib, "data/processed/variaveis complementares/dados_pib.rds")
saveRDS(dados_mensais_pib, "data/processed/variaveis complementares/dados_mensais_pib.rds")
dados_pib
