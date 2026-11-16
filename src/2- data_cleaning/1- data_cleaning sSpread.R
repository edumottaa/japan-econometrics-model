# Preparacao dos dados do satelite

library(tidyverse)
library(tsibble)

# 1. Calcular as medias mensais dos juros
ois_mensal <- ois2 |>
  filter(is.finite(ois2)) |>
  group_by(date = yearmonth(date)) |>
  summarise(ois2 = mean(ois2), .groups = "drop")

ust_mensal <- ust30 |>
  filter(is.finite(ust30)) |>
  group_by(date = yearmonth(date)) |>
  summarise(ust30 = mean(ust30), .groups = "drop")

# 2. Ordenar o fluxo estrangeiro e o historico do BVAR
fluxo_estrangeiro <- fluxo_estrangeiro |> arrange(date)
historico_satelite <- dados_bvar |>
  as_tibble() |> mutate(date = yearmonth(date)) |> arrange(date)

# 3. Reunir as series mensais, preservando as datas sem fluxo ou OIS
aux_satelite <- ust_mensal |>
  full_join(ois_mensal, by = "date") |>
  full_join(fluxo_estrangeiro, by = "date") |>
  arrange(date)

stopifnot(!anyDuplicated(aux_satelite$date),
          !anyDuplicated(historico_satelite$date))
