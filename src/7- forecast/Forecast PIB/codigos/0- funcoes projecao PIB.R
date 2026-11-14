# Agregar a atividade mensal do BVAR para usar como preditora do PIB trimestral.

library(fpp3)
library(tidyverse)
library(distributional)

preparar_atividade_pib <- function(historico, previsoes, ultimo_pib, log_bvar = FALSE) {
  # O CV guarda 100 * log(atividade); o forecast final já guarda o índice em nível.
  futuro <- previsoes |> as_tibble() |> filter(variavel == "atividade") |> arrange(date)
  futuro <- futuro |> transmute(date, atividade = if (log_bvar)
    map_dbl(parameters(valor)$x, ~ mean(exp(.x / 100))) else mean(valor))

  bind_rows(historico |> as_tibble() |> select(date, atividade) |>
      filter(date < min(futuro$date)), futuro) |>
    arrange(date) |>
    as_tsibble(index = date) |>
    index_by(trimestre = yearquarter(date)) |>
    summarise(meses = n(), atividade = mean(atividade)) |>
    rename(date = trimestre) |>
    filter(meses == 3L) |>
    mutate(atividade_crescimento = 100 * (atividade / lag(atividade) - 1)) |>
    filter(date > ultimo_pib) |>
    select(date, atividade_crescimento)
}
