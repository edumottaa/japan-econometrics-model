# Construcao das variaveis do satelite

library(tidyverse)
library(tsibble)

# Variacoes em pontos-base; OIS apenas pos-YCC; fluxo defasado dois meses.
preparar_variaveis_satelite <- function(historico, aux, origem) {
  historico |>
    select(date, inclinacao) |>
    left_join(select(aux, date, ois2, fluxo), by = "date") |>
    arrange(date) |>
    mutate(
      d_spread = 100 * (inclinacao - lag(inclinacao)),
      ois_pos = 100 * (ois2 - lag(ois2)) * as.integer(date >= yearmonth(POS_YCC)),
      fluxo_l2 = lag(fluxo, 2)
    ) |>
    filter(date <= origem, if_all(c(d_spread, ois_pos, fluxo_l2), is.finite))
}
