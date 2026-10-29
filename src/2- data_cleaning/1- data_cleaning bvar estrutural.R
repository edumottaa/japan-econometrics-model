# Preparação dos dados

library(tidyverse)
library(tsibble)

#1 Renomear as séries, reunir as fontes e criar a série temporal
data <- fred |>
  rename(
    ust2y  = DGS2,
    brent  = DCOILBRENTEU,
    vix    = VIXCLS,
    usdjpy = DEXJPUS,
    topix  = SPASTT01JPM661N
  ) |>
  left_join(ip_mundial, by = "date") |>
  left_join(atividade,  by = "date") |>
  left_join(cpi,        by = "date") |>
  left_join(select(juro_curto, date, juro_curto), by = "date") |>
  left_join(jgb,        by = "date") |>
  mutate(date = yearmonth(date)) |>
  filter(date < yearmonth(Sys.Date())) |>   # Excluir o mês corrente, ainda incompleto
  as_tsibble(index = date)



