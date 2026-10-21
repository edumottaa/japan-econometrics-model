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



# 2. Preparação dos dados para o S2

# 2.1. Séries mensais
data_s2_mensal <- cpi_s2_mic |>
  filter(base == "2025") |>
  transmute(date, cpi_core = c0161_1, pi_core = c0161_3,
            cpi_servicos = c0220_1, pi_servicos = c0220_3) |>
  full_join(select(cpi_boj, date, pi_core_sem_inst, pi_core_core_sem_inst), by = "date") |>
  full_join(fatores_institucionais, by = "date") |>
  full_join(precos_importacao, by = "date") |>
  full_join(expectativa_cabinet, by = "date") |>
  mutate(date = yearmonth(date)) |>
  filter(date < yearmonth(Sys.Date())) |>
  arrange(date) |>
  as_tsibble(index = date) |>
  fill_gaps()

#Tankan e hiato; último resultado divulgado

tankan_mensal <- expectativa_inflacao |>
  transmute(date = yearmonth(as.Date(date)) + if_else(lubridate::quarter(as.Date(date)) == 4L, 2L, 3L),
            expectativa_1ano_tankan = expectativa_1ano)

hiato_mensal <- hiato_produto |>
  transmute(date = yearmonth(as.Date(date)) + 6L, hiato)




# 2.2. Séries trimestrais: média dos três meses completos para cada série mensal
# Tankan e hiato entram pelo trimestre de referência
data_s2_trimestral <- data_s2_mensal |>
  as_tibble() |>
  mutate(date = yearquarter(date)) |>
  group_by(date) |>
  summarise(across(everything(), ~ if (n() == 3L && !anyNA(.x)) mean(.x) else NA_real_),
            .groups = "drop") |>
  full_join(rename(expectativa_inflacao, expectativa_1ano_tankan = expectativa_1ano), by = "date") |>
  full_join(hiato_produto, by = "date") |>
  arrange(date) |>
  as_tsibble(index = date) |>
  fill_gaps()


data_s2_mensal <- data_s2_mensal |>
  as_tibble() |>
  left_join(tankan_mensal, by = "date") |>
  left_join(hiato_mensal, by = "date") |>
  arrange(date) |>
  fill(expectativa_1ano_tankan, hiato, .direction = "down") |>
  as_tsibble(index = date)

