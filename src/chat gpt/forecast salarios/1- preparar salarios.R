# Reutilizar a coleta; atualizar executando o script de salarios.
if (!file.exists("data/cache/cache_salarios.rds"))
  source("src/1- data_collection/2- salarios.R", encoding = "UTF-8")
salarios_modelo <- readRDS("data/cache/cache_salarios.rds") |>
  mutate(date = yearmonth(date)) |>
  as_tsibble(index = date)
stopifnot(!any(has_gaps(salarios_modelo)$.gaps), all(is.finite(salarios_modelo$salario)))
