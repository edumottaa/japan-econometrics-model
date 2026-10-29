# Limpeza das variáveis complementares: PIB real com ajuste sazonal.

library(fpp3)
library(tidyverse)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")

# 1. Ler o período e o PIB; o CSV só repete o ano no primeiro trimestre
pib_original <- read_csv("data/raw/variaveis complementares/pib_real_trimestral.csv",
  skip = 7, col_names = FALSE, col_types = cols(.default = col_character()),
  locale = locale(encoding = "CP932"), show_col_types = FALSE)

pib <- pib_original |>
  transmute(periodo = str_remove_all(X1, "\\s"),
    ano = as.integer(str_extract(periodo, "^[0-9]{4}")),
    trimestre = match(str_extract(periodo, "[0-9]+-[0-9]+"), c("1-3", "4-6", "7-9", "10-12")),
    pib = parse_number(X2, locale = locale(decimal_mark = ".", grouping_mark = ","))) |>
  fill(ano) |>
  filter(!is.na(ano), !is.na(trimestre), !is.na(pib)) |>
  transmute(date = yearquarter(make_date(ano, 3L * trimestre - 2L)), pib) |>
  arrange(date) |>
  as_tsibble(index = date)
stopifnot(nrow(pib) >= 80L, !anyDuplicated(pib$date),
  !any(has_gaps(pib)$.gaps), all(is.finite(pib$pib)), all(pib$pib > 0))

# 2. Manter o nível oficial
saveRDS(pib, "data/cache/cache_pib.rds")
pib
