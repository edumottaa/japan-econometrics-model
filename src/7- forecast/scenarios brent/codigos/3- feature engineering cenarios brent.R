# Feature engineering dos cenarios construidos no script 1.

library(fpp3)
library(tidyverse)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
saida <- "src/Outputs/cenarios_brent"
dir.create(saida, recursive = TRUE, showWarnings = FALSE)

# 1. Construir os cenarios em dolares.
source("src/7- forecast/scenarios brent/codigos/1- construir cenarios brent.R", local = TRUE, encoding = "UTF-8")
stopifnot(all(is.finite(cenarios_brent$preco)), all(cenarios_brent$preco > 0))

# 2. Aplicar a transformacao do BVAR.
cenarios_brent_modelo <- cenarios_brent |>
  transmute(date, cenario, brent = 100 * log(preco))

# 3. Salvar na escala do modelo.
saveRDS(cenarios_brent_modelo, file.path(saida, "cenarios_brent_modelo.rds"))
write_csv(as_tibble(cenarios_brent_modelo) |> mutate(date = as.Date(date)),
          file.path(saida, "cenarios_brent_modelo.csv"))
