# 1. Leitura dos dados do satelite

library(tidyverse)
library(fpp3)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")

# 1.1. OIS japones de dois anos
ois2 <- read_csv("data/raw/satelite_fluxo/ois2_historico.csv",
                 show_col_types = FALSE) |>
  transmute(date = as.Date(date), ois2 = ois2_pct)

# 1.2. Compras liquidas estrangeiras de titulos superlongos
fluxo_estrangeiro <- read_csv("data/raw/satelite_fluxo/fluxos_jsda_superlongos.csv",
                              show_col_types = FALSE) |>
  filter(investidor == "Foreigners") |>
  transmute(date = yearmonth(mes), fluxo = liquido_tri)

# 1.3. Treasury americano de trinta anos
ust30 <- read_csv("data/raw/satelite_fluxo/DGS30_2026-11-04.csv",
                  na = c("", "."), show_col_types = FALSE) |>
  transmute(date = as.Date(observation_date), ust30 = DGS30)

# 1.4. Curvas a termo extraidas da Bloomberg
curvas <- read_csv("data/raw/satelite_fluxo/curvas_bloomberg.csv",
                   show_col_types = FALSE)

# 1.5. Base historica do BVAR
dados_bvar <- readRDS("data/processed/bvar_producao_tendencia_jgb30_5_20261112/dados.rds")
