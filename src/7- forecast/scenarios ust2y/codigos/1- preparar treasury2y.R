# Caminho mensal do Treasury de 2 anos.

library(fpp3)
library(tidyverse)
library(readxl)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")

# 1. Curva Bloomberg de 22/10/2026; taxa zero usada como proxy da DGS2.
data_curva <- as.Date("2026-10-22")
treasury_bloomberg <- read_excel(
  "data/Treasury2Y  market expectations.xlsx", sheet = "Treasury 2Y",
  range = "A7:B32", col_types = c("numeric", "numeric")
) |>
  transmute(horizonte = as.integer(`Horizonte (meses)`),
            ust2y = `Taxa zero de 2 anos (% a.a.)`)

stopifnot(identical(treasury_bloomberg$horizonte, 0:24),
          all(is.finite(treasury_bloomberg$ust2y)))

# 2. Horizonte zero e spot; manter somente os meses futuros, em % a.a.
caminho_ust2y <- treasury_bloomberg |>
  filter(horizonte > 0) |>
  transmute(date = yearmonth(data_curva) + horizonte, ust2y) |>
  as_tsibble(index = date)
