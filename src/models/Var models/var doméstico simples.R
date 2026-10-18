library(tidyverse)
library(ggplot2)
library(dplyr)
library(fpp3)




# VAR de referência e seleção de defasagens
#var ds: var doméstico simples
var_ds <- data_bal |>
  model(
    aicc = VAR(vars(atividade,cpi_core, juro_curto)),
    bic = VAR(vars(atividade,cpi_core, juro_curto), ic = "bic")
  )
var_ds 

var_ds |>
  select(aicc) |>
  forecast() |>
  autoplot(data_bal |> filter(year(date) > 2000))




