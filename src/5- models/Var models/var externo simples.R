library(tidyverse)
library(ggplot2)
library(dplyr)
library(fpp3)





# 4. VAR de referência e seleção de defasagens
var_ex <- data_bal |>
  model(
    aicc = VAR(vars(ip_mundial, brent, vix, ust2y)),
    bic = VAR(vars(ip_mundial, brent, vix, ust2y), ic = "bic")
  )
var_ex 

var_ex |>
  select(aicc) |>
  forecast() |>
  autoplot(data_bal |> filter(year(date) > 2000))




