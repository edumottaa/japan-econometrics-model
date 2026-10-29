# Forecast de 12 lags com a mesma base, priors e cenarios; sem CV.
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
lags_forecast <- 12L
tryCatch(
  source("src/chat gpt/forecast overnight/3- forecast overnight fim de mes.R", encoding = "UTF-8"),
  finally = rm(lags_forecast))
