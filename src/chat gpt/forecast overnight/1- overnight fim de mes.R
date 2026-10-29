# Overnight efetiva de fim de mes, em % a.a. (BoJ/FM02).
library(tidyverse)
library(tsibble)
library(jsonlite)

boj_overnight <- fromJSON(paste0(
  "https://www.stat-search.boj.or.jp/api/v1/getDataCode?format=json&lang=en",
  "&db=FM02&code=STRECLUCON&startDate=200001"))
overnight_fim_mes <- tibble(
  date = yearmonth(ym(boj_overnight$RESULTSET$VALUES$SURVEY_DATES[[1]])),
  overnight_fim_mes = as.numeric(boj_overnight$RESULTSET$VALUES$VALUES[[1]])) |>
  filter(date < yearmonth(Sys.Date())) |> arrange(date) |> as_tsibble(index = date)
stopifnot(nrow(overnight_fim_mes) > 0L, !any(has_gaps(overnight_fim_mes)$.gaps),
          all(is.finite(overnight_fim_mes$overnight_fim_mes)))
call_rate <- overnight_fim_mes |>
  as_tibble() |> transmute(date = as.Date(date), call_rate = overnight_fim_mes)
