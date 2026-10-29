Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(TZ = "America/Sao_Paulo")
library(fpp3)
library(tidyverse)
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
pasta <- "src/Outputs/Forecast BVAR estrutural/overnight fim de mes"
ajuste <- readRDS("data/processed/forecast_overnight_fim_mes/modelo.rds")
resultado <- readRDS(file.path(pasta, "forecast_bvar.rds"))
taxa <- read_csv(file.path(pasta, "overnight_fim_mes.csv"), show_col_types = FALSE) |>
  mutate(date = yearmonth(date))
anterior <- readRDS("data/processed/comparacao_hiato_priors_20261024/base_forecast.rds") |>
  select(date, all_of(ajuste$variaveis))
dados <- ajuste$dados_modelo
novos <- dados |> filter(date >= yearmonth("2024 Mar")) |>
  left_join(taxa, by = "date")
stopifnot(ncol(dados) == 8L, ajuste$configuracao$p == 6L,
  identical(select(dados, -juro_curto), select(anterior, -juro_curto)),
  identical(filter(dados, date < yearmonth("2024 Mar")), filter(anterior, date < yearmonth("2024 Mar"))),
  all(novos$juro_curto == novos$overnight_fim_mes),
  nrow(resultado$resumo) == 7L * 24L * 4L,
  all(resultado$diagnosticos$erro_condicoes <= 1e-8),
  all(resultado$diagnosticos$finitos), all(resultado$diagnosticos$sorteios == 2500L),
  all(is.finite(as.matrix(select(resultado$resumo, media:superior_95)))),
  length(list.files(pasta, pattern = "[.]png$")) == 15L)
setembro <- yearmonth("2026 Sep")
stopifnot(all(filter(resultado$caminhos, date == setembro)$juro_curto ==
  filter(taxa, date == setembro)$overnight_fim_mes),
  all(is.na(filter(resultado$caminhos, date > setembro)$juro_curto)))
print(ajuste$diagnostico)
print(tail(taxa, 3), width = Inf)
print(resultado$caminhos |> filter(date == setembro) |> select(date, cenario, juro_curto))
print(read_csv(file.path(pasta, "resumo_unidades_grafico.csv"), show_col_types = FALSE) |>
  filter(cenario == "Base", variavel %in% c("cpi_core", "juro_curto"),
    date %in% as.Date(c("2026-12-01", "2027-12-01", "2028-08-01"))) |>
  select(variavel, date, media), n = Inf)
cat("VALIDADO: substituicao, amostra, condicoes, quatro cenarios e quinze graficos.\n")
