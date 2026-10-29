# Mesmo BVAR estrutural; substituir apenas a overnight media pela de fim de mes.
library(fpp3)
library(tidyverse)
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
somente_funcoes_forecast <- TRUE
source("src/7- forecast/comparacao hiato e priors/1- forecast bvars.R", encoding = "UTF-8")
somente_funcoes_forecast <- FALSE
somente_funcoes_graficos <- TRUE
source("src/7- forecast/comparacao hiato e priors/2- graficos forecast bvars.R", encoding = "UTF-8")
somente_funcoes_graficos <- FALSE
source("src/chat gpt/forecast overnight/1- overnight fim de mes.R", encoding = "UTF-8")

# 1. Manter a taxa-sombra ate fev/2024 e as demais series na mesma amostra.
if (!exists("lags_forecast")) lags_forecast <- 6L
stopifnot(lags_forecast %in% c(6L, 12L))
cfg <- tibble(formato = "overnight_fim_mes", lambda = .20, mu = 1, delta = 1,
              p = as.integer(lags_forecast), incluir_hiato = FALSE)
variaveis <- c(externas, alvos_base)
base_anterior <- readRDS("data/processed/comparacao_hiato_priors_20261024/base_forecast.rds") |>
  select(date, all_of(variaveis))
dados_boj <- base_anterior |>
  left_join(overnight_fim_mes, by = "date") |>
  mutate(juro_curto = if_else(date < yearmonth("2024 Mar"), juro_curto, overnight_fim_mes)) |>
  select(date, all_of(variaveis))
stopifnot(identical(select(dados_boj, -juro_curto), select(base_anterior, -juro_curto)),
          !any(has_gaps(dados_boj)$.gaps),
          all(is.finite(as.matrix(select(as_tibble(dados_boj), -date)))))

# 2. Reestimar com os mesmos priors, sinais e narrativas, sem CV.
sufixo_lags <- if (cfg$p == 6L) "" else paste0(" lag", cfg$p)
pasta_modelo <- paste0("data/processed/forecast_overnight_fim_mes", sufixo_lags)
saida <- paste0("src/Outputs/Forecast BVAR estrutural/overnight fim de mes", sufixo_lags)
dir.create(pasta_modelo, recursive = TRUE, showWarnings = FALSE)
dir.create(saida, recursive = TRUE, showWarnings = FALSE)
arquivo_modelo <- file.path(pasta_modelo, "modelo.rds")
reutilizar <- FALSE
if (file.exists(arquivo_modelo)) {
  ajuste <- readRDS(arquivo_modelo)
  reutilizar <- identical(ajuste$dados_modelo, dados_boj) && identical(ajuste$configuracao, cfg)
}
if (!reutilizar) {
  gravar_status(pasta_modelo, "estimando")
  ajuste <- estimar_bvar(dados_boj, cfg, estrutural = TRUE)
  ajuste$dados_modelo <- dados_boj
  ajuste$configuracao <- cfg
  saveRDS(ajuste, arquivo_modelo)
}
post_boj <- ajuste$post

# 3. Usar a mesma overnight de fim de mes nas observacoes apos a amostra.
preparar_caminhos_anterior <- preparar_caminhos
preparar_caminhos <- function(dados) {
  preparar_caminhos_anterior(dados) |>
    select(-juro_curto) |> left_join(overnight_fim_mes, by = "date") |>
    rename(juro_curto = overnight_fim_mes)
}
gravar_status(pasta_modelo, "forecast")
resultado <- forecast_ajuste(ajuste, paste0("BVAR estrutural | overnight fim de mes | ", cfg$p, " lags"), saida)
write_csv(overnight_fim_mes |> mutate(date = as.Date(date)), file.path(saida, "overnight_fim_mes.csv"))
write_csv(ajuste$diagnostico, file.path(saida, "diagnostico_estimacao.csv"))

# 4. Graficos nas mesmas unidades e cores dos forecasts anteriores.
titulos["juro_curto"] <- "Juro curto | fim de mes (% a.a.)"
grafico <- preparar_grafico(resultado)
plotar_formato(grafico, saida)
gravar_status(pasta_modelo, "concluido", origem = as.character(resultado$origem),
              lags = cfg$p, lambda = cfg$lambda, mu = cfg$mu, delta = cfg$delta)
print(grafico$resumo |> filter(cenario == "Base", variavel %in% alvos_base,
  date %in% yearmonth(c("2026 Dec", "2027 Dec", "2028 Aug"))) |>
  select(variavel, date, media), n = Inf)
