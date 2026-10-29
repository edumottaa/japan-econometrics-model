# BVAR original: 12 lags e lambda 0,10; mesma base e mesmos cenarios.
Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(TZ = "America/Sao_Paulo")
library(fpp3)
library(tidyverse)
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
somente_funcoes_forecast <- TRUE
source("src/7- forecast/comparacao hiato e priors/1- forecast bvars.R", encoding = "UTF-8")
somente_funcoes_forecast <- FALSE
somente_funcoes_graficos <- TRUE
source("src/7- forecast/comparacao hiato e priors/2- graficos forecast bvars.R", encoding = "UTF-8")
somente_funcoes_graficos <- FALSE

# 1. Alterar somente lambda em relacao ao modelo de 12 lags.
referencia <- readRDS("data/processed/forecast_overnight_fim_mes lag12/modelo.rds")
forecast_referencia <- readRDS("src/Outputs/Forecast BVAR estrutural/overnight fim de mes lag12/forecast_bvar.rds")
cfg <- referencia$configuracao |> mutate(lambda = .10)
dados <- referencia$dados_modelo
stopifnot(cfg$p == 12L, ncol(dados) == 8L,
          identical(dados, forecast_referencia$dados_modelo),
          identical(select(cfg, -lambda), select(referencia$configuracao, -lambda)))
pasta_modelo <- "data/processed/forecast_overnight_fim_mes lag12 lambda010"
saida <- "src/Outputs/Forecast BVAR estrutural/overnight fim de mes lag12 lambda010"
dir.create(pasta_modelo, recursive = TRUE, showWarnings = FALSE)
dir.create(saida, recursive = TRUE, showWarnings = FALSE)
arquivo <- file.path(pasta_modelo, "modelo.rds")

if (file.exists(arquivo)) {
  ajuste <- readRDS(arquivo)
  stopifnot(identical(ajuste$configuracao, cfg), identical(ajuste$dados_modelo, dados))
} else {
  gravar_status(pasta_modelo, "estimando", lags = 12L, lambda = .10)
  ajuste <- estimar_bvar(dados, cfg, estrutural = TRUE)
  ajuste$dados_modelo <- dados
  ajuste$configuracao <- cfg
  saveRDS(ajuste, arquivo)
}

# 2. Projetar com as condicoes ja usadas; sem atualizar series ou planilhas.
preparar_caminhos <- function(dados) forecast_referencia$caminhos
stopifnot(dim(ajuste$post$posterior$A)[2] == 85L)
gravar_status(pasta_modelo, "forecast", lags = 12L, lambda = .10)
resultado <- forecast_ajuste(ajuste, "BVAR estrutural | 12 lags | lambda 0,10", saida)
stopifnot(identical(resultado$caminhos, forecast_referencia$caminhos),
          all(resultado$diagnosticos$finitos),
          all(resultado$diagnosticos$erro_condicoes <= 1e-8))
write_csv(ajuste$diagnostico, file.path(saida, "diagnostico_estimacao.csv"))
titulos["juro_curto"] <- "Juro curto | fim de mes (% a.a.)"
plotar_formato(preparar_grafico(resultado), saida)
gravar_status(pasta_modelo, "concluido", lags = 12L, lambda = .10)
