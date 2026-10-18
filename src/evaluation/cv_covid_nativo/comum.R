.libPaths(c("C:/Users/dudup/OneDrive/japan-econometrics-model/software/bsvarSIGNs-3.0-forecast-fix/library", "C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
suppressPackageStartupMessages({library(tidyverse); library(fpp3); library(bsvarSIGNs); library(bsvars); library(BVAR)})
root <- "C:/Users/dudup/OneDrive/japan-econometrics-model"
externas <- c("ust2y", "ip_mundial", "brent", "vix")
domesticas <- c("atividade", "cpi_core", "juro_curto", "inclinacao", "usdjpy")
variaveis <- c(externas, domesticas)
S <- 2500L
H <- 24L

# Cada etapa usa a mesma base, as mesmas origens e as mesmas sementes.
modelos <- tribble(
  ~etapa, ~modelo, ~p, ~familia, ~covid, ~dummies,
  1L, "foreign_p3_covid", 3L, "foreign", TRUE, FALSE,
  1L, "foreign_p6_covid", 6L, "foreign", TRUE, FALSE,
  1L, "foreign_p3_sem_covid", 3L, "foreign", FALSE, FALSE,
  1L, "foreign_p6_sem_covid", 6L, "foreign", FALSE, FALSE,
  2L, "foreign_p3_covid_dummies", 3L, "foreign", TRUE, TRUE,
  2L, "foreign_p6_covid_dummies", 6L, "foreign", TRUE, TRUE,
  3L, "condicionado_p3", 3L, "condicionado", FALSE, FALSE,
  3L, "condicionado_p6", 6L, "condicionado", FALSE, FALSE,
  4L, "foreign_p12_covid", 12L, "foreign", TRUE, FALSE,
  4L, "foreign_p12_sem_covid", 12L, "foreign", FALSE, FALSE,
  5L, "condicionado_p12", 12L, "condicionado", FALSE, FALSE
)

dummies_covid <- function(datas) {
  D <- outer(as.numeric(as.Date(datas)),
    as.numeric(as.Date(c("2020-03-01", "2020-04-01", "2020-05-01"))), "==") * 1
  colnames(D) <- c("covid_mar20", "covid_abr20", "covid_mai20")
  D <- D[, colSums(D) > 0, drop = FALSE]
  if (ncol(D) == 0) NULL else D
}

# O CRPS de cada previsao e calculado pela funcao do fable, sem formula propria.
avaliar_distribuicao <- function(distribuicao, treino, teste, id, modelo, variavel) {
  nh <- nrow(teste)
  valores_reais <- teste[[variavel]]
  crps <- map_dbl(seq_len(nh), function(j) fabletools::CRPS(distribuicao[j], valores_reais[j]))
  tibble(.id = id, .model = modelo, variavel = variavel, origem = max(treino$date),
    date = teste$date, h = seq_len(nh), observado = valores_reais,
    previsao = as.numeric(mean(distribuicao))[seq_len(nh)],
    CRPS = crps)
}

avaliar_sorteios <- function(F, treino, teste, id, modelo) {
  stopifnot(identical(dim(F)[1:2], c(9L, H)), all(is.finite(F)))
  map_dfr(domesticas, function(v) {
    k <- match(v, variaveis)
    amostras <- matrix(F[k, seq_len(nrow(teste)), , drop = FALSE], nrow = nrow(teste))
    distribuicao <- distributional::dist_sample(lapply(seq_len(nrow(teste)), function(j) amostras[j, ]))
    avaliar_distribuicao(distribuicao, treino, teste, id, modelo, v)
  })
}

status <- function(out, nome, ...) {
  jsonlite::write_json(list(atualizado = format(Sys.time(), usetz = TRUE), ...),
    file.path(out, "status", paste0(nome, ".json")), auto_unbox = TRUE, pretty = TRUE)
}

salvar_janela <- function(x, arquivo) {
  temporario <- paste0(arquivo, ".tmp")
  saveRDS(x, temporario)
  stopifnot(file.rename(temporario, arquivo))
}