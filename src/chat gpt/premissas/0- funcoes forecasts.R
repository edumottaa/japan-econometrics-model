library(fpp3)
library(tidyverse)
library(distributional)
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
somente_funcoes_forecast <- TRUE
source("src/7- forecast/comparacao hiato e priors/1- forecast bvars.R", encoding = "UTF-8")
somente_funcoes_forecast <- FALSE
somente_funcoes_graficos <- TRUE
source("src/7- forecast/comparacao hiato e priors/2- graficos forecast bvars.R", encoding = "UTF-8")
somente_funcoes_graficos <- FALSE
raiz_premissas <- "src/Outputs/Forecast BVAR estrutural/premissas inflacao e BoJ"
raiz_modelos <- "data/processed/premissas_inflacao_boj"
dir.create(raiz_modelos, recursive = TRUE, showWarnings = FALSE)

referencia <- function(p) {
  sufixo <- if (p == 6L) "" else " lag12"
  list(ajuste = readRDS(paste0("data/processed/forecast_overnight_fim_mes", sufixo, "/modelo.rds")),
    resultado = readRDS(paste0("src/Outputs/Forecast BVAR estrutural/overnight fim de mes", sufixo, "/forecast_bvar.rds")))
}

indices_pesos <- function(pesos, S = length(pesos)) {
  pesos <- pesos / sum(pesos)
  set.seed(123)
  findInterval(runif(1) / S + (0:(S - 1L)) / S, c(0, cumsum(pesos)), rightmost.closed = TRUE)
}

salvar_previsoes <- function(resultado, pasta) {
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  x <- resultado$previsoes
  resultado$resumo <- as_tibble(x) |> transmute(.model, cenario, variavel, date,
    media = .mean, mediana = median(valor), inferior_95 = quantile(valor, .025),
    inferior_80 = quantile(valor, .10), superior_80 = quantile(valor, .90), superior_95 = quantile(valor, .975))
  saveRDS(resultado, file.path(pasta, "forecast_bvar.rds"))
  write_csv(resultado$resumo |> mutate(date = as.Date(date)), file.path(pasta, "resumo_forecast.csv"))
  write_csv(resultado$diagnosticos, file.path(pasta, "diagnosticos.csv"))
  plotar_formato(preparar_grafico(resultado), pasta)
  invisible(resultado)
}

reponderar_trajetorias <- function(resultado, indices, nome) {
  x <- as_tibble(resultado$previsoes)
  amostras <- parameters(x$valor)$x
  amostras <- lapply(seq_len(nrow(x)), function(i) amostras[[i]][indices[[x$cenario[i]]]])
  x$.model <- nome
  x$valor <- dist_sample(amostras)
  dimnames(x$valor) <- "valor"
  x$.mean <- mean(x$valor)
  resultado$previsoes <- x |> as_tsibble(key = c(.model, cenario, variavel), index = date) |>
    as_fable(response = "valor", distribution = "valor")
  resultado
}

# Corrigir a mesma recursao instalada, agora tambem com tendencia exogena.
prever_sem_tendencia <- prever_bvar
prever_bvar <- function(post, dados, condicoes, p) {
  N <- ncol(condicoes)
  if (dim(post$posterior$A)[2] == N * p + 1L)
    return(prever_sem_tendencia(post, dados, condicoes, p))
  h <- nrow(condicoes)
  exogenas <- matrix(seq_len(h) / 12, ncol = 1)
  set.seed(123)
  nativa <- bsvarSIGNs::forecast(post, horizon = h, exogenous_forecast = exogenas,
    conditional_forecast = condicoes)
  F <- nativa$forecasts
  ruido <- F - nativa$forecast_mean
  Y <- dados |> as_tibble() |> select(all_of(colnames(condicoes))) |> as.matrix()
  inicial <- c(t(Y[nrow(Y) - (0:(p - 1L)), , drop = FALSE]))
  padroes <- apply(!is.na(condicoes), 1, paste, collapse = "")
  for (s in seq_len(dim(F)[3])) {
    lags <- inicial
    sigma <- post$posterior$Sigma[, , s]
    ganhos <- list()
    for (j in seq_len(h)) {
      media <- drop(post$posterior$A[, , s] %*% c(lags, 1, exogenas[j, ]))
      sorteio <- media + ruido[, j, s]
      fixas <- which(!is.na(condicoes[j, ]))
      if (length(fixas)) {
        chave <- padroes[j]
        if (is.null(ganhos[[chave]])) ganhos[[chave]] <-
          sigma[, fixas, drop = FALSE] %*% solve(sigma[fixas, fixas, drop = FALSE])
        sorteio <- sorteio - drop(ganhos[[chave]] %*% (media[fixas] - nativa$forecast_mean[fixas, j, s]))
        sorteio[fixas] <- condicoes[j, fixas]
      }
      F[, j, s] <- sorteio
      lags <- c(sorteio, head(lags, N * (p - 1L)))
    }
  }
  erro <- max(abs(apply(F, 3, function(x) t(matrix(x, N, h))[!is.na(condicoes)]) - condicoes[!is.na(condicoes)]))
  stopifnot(all(is.finite(F)), erro <= 1e-8)
  list(sorteios = F, erro_condicoes = erro)
}
