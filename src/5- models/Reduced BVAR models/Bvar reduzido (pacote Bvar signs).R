
# 5. Construção das matrizes dos blocos externo e doméstico

library(tidyverse)
library(bsvars)
library(bsvarSIGNs)

matrix_externa <- data_bal |>
  as_tibble() |>
  select(ust2y, ip_mundial, brent, vix) |>
  as.matrix()

matrix_interna <- data_bal |>
  as_tibble() |>
  select(atividade, cpi_core, juro_curto, inclinacao, usdjpy) |>
  as.matrix()

# 6. Especificação do BVAR
# Nove variáveis, sem bolsa. Mu = delta = 1 e lambda = 0,2 fixos; psi automático e fixo.
p <- 6  #seleção dos lags

# COVID contada após as p observações iniciais usadas como defasagens.
inicio_covid <- match(as.Date("2020-03-01"), as.Date(data_bal$date)) - p
if (is.na(inicio_covid) || inicio_covid <= 0) inicio_covid <- NULL
# Estimar os quatro parâmetros COVID somente com quatro meses já observados.
if (!is.null(inicio_covid) && nrow(data_bal) - p - inicio_covid + 1L < 4L) inicio_covid <- NULL

spec = specify_bsvarSIGN$new(
  data = matrix_interna,
  foreign = matrix_externa,
  p = p,
  hyper_mu = FALSE,
  hyper_delta = FALSE,
  hyper_lambda = FALSE,
  hyper_psi = FALSE,
  hyper_covid = inicio_covid,
  sign_structural = matrix(NA_real_, 9, 9)
)

# 7. Estimação e inspeção dos sorteios dos hiperparâmetros
set.seed(123)
if (!is.null(inicio_covid)) spec$estimate_hyper(S = 5000, burn_in = 2500)

dim(spec$prior$hyper)
hyper_draws = t(spec$prior$hyper)

# Trajetórias dos quatro hiperparâmetros COVID estimados, após o aquecimento.
if (!is.null(inicio_covid)) {
  hyper_covid_draws <- hyper_draws[, (ncol(hyper_draws) - 3):ncol(hyper_draws), drop = FALSE]
  colnames(hyper_covid_draws) <- c("COVID março", "COVID abril", "COVID maio", "Decaimento")
  plot.ts(hyper_covid_draws, nc = 1, col = "black",
          main = "Sorteios dos hiperparâmetros COVID", xlab = "Iteração")
}

# 8. Estimação dos parâmetros do BVAR
post = bsvars::estimate(spec, S = 2500, show_progress = FALSE)

# 8.1. Checagem simples dos resultados da estimação
coeficientes_sorteados <- matrix(
  post$posterior$A,
  ncol = dim(post$posterior$A)[3]
)
lags_domesticos <- unlist(lapply(0:(p - 1), function(j) 5:9 + 9 * j))

diagnostico_estimacao <- tibble(
  sorteios = ncol(coeficientes_sorteados),
  valores_validos = all(is.finite(coeficientes_sorteados)) &&
    all(is.finite(post$posterior$Sigma)),
  conjuntos_distintos = ncol(unique(coeficientes_sorteados, MARGIN = 2)),
  ess_ponderacao = post$posterior$ess,
  coef_dom_nas_externas = max(abs(post$posterior$A[1:4, lags_domesticos, ])),
  impacto_dom_nas_externas = max(abs(post$posterior$Theta0[1:4, 5:9, ]))
)

diagnostico_estimacao

if (!diagnostico_estimacao$valores_validos || !is.finite(diagnostico_estimacao$ess_ponderacao) ||
    diagnostico_estimacao$ess_ponderacao < 50 || diagnostico_estimacao$conjuntos_distintos < 50 ||
    diagnostico_estimacao$coef_dom_nas_externas > 1e-8 || diagnostico_estimacao$impacto_dom_nas_externas > 1e-8) {
  stop("O ajuste nao passou pela checagem numerica; nao interpretar as previsoes.")
}
