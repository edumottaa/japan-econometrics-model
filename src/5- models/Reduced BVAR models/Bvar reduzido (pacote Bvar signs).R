
# 5. Construção das matrizes dos blocos externo e doméstico

library(tidyverse)
library(fpp3)
library(bsvars)
library(bsvarSIGNs)

# Mesma amostra e transformacoes do BVAR com producao mundial (ja em 100 * log).
data_bal <- readRDS("C:/Users/dudup/OneDrive/japan-econometrics-model/data/processed/cenarios_propios_20261104/modelo_producao/dados.rds") |>
  select(date, ust2y, brent, ip_mundial, atividade, cpi_core, juro_curto, inclinacao, usdjpy) |>
  arrange(date) |>
  as_tsibble(index = date)

matrix_externa <- data_bal |>
  as_tibble() |>
  select(ust2y, brent, ip_mundial) |>
  as.matrix()

matrix_interna <- data_bal |>
  as_tibble() |>
  select(atividade, cpi_core, juro_curto, inclinacao, usdjpy) |>
  as.matrix()

# 6. Especificação do BVAR
# Oito variáveis: três externas e cinco domésticas, sem restricoes de sinais ou narrativas.
# Mesmos lags, tendencia, priors e pesos da configuracao estrutural.
p <- 6  #seleção dos lags
lambda <- 0.2
mu <- delta <- 1
rho_peso <- 0.995
stopifnot(rho_peso > 0, rho_peso <= 1)
tendencia <- matrix((seq_len(nrow(matrix_interna)) - nrow(matrix_interna)) / 12, ncol = 1)

# COVID contada após as p observações iniciais usadas como defasagens.
inicio_covid <- match(as.Date("2020-03-01"), as.Date(data_bal$date)) - p
if (is.na(inicio_covid) || inicio_covid <= 0) inicio_covid <- NULL
# Estimar os quatro parâmetros COVID somente com quatro meses já observados.
if (!is.null(inicio_covid) && nrow(data_bal) - p - inicio_covid + 1L < 4L) inicio_covid <- NULL

spec = specify_bsvarSIGN$new(
  data = matrix_interna,
  foreign = matrix_externa,
  exogenous = tendencia,
  p = p,
  hyper_mu = FALSE,
  hyper_delta = FALSE,
  hyper_lambda = FALSE,
  hyper_psi = FALSE,
  hyper_covid = inicio_covid,
  sign_structural = matrix(NA_real_, 8, 8)
)

# Mesmos priors fixos e ponderacao exponencial, depois de construir as defasagens.
spec$prior$hyper[1:3, ] <- c(mu, delta, lambda)
Y_bvar <- spec$data_matrices$Y
X_bvar <- spec$data_matrices$X
pesos <- rho_peso^rev(seq_len(ncol(Y_bvar)) - 1)
spec$data_matrices$Y <- sweep(Y_bvar, 2, sqrt(pesos), "*")
spec$data_matrices$X <- sweep(X_bvar, 2, sqrt(pesos), "*")
spec$prior$Y <- spec$data_matrices$Y
spec$prior$X <- spec$data_matrices$X

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

# Recuperar o histórico original para o forecast.
post$last_draw$data_matrices$Y <- Y_bvar
post$last_draw$data_matrices$X <- X_bvar
post$last_draw$prior$Y <- Y_bvar
post$last_draw$prior$X <- X_bvar

# Recuperar as escalas e os choques de cada sorteio nas unidades do histórico.
# O pacote reamostra os parâmetros; recalcular mantém os choques alinhados a eles.
for (s in seq_len(dim(post$posterior$A)[3])) {
  escala <- rep(1, ncol(Y_bvar))
  covid <- tail(post$posterior$hyper[, s], 4)
  if (!is.null(inicio_covid)) {
    escala[inicio_covid + 0:2] <- covid[1:3]
    if (inicio_covid + 3 <= length(escala)) {
      meses <- (inicio_covid + 3):length(escala)
      escala[meses] <- 1 + (covid[3] - 1) * covid[4]^seq_along(meses)
    }
  }
  escala <- escala / sqrt(pesos)
  post$posterior$sigma[, , s] <- matrix(escala, nrow(Y_bvar), ncol(Y_bvar), byrow = TRUE)
  post$posterior$shocks[, , s] <- post$posterior$B[, , s] %*%
    sweep(Y_bvar - post$posterior$A[, , s] %*% X_bvar, 2, escala, "/")
}
pesos_exponenciais <- tibble(date = data_bal$date[-seq_len(p)], peso = pesos)
attr(post, "pesos_exponenciais") <- list(rho = rho_peso, dados = pesos_exponenciais,
  metodo = "Precisão temporal: Var(erro_t) = escala_COVID_t^2 * Sigma / peso_t")

# 8.1. Checagem simples dos resultados da estimação
coeficientes_sorteados <- matrix(
  post$posterior$A,
  ncol = dim(post$posterior$A)[3]
)
lags_domesticos <- unlist(lapply(0:(p - 1), function(j) 4:8 + 8 * j))

diagnostico_estimacao <- tibble(
  sorteios = ncol(coeficientes_sorteados),
  valores_validos = all(is.finite(coeficientes_sorteados)) &&
    all(is.finite(post$posterior$Sigma)),
  conjuntos_distintos = ncol(unique(coeficientes_sorteados, MARGIN = 2)),
  ess_ponderacao = post$posterior$ess,
  coef_dom_nas_externas = max(abs(post$posterior$A[1:3, lags_domesticos, ])),
  impacto_dom_nas_externas = max(abs(post$posterior$Theta0[1:3, 4:8, ]))
)

diagnostico_estimacao

if (!diagnostico_estimacao$valores_validos || !is.finite(diagnostico_estimacao$ess_ponderacao) ||
    diagnostico_estimacao$ess_ponderacao < 50 || diagnostico_estimacao$conjuntos_distintos < 50 ||
    diagnostico_estimacao$coef_dom_nas_externas > 1e-8 || diagnostico_estimacao$impacto_dom_nas_externas > 1e-8) {
  stop("O ajuste nao passou pela checagem numerica; nao interpretar as previsoes.")
}
