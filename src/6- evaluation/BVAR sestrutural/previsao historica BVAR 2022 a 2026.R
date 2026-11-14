# Previsao historica de origem fixa: treino ate dezembro de 2021.
# BVAR atual com produção mundial, tendência e spread JGB30Y−5Y.
# Mesmo BVAR reduzido, sem sinais, narrativas ou filtro de estabilidade.
library(tidyverse)
library(fpp3)
library(bsvars)
library(bsvarSIGNs)

# 1. Amostra e configuracao do modelo principal.
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
dados <- readRDS(file.path(raiz, 'data/processed/bvar_producao_tendencia_jgb30_5_20261112/dados.rds')) |>
  select(date, ust2y, brent, ip_mundial, atividade, cpi_core, juro_curto, inclinacao, usdjpy) |>
  arrange(date)
origem <- yearmonth('2021 Dec')
treino <- dados |> filter(date <= origem)
teste <- dados |> filter(date > origem)
h <- nrow(teste)
p <- 6
lambda <- 0.2
mu <- delta <- 1
rho_peso <- 0.995
S <- 2500
variaveis <- names(dados)[-1]
domesticas <- tail(variaveis, 5)
matriz <- treino |> as_tibble() |> select(-date) |> as.matrix()
tendencia <- matrix((seq_len(nrow(treino)) - nrow(treino)) / 12, ncol = 1)
tendencia_futura <- matrix(seq_len(h) / 12, ncol = 1)
inicio_covid <- match(as.Date('2020-03-01'), as.Date(treino$date)) - p

# 2. Estimar os parametros usando somente o treino.
set.seed(2022)
spec <- specify_bsvarSIGN$new(data = matriz[, 4:8], foreign = matriz[, 1:3],
  exogenous = tendencia, p = p, hyper_mu = FALSE, hyper_delta = FALSE,
  hyper_lambda = FALSE, hyper_psi = FALSE, hyper_covid = inicio_covid,
  sign_structural = matrix(NA_real_, 8, 8))
spec$prior$hyper[1:3, ] <- c(mu, delta, lambda)
Y <- spec$data_matrices$Y
X <- spec$data_matrices$X
pesos <- rho_peso^rev(seq_len(ncol(Y)) - 1)
spec$data_matrices$Y <- sweep(Y, 2, sqrt(pesos), '*')
spec$data_matrices$X <- sweep(X, 2, sqrt(pesos), '*')
spec$prior$Y <- spec$data_matrices$Y
spec$prior$X <- spec$data_matrices$X
spec$estimate_hyper(S = 5000, burn_in = 2500)
post <- bsvars::estimate(spec, S = S, show_progress = FALSE)
stopifnot(is.finite(post$posterior$ess), post$posterior$ess >= 50,
  all(is.finite(post$posterior$A)), all(is.finite(post$posterior$Sigma)))
post$last_draw$data_matrices$Y <- Y
post$last_draw$data_matrices$X <- X
post$last_draw$prior$Y <- Y
post$last_draw$prior$X <- X

# 3. Duas previsoes: externas realizadas ou externas previstas pelo BVAR.
# A segunda nao recebe dados de 2022 em diante; ambas usam a mesma posterior.
trajetorias <- list()
for (tipo in c('BVAR ex post', 'BVAR sem externas realizadas')) {
  condicoes <- matrix(NA_real_, h, 8)
  if (tipo == 'BVAR ex post') condicoes[, 1:3] <- as.matrix(select(as_tibble(teste), ust2y, brent, ip_mundial))
  set.seed(123)
  previsao <- bsvarSIGNs::forecast(post, horizon = h,
    conditional_forecast = condicoes, exogenous_forecast = tendencia_futura)
  F <- previsao$forecasts
  medias <- previsao$forecast_mean
  inovacoes <- F - medias
  estado_inicial <- c(t(matriz[nrow(matriz) - (0:(p - 1)), , drop = FALSE]), 1, 1 / 12)
  for (s in seq_len(S)) {
    estado <- estado_inicial
    sigma <- post$posterior$Sigma[, , s]
    ganho <- sigma[, 1:3, drop = FALSE] %*% solve(sigma[1:3, 1:3, drop = FALSE])
    for (j in seq_len(h)) {
      media <- drop(post$posterior$A[, , s] %*% estado)
      ajuste <- rep(0, 8)
      if (tipo == 'BVAR ex post') ajuste <- drop(ganho %*% (media[1:3] - medias[1:3, j, s]))
      F[, j, s] <- media + inovacoes[, j, s] - ajuste
      if (tipo == 'BVAR ex post') F[1:3, j, s] <- condicoes[j, 1:3]
      estado <- c(F[, j, s], head(estado, 8 * (p - 1)), 1, (j + 1) / 12)
    }
  }
  stopifnot(all(is.finite(F)))
  trajetorias[[tipo]] <- F
}

# 4. Benchmarks ajustados na mesma amostra, sem informacao futura.
benchmarks <- list()
for (alvo in domesticas) {
  serie <- treino |> select(date, valor = all_of(alvo))
  ajuste <- serie |> model(
    AR = ARIMA(valor ~ pdq(d = 1, q = 0) + PDQ(D = 0, Q = 0)),
    Naive = NAIVE(valor), Drift = RW(valor ~ drift()))
  benchmarks[[alvo]] <- ajuste |> forecast(h = h) |> as_tibble() |>
    mutate(variavel = alvo) |> select(.model, date, variavel, valor, .mean)
}
ajuste_var <- treino |> model(VAR = VAR(vars(atividade, cpi_core, juro_curto)))
previsao_var <- ajuste_var |> forecast(h = h)

# 5. Armazenamento da saida em outputs.
saida <- file.path(raiz, 'src/Outputs/2- Evaluation/Desempenho BVAR reduzido/CV pesos producao mundial JGB30Y-5Y')
dir.create(saida, recursive = TRUE, showWarnings = FALSE)
saveRDS(list(origem = origem, treino = treino, teste = teste, trajetorias = trajetorias,
  benchmarks = bind_rows(benchmarks), var = previsao_var, ess = post$posterior$ess,
  configuracao = list(p = p, rho = rho_peso, lambda = lambda, mu = mu, delta = delta, S = S)),
  file.path(saida, 'previsao origem fixa 2021.rds'))
message('PREVISAO_ORIGEM_FIXA_CONCLUIDA | ESS ', round(post$posterior$ess, 1))
