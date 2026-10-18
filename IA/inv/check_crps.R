suppressMessages({library(tidyverse); library(fpp3); library(bsvarSIGNs); library(distributional)})
base <- "C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/"
m <- readRDS(paste0(base, "mats.rds"))
dados <- cbind(m$ext, m$int)
data_bal <- as_tsibble(tibble(date = yearmonth("2000 Jan") + 0:(nrow(dados) - 1), as_tibble(dados)), index = date)
N <- 10; p <- 12; h <- 12; icpi <- which(colnames(dados) == "cpi_core")

# media (choque = 0) e desvio padrao analitico (formula multi-step) de um conjunto (A, Sigma)
analitico <- function(A_s, S_s, x0) {
  x <- x0; mu <- numeric(h)
  for (j in 1:h) { y <- A_s %*% x; mu[j] <- y[icpi]; x <- c(y, x[1:(N * (p - 1))], 1) }
  comp <- rbind(A_s[, 1:(N * p)], cbind(diag(N * (p - 1)), matrix(0, N * (p - 1), N)))
  V <- matrix(0, N, N); M <- diag(N * p); sd <- numeric(h)
  for (j in 1:h) { Psi <- M[1:N, 1:N]; V <- V + Psi %*% S_s %*% t(Psi); sd[j] <- sqrt(V[icpi, icpi]); M <- comp %*% M }
  list(mu = mu, sd = sd)
}
# caminho simulado com choques sorteados
simulado <- function(A_s, S_s, x0) {
  x <- x0; L <- t(chol(S_s)); out <- numeric(h)
  for (j in 1:h) { y <- A_s %*% x + L %*% rnorm(N); out[j] <- y[icpi]; x <- c(y, x[1:(N * (p - 1))], 1) }
  out
}

origens <- c(180, 195, 210, 225, 240)
set.seed(123)
res <- map_dfr(seq_along(origens), function(k) {
  tt <- origens[k]
  d  <- dados[1:tt, ]
  sp <- specify_bsvarSIGN$new(data = d, p = 12)
  po <- estimate(sp, S = 200, show_progress = FALSE)
  A <- po$posterior$A; S <- po$posterior$Sigma
  x0 <- c(t(d[tt:(tt - p + 1), ]), 1)
  dt <- data_bal$date[tt] + 1:h

  # 1) mistura de 200 normais, uma por sorteio dos coeficientes, sem sortear choques
  an <- lapply(1:200, function(s) analitico(A[, , s], S[, , s], x0))
  mu <- sapply(an, `[[`, "mu"); sd <- sapply(an, `[[`, "sd")         # h x 200
  mistura <- lapply(1:h, function(j)
    dist_mixture(!!!lapply(1:200, function(s) dist_normal(mu[j, s], sd[j, s])),
                 weights = rep(1 / 200, 200)))

  # 2) simulacao com choques sorteados (a abordagem atual)
  sim <- sapply(1:200, function(s) simulado(A[, , s], S[, , s], x0))  # h x 200
  simulacao <- dist_sample(lapply(1:h, function(j) sim[j, ]))

  # 3) a proposta do usuario: coeficientes medios, choque zero, normal com desvio padrao analitico
  A_bar <- apply(A, c(1, 2), mean); S_bar <- apply(S, c(1, 2), mean)
  b <- analitico(A_bar, S_bar, x0)
  normal_unica <- dist_normal(b$mu, b$sd)

  bind_rows(
    tibble(.model = "simulacao",        date = dt, cpi_core = simulacao,    .mean = rowMeans(sim)),
    tibble(.model = "mistura_analitica", date = dt, cpi_core = do.call(c, mistura), .mean = rowMeans(mu)),
    tibble(.model = "normal_unica",     date = dt, cpi_core = normal_unica, .mean = b$mu)
  ) |> mutate(.id = k)
}) |>
  as_tsibble(index = date, key = c(.id, .model)) |>
  as_fable(response = "cpi_core", distribution = cpi_core)

cat("\n== RMSE, MAE e CRPS do cpi_core, 5 origens ==\n")
print(res |> accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS)))
