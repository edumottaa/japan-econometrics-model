suppressMessages({library(tidyverse); library(fpp3); library(bsvarSIGNs); library(distributional)})
base <- "C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/"
m <- readRDS(paste0(base, "mats.rds"))
dados <- cbind(m$ext, m$int)
data_bal <- as_tsibble(tibble(date = yearmonth("2000 Jan") + 0:(nrow(dados) - 1), as_tibble(dados)), index = date)
icpi <- which(colnames(dados) == "cpi_core")

# ---- TESTE 1: a origem da previsao do pacote, com p = 1 ----
sp1 <- specify_bsvarSIGN$new(data = dados, p = 1)
set.seed(1)
po1 <- estimate(sp1, S = 20, show_progress = FALSE)
fc1 <- forecast(po1, horizon = 2)
A1 <- po1$posterior$A[, , 1]
x_ajuste <- sp1$data_matrices$X[, ncol(sp1$data_matrices$X)]   # regressores usados no ultimo ajuste
x_zero   <- c(dados[nrow(dados), ], 1)                         # regressores do mes seguinte ao ultimo dado
cat("\nTESTE 1 (p = 1), CPI, mes 1 da previsao:\n")
cat("  pacote - A x ultimo ajuste :", signif(fc1$forecast_mean[icpi, 1, 1] - (A1 %*% x_ajuste)[icpi], 4), "\n")
cat("  pacote - A x ultimo dado   :", signif(fc1$forecast_mean[icpi, 1, 1] - (A1 %*% x_zero)[icpi], 4), "\n")

# ---- TESTE 2: formula estilo fable, com a posterior do bsvarSIGNs (p = 12) ----
previsao_formula <- function(post, dados, p = 12, h = 12) {
  A <- apply(post$posterior$A, c(1, 2), mean)
  S <- apply(post$posterior$Sigma, c(1, 2), mean)
  N <- ncol(dados); T <- nrow(dados)
  icpi <- which(colnames(dados) == "cpi_core")
  comp <- rbind(A[, 1:(N * p)], cbind(diag(N * (p - 1)), matrix(0, N * (p - 1), N)))
  x <- c(t(dados[T:(T - p + 1), ]), 1)
  M <- diag(N * p); V <- matrix(0, N, N)
  media <- numeric(h); desvio <- numeric(h)
  for (j in 1:h) {
    y <- A %*% x
    media[j] <- y[icpi]
    x <- c(y, x[1:(N * (p - 1))], 1)
    Psi <- M[1:N, 1:N]
    V <- V + Psi %*% S %*% t(Psi)
    desvio[j] <- sqrt(V[icpi, icpi])
    M <- comp %*% M
  }
  list(media = media, desvio = desvio)
}

origens <- c(180, 210, 240)
S_cv <- 100
fc_formula <- map_dfr(seq_along(origens), function(k) {
  fim <- origens[k]
  dados_t <- dados[1:fim, ]
  post_t <- estimate(specify_bsvarSIGN$new(data = dados_t, p = 12), S = S_cv, show_progress = FALSE)
  r <- previsao_formula(post_t, dados_t)
  tibble(.id = k, .model = "bvar", date = data_bal$date[fim] + 1:12,
         cpi_core = dist_normal(r$media, r$desvio), .mean = r$media)
}) |>
  as_tsibble(index = date, key = c(.id, .model)) |>
  as_fable(response = "cpi_core", distribution = cpi_core)

cat("\nTESTE 2: formula estilo fable, bsvarSIGNs, 3 origens\n")
print(fc_formula |> accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS)))
print(head(fc_formula, 3))
