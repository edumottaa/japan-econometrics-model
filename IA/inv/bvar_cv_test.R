suppressMessages({library(tidyverse); library(fpp3); library(bsvarSIGNs)})
m <- readRDS("C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/mats.rds")
data_bal <- as_tsibble(tibble(date = yearmonth("2000 Jan") + 0:(nrow(m$ext) - 1),
                              as_tibble(cbind(m$ext, m$int))), index = date)
matrix_externa <- m$ext; matrix_interna <- m$int

# ---- code for the user ----
previsao_bvar <- function(post, dados, p = 12, h = 12) {
  A     <- post$posterior$A        # coeficientes: 10 x 121 x S
  Sigma <- post$posterior$Sigma    # covariancia dos choques: 10 x 10 x S
  N <- ncol(dados)
  T <- nrow(dados)
  S <- dim(A)[3]
  out <- array(NA, c(h, N, S), dimnames = list(NULL, colnames(dados), NULL))
  for (s in 1:S) {
    x <- c(t(dados[T:(T - p + 1), ]), 1)                 # ultimos 12 meses + constante
    for (j in 1:h) {
      y <- A[, , s] %*% x + t(chol(Sigma[, , s])) %*% rnorm(N)
      out[j, , s] <- y
      x <- c(y, x[1:(N * (p - 1))], 1)                   # empurra as defasagens um mes
    }
  }
  out
}

dados   <- cbind(matrix_externa, matrix_interna)
origens <- seq(180, nrow(dados) - 1, by = 3)
origens <- origens[1:2]   # TEST ONLY
S_cv    <- 20             # TEST ONLY

t0 <- Sys.time()
fc_bvar <- map_dfr(seq_along(origens), function(k) {
  t <- origens[k]
  spec_t <- specify_bsvarSIGN$new(data = dados[1:t, ], p = 12)
  post_t <- estimate(spec_t, S = S_cv, show_progress = FALSE)
  fc     <- previsao_bvar(post_t, dados[1:t, ])
  tibble(.id      = k,
         .model   = "bvar",
         date     = data_bal$date[t] + 1:12,
         cpi_core = distributional::dist_sample(lapply(1:12, function(j) fc[j, "cpi_core", ])),
         .mean    = apply(fc[, "cpi_core", ], 1, mean))
}) |>
  as_tsibble(index = date, key = c(.id, .model)) |>
  as_fable(response = "cpi_core", distribution = cpi_core)
# ----------------------------
cat("seconds for 2 origins x", S_cv, "draws:", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "\n")
print(fc_bvar |> accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS)))
print(head(fc_bvar, 3))
cat("origins in full run:", length(seq(180, nrow(dados) - 1, by = 3)), "\n")
