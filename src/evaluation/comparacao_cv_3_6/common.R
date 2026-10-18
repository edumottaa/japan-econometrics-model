.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
suppressPackageStartupMessages({library(tidyverse); library(fpp3); library(BVAR); library(bsvars); library(bsvarSIGNs)})
root <- "C:/Users/dudup/OneDrive/japan-econometrics-model"
source(file.path(root, "src/evaluation/prever_bsvarsigns.R"))
vars <- c("ust2y", "ip_mundial", "brent", "vix", "atividade", "cpi_core", "juro_curto", "inclinacao", "usdjpy", "topix")
alvos <- c("cpi_core", "atividade")

log_line <- function(...) {cat(format(Sys.time(), "%H:%M:%S"), ..., "\n"); flush(stdout())}
write_status <- function(path, ...) {
  jsonlite::write_json(list(..., atualizado_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)), path, auto_unbox = TRUE, pretty = TRUE, null = "null")
}

# CRPS empírico exato; evita depender da extração de amostras entre versões do fable/distributional.
crps_emp <- function(x, y) {
  x <- sort(x); n <- length(x)
  mean(abs(x - y)) - sum((2 * seq_len(n) - n - 1) * x) / n^2
}
crps_norm <- function(mu, sd, y) {
  z <- (y - mu) / sd
  ans <- sd * (z * (2 * pnorm(z) - 1) + 2 * dnorm(z) - 1 / sqrt(pi))
  ans[sd == 0] <- abs(y[sd == 0] - mu[sd == 0]); ans
}
base_scores <- function(model, fold, variable, origin, dates, y, mu, crps, q16, q84, q05, q95, quality = "ok") {
  tibble(model, fold, variable, origin = as.Date(origin), date = as.Date(dates), h = seq_along(y),
    actual = y, mean = mu, error = mu - y, squared_error = (mu - y)^2,
    absolute_error = abs(mu - y), crps, q16, q84, q05, q95, quality)
}
score_normal <- function(model, fold, variable, origin, dates, y, mu, sd) {
  base_scores(model, fold, variable, origin, dates, y, mu, crps_norm(mu, sd, y),
    mu + qnorm(.16) * sd, mu + qnorm(.84) * sd, mu + qnorm(.05) * sd, mu + qnorm(.95) * sd)
}
score_draws <- function(F, train, test, fold, model, quality = "ok") {
  stopifnot(length(dim(F)) == 3, dim(F)[1] == 10, all(is.finite(F)))
  map_dfr(alvos, function(v) {
    a <- F[match(v, vars), seq_len(nrow(test)), , drop = FALSE]
    a <- matrix(a, nrow = nrow(test))
    qs <- t(apply(a, 1, quantile, probs = c(.05, .16, .84, .95), names = FALSE))
    base_scores(model, fold, v, max(train$date), test$date, test[[v]], rowMeans(a),
      vapply(seq_len(nrow(test)), function(h) crps_emp(a[h, ], test[[v]][h]), numeric(1)),
      qs[, 2], qs[, 3], qs[, 1], qs[, 4], quality)
  })
}

# Converter apenas a organização dos coeficientes para reutilizar a previsão recursiva.
# BVAR: constante primeiro; bsvarSIGNs/função recursiva: constante por último.
prever_normal <- function(fit, Y, h) {
  A <- aperm(fit$beta, c(3, 2, 1)); K <- dim(A)[2]
  objeto <- list(posterior = list(A = A[, c(2:K, 1), , drop = FALSE],
      Sigma = aperm(fit$sigma, c(2, 3, 1))), last_draw = list(p = fit$meta$lags))
  prever_bsvarsigns(objeto, Y, h)
}

diagnostico_bvar <- function(fit, p, fold) {
  ess <- vapply(seq_len(ncol(fit$hyper)), function(j) as.numeric(coda::effectiveSize(fit$hyper[, j])), numeric(1))
  distinct <- vapply(seq_len(ncol(fit$hyper)), function(j) length(unique(fit$hyper[, j])), integer(1))
  tibble(family = "BVAR", p, fold, draws = dim(fit$beta)[1],
    acceptance = fit$meta$accepted / (fit$meta$n_draw - fit$meta$n_burn),
    ess_hyper_min = min(ess), hyper_distinct_min = min(distinct),
    values_finite = all(is.finite(fit$beta)) && all(is.finite(fit$sigma)),
    quality = if (any(!is.finite(ess)) || min(distinct) <= 5 || !all(is.finite(fit$beta))) "diagnostic_failure" else if (min(ess) < 50) "low_hyper_ess" else "ok")
}
diagnostico_signs <- function(post, raw_weights, p, fold) {
  w <- as.numeric(raw_weights)
  if (length(w) && all(is.finite(w)) && max(w) > 0) {
    w <- w / max(w); w <- w / sum(w)
    ess <- 1 / sum(w^2); max_share <- max(w)
  } else {ess <- NA_real_; max_share <- NA_real_}
  S <- dim(post$posterior$A)[3]
  distinct <- ncol(unique(matrix(post$posterior$A, ncol = S), MARGIN = 2))
  dom_lags <- unlist(lapply(0:(p - 1), function(j) 5:10 + 10 * j))
  b1 <- max(abs(post$posterior$A[1:4, dom_lags, ]))
  b2 <- max(abs(post$posterior$Theta0[1:4, 5:10, ]))
  finite <- all(is.finite(post$posterior$A)) && all(is.finite(post$posterior$Sigma))
  tibble(family = "bsvarSIGNs", p, fold, draws = S, ess_importance = ess,
    max_weight_share = max_share, distinct_coefficients = distinct,
    domestic_lags_in_foreign = b1, domestic_impact_in_foreign = b2,
    values_finite = finite, quality = if (!finite || !is.finite(ess) || ess < 50 || distinct < 50 || b1 > 1e-8 || b2 > 1e-8) "diagnostic_failure" else "ok")
}
