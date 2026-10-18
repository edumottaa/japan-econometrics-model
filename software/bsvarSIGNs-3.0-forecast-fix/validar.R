args <- commandArgs(TRUE)
fix <- "C:/Users/dudup/OneDrive/japan-econometrics-model/software/bsvarSIGNs-3.0-forecast-fix"
.libPaths(c(if (args[1] == "fixed") file.path(fix, "library"), "C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
suppressPackageStartupMessages(library(bsvarSIGNs))
set.seed(991)
y <- cbind(sin((1:140)/9) + rnorm(140, 0, .01), cos((1:140)/11) + rnorm(140, 0, .01))
rows <- list()
for (p in c(1L, 2L, 3L, 6L, 12L)) for (exog in c(FALSE, TRUE)) {
  D <- if (exog) cbind(as.numeric(seq_len(140) == 100), as.numeric(seq_len(140) == 101)) else NULL
  spec <- specify_bsvarSIGN$new(y, p = p, exogenous = D, hyper_mu = FALSE,
    hyper_delta = FALSE, hyper_lambda = FALSE, hyper_psi = FALSE,
    sign_structural = matrix(NA_real_, 2, 2))
  set.seed(200 + p)
  post <- bsvars::estimate(spec, S = 10L, show_progress = FALSE)
  a <- if (p == 1L) .5 else if (p == 2L) c(1.5, -.6) else c(.4, rep(0, p - 2), .1)
  post$posterior$A[] <- 0
  for (s in 1:10) {
    for (j in seq_len(p)) post$posterior$A[, (j-1L)*2L + 1:2, s] <- diag(a[j], 2)
    post$posterior$A[, 2L*p+1L, s] <- .125
    if (exog) post$posterior$A[, 2L*p + 2:3, s] <- matrix(c(.7, -.4, .2, .3), 2)
    post$posterior$Sigma[, , s] <- diag(1e-24, 2)
  }
  future <- if (exog) cbind(seq_len(24)/24, rep(c(1, 0, -1), 8)) else NULL
  f <- bsvarSIGNs::forecast(post, 24, exogenous_forecast = future)
  target <- sapply(1:2, function(k) {
    forcing <- rep(.125, 24)
    if (exog) forcing <- forcing + drop(future %*% post$posterior$A[k, 2L*p + 2:3, 1])
    as.numeric(stats::filter(forcing, a, method = "recursive", init = rev(tail(y[, k], p))))
  })
  error <- max(abs(t(f$forecast_mean[, , 1]) - target))
  rows[[length(rows)+1L]] <- data.frame(test = paste0("AR_p",p,"_exog",exog), max_error = error, passed = error < 1e-8)
}
# Future COVID covariance: preserve the package's native scale formula.
spec <- specify_bsvarSIGN$new(y, p = 2L, hyper_mu = FALSE, hyper_delta = FALSE,
  hyper_lambda = FALSE, hyper_psi = FALSE, hyper_covid = 134L,
  sign_structural = matrix(NA_real_, 2, 2))
spec$prior$covid <- 134L # Known COVID profile: activate it without estimating hyperparameters.
set.seed(813); post <- bsvars::estimate(spec, S = 10L, show_progress = FALSE)
post$posterior$hyper[6:9, ] <- rep(c(2, 3, 4, .8), 10)
post$posterior$Sigma[] <- 0
for (s in 1:10) post$posterior$Sigma[, , s] <- matrix(c(1, .3, .3, 2), 2)
f <- bsvarSIGNs::forecast(post, 24)
T <- ncol(post$last_draw$data_matrices$X)
scale <- 1 + (4 - 1) * .8^(T + 0:23 - (134 - 1) - 2)
error <- max(vapply(1:24, function(h) max(abs(f$forecast_covariance[, , h, 1] - scale[h]^2 * post$posterior$Sigma[, , 1])), numeric(1)))
rows[[length(rows)+1L]] <- data.frame(test = "COVID_covariance", max_error = error, passed = error < 1e-10)
# Conditioning remains the native conditional Gaussian draw.
cf <- matrix(NA_real_, 24, 2); cf[, 1] <- seq_len(24)/10
f <- bsvarSIGNs::forecast(post, 24, conditional_forecast = cf)
error <- max(abs(f$forecasts[1, , ] - cf[, 1]))
rows[[length(rows)+1L]] <- data.frame(test = "conditional_path", max_error = error, passed = error < 1e-10)
# Same native fit in separate runs: source patch must not change estimation.
set.seed(20261018)
spec <- specify_bsvarSIGN$new(data = y[, 2, drop = FALSE], foreign = y[, 1, drop = FALSE],
  p = 3L, hyper_mu = FALSE, hyper_delta = FALSE, hyper_lambda = FALSE,
  hyper_psi = FALSE, sign_structural = matrix(NA_real_, 2, 2))
post <- bsvars::estimate(spec, S = 100L, show_progress = FALSE)
saveRDS(post$posterior, file.path(fix, paste0("posterior_", args[1], ".rds")))
result <- do.call(rbind, rows)
write.csv(result, file.path(fix, paste0("tests_", args[1], ".csv")), row.names = FALSE)
print(result); cat("Package:", as.character(packageVersion("bsvarSIGNs")), find.package("bsvarSIGNs"), "\n")
if (args[1] == "fixed") stopifnot(all(result$passed))