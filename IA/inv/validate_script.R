suppressMessages({library(tidyverse); library(fpp3); library(bsvarSIGNs)})
base <- "C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/"
ns <- asNamespace("bsvarSIGNs")
m <- readRDS(paste0(base, "inv/mats.rds"))
data_bal <- as_tsibble(tibble(date = yearmonth("2000 Jan") + 0:(nrow(m$ext) - 1),
                              as_tibble(cbind(m$ext, m$int))), index = date)

# 1) Run sections 5-8 of the corrected script exactly as written (up to the plot)
src  <- readLines(paste0(base, "model_clean_comments_corrigido.R"), encoding = "UTF-8")
code <- src[grep("^# 5 Build the model data matrix", src):length(src)]
cut_at <- grep("ggplot() +", code, fixed = TRUE)
t0 <- Sys.time()
eval(parse(text = code[1:(cut_at - 1)]))
cat("time sections 5-8:", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s\n")
cat("acceptance rate:", round(accepted / n_draw, 3), "\n")
print(round(t(apply(hyper, 1, quantile, probs = c(0.05, 0.5, 0.95))), 4))
half <- n_keep / 2
cat("mean of 1st vs 2nd half of kept draws (mu, delta, lambda):\n")
print(round(rbind(first = rowMeans(hyper[1:3, 1:half]), second = rowMeans(hyper[1:3, -(1:half)])), 4))
if (requireNamespace("coda", quietly = TRUE))
  print(round(coda::effectiveSize(coda::mcmc(t(draws[, (n_burn + 1):n_draw])))))
cat("posterior draws:", length(post), "| any NA in B:", any(sapply(post, function(d) anyNA(d$B))),
    "| all Sigma positive definite:", all(sapply(post, function(d) all(eigen(d$Sigma, only.values = TRUE)$values > 0))), "\n")

# 2) How often the package's estimate() formula fails for these hyperparameter draws
niw <- get("_bsvarSIGNs_niw_cpp", ns)
pr  <- spec$prior$get_prior()
fails <- sapply(1:n_keep, function(s) {
  h <- hyper[, s]; N <- 10; p <- 12
  v <- diag(pr$V); v[1:(N * p)] <- h[3]^2 * v[1:(N * p)] * rep(1 / h[-(1:3)], p)
  Yp <- rbind(t(pr$Ysoc) / h[1], t(pr$Ysur) / h[2], t(spec$prior$Y))
  Xp <- rbind(t(pr$Xsoc) / h[1], t(pr$Xsur) / h[2], t(spec$prior$X))
  S  <- .Call(niw, Yp, Xp, t(pr$A), diag(v), diag(h[-(1:3)]), pr$nu)[[3]]
  inherits(try(chol(S), silent = TRUE), "try-error")
})
cat("\npackage formula (estimate) not positive definite for", sum(fails), "of", n_keep, "hyperparameter draws\n")

# 3) Cross-check: our draw() vs the package's estimate() where the package works (default hyperparameters)
spec_def <- specify_bsvarSIGN$new(data = dados, p = 12)
h_def <- spec_def$prior$hyper[1:13, 1]
set.seed(1); post_pkg <- estimate(spec_def, S = 200, show_progress = FALSE)
set.seed(2); ours <- lapply(1:2000, function(i) bv$draw(h_def))
sig_pkg  <- apply(post_pkg$posterior$Sigma, c(1, 2), mean)
sig_ours <- Reduce(`+`, lapply(ours, `[[`, "Sigma")) / length(ours)
A_pkg  <- apply(post_pkg$posterior$A, c(1, 2), mean)            # N x K
A_ours <- t(Reduce(`+`, lapply(ours, `[[`, "B")) / length(ours))
cat("\ncross-check at default hyperparameters (package estimate vs our draw):\n")
print(round(rbind(sigma_diag_pkg = diag(sig_pkg), sigma_diag_ours = diag(sig_ours)), 4))
cat("own first-lag coefficient, package:", round(diag(A_pkg[, 1:10]), 3), "\n")
cat("own first-lag coefficient, ours:   ", round(diag(A_ours[, 1:10]), 3), "\n")
cat("max abs difference in mean coefficients:", signif(max(abs(A_pkg - A_ours)), 3), "\n")

# 4) Numerical proof of the forecast() problem (issue #91), using the package's own draw
fp <- forecast(post_pkg, horizon = 3)
A1 <- post_pkg$posterior$A[, , 1]
X_last <- spec_def$data_matrices$X[, ncol(spec_def$data_matrices$X)]   # regressors of y_T
x0 <- c(t(dados[nrow(dados):(nrow(dados) - 11), ]), 1)                 # regressors of y_T+1
y1 <- fp$forecasts[, 1, 1]
cat("\nforecast(): h=1 mean minus fitted y_T (A X_T):", signif(max(abs(fp$forecast_mean[, 1, 1] - A1 %*% X_last)), 3), "\n")
cat("forecast(): h=1 mean minus correct forecast (A x0):", signif(max(abs(fp$forecast_mean[, 1, 1] - A1 %*% x0)), 3), "\n")
cat("forecast(): h=2 mean minus 'replace lag 1 only':", signif(max(abs(fp$forecast_mean[, 2, 1] - A1 %*% c(y1, X_last[11:121]))), 3), "\n")
cat("forecast(): h=2 mean minus 'shift all lags':", signif(max(abs(fp$forecast_mean[, 2, 1] - A1 %*% c(y1, X_last[1:110], 1))), 3), "\n")

# 5) Sanity of our forecast: 1-step median vs last observation
cat("\nlast observation vs our 1-step-ahead median:\n")
print(round(rbind(last = dados[nrow(dados), ], h1_median = apply(fc[1, , ], 1, median)), 2))

# 6) Fan chart
g <- eval(parse(text = paste(code[cut_at:length(code)], collapse = "\n")))
ggsave(paste0(base, "inv/fanchart_check.png"), g, width = 11, height = 7, dpi = 80)
cat("fan chart saved\n")
