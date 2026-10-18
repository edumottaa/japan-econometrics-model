suppressMessages({library(BVAR); library(bsvarSIGNs)})
m <- readRDS("C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/mats.rds")

# (a) where does the bsvarSIGNs forecast start from?
f <- deparse(get("forecast.PosteriorBSVARSIGN", asNamespace("bsvarSIGNs")))
cat("--- bsvarSIGNs forecast: origin ---\n"); cat(grep("X_T|T =|ncol", f, value = TRUE), sep = "\n")

# (b) BVAR with the plan's hierarchical prior: lambda, psi, mu (soc), delta (sur)
dados <- cbind(m$ext, m$int)
priors <- bv_priors(hyper = c("lambda", "psi", "soc", "sur"),
                    mn = bv_minnesota(lambda = bv_lambda(), alpha = bv_alpha(), psi = bv_psi()),
                    soc = bv_soc(), sur = bv_sur())
mh <- bv_metropolis(adjust_acc = TRUE, acc_lower = 0.25, acc_upper = 0.45)
set.seed(123)
tt <- system.time(run <- bvar(dados, lags = 12, n_draw = 15000, n_burn = 5000,
                              priors = priors, mh = mh, verbose = FALSE))
cat("\n--- BVAR ---\nseconds:", round(tt["elapsed"]), "\n")
print(run)
H <- run$hyper
half <- nrow(H) / 2
st <- data.frame(mean = colMeans(H), q05 = apply(H, 2, quantile, .05), q95 = apply(H, 2, quantile, .95),
                 mean_1st = colMeans(H[1:half, ]), mean_2nd = colMeans(H[-(1:half), ]),
                 distinct = apply(H, 2, function(x) length(unique(x))))
print(signif(st, 4))
if (requireNamespace("coda", quietly = TRUE)) {
  cat("effective sample size (coda):\n"); print(round(coda::effectiveSize(coda::mcmc(H))))
}
tf <- system.time(fc <- predict(run, horizon = 24, conf_bands = c(0.05, 0.16)))
cat("\npredict 24 months:", round(tf["elapsed"], 1), "s | dims:", dim(fc$fcast), "| any NA:", anyNA(fc$fcast), "\n")
saveRDS(list(hyper = H), "C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/bvar_hyper.rds")
