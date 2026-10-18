suppressMessages(library(BVAR))
m <- readRDS("C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/mats.rds")
dados <- cbind(m$ext, m$int)

priors <- bv_priors(hyper = "auto",
                    mn  = bv_minnesota(lambda = bv_lambda(), alpha = bv_alpha()),
                    soc = bv_soc(), sur = bv_sur())
mh <- bv_metropolis(scale_hess = c(0.05, 0.0001, 0.0001),
                    adjust_acc = TRUE, acc_lower = 0.25, acc_upper = 0.45)
set.seed(123)
tt <- system.time(run <- suppressMessages(bvar(dados, lags = 12, n_draw = 15000, n_burn = 5000,
                                               priors = priors, mh = mh, verbose = FALSE)))
cat("seconds:", round(tt["elapsed"]), "| acceptance rate (after burn-in):",
    round(run$meta$accepted / (run$meta$n_draw - run$meta$n_burn), 3), "\n")
H <- run$hyper; half <- nrow(H) / 2
print(signif(data.frame(mean = colMeans(H), q05 = apply(H, 2, quantile, .05), q95 = apply(H, 2, quantile, .95),
                        mean_1st = colMeans(H[1:half, ]), mean_2nd = colMeans(H[-(1:half), ]),
                        distinct = apply(H, 2, function(x) length(unique(x)))), 4))
if (requireNamespace("coda", quietly = TRUE)) print(round(coda::effectiveSize(coda::mcmc(H))))
cat("mode from optimisation:", signif(run$optim$par, 4), "\n")
fc <- predict(run, horizon = 24, conf_bands = c(0.05, 0.16))
cat("forecast dims:", dim(fc$fcast), "| NA:", anyNA(fc$fcast), "\n")
