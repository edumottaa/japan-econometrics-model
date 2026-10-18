suppressMessages(library(bsvarSIGNs))
ns <- asNamespace("bsvarSIGNs")
m  <- readRDS("C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/mats.rds")

# Same steps as spec$estimate_hyper, but stopping at the mode (optim), so we can inspect it
find_mode <- function(spec) {
  self <- spec; private <- spec$.__enclos_env__$private
  model = c(private$hyper_mu, private$hyper_delta, private$hyper_lambda,
            private$hyper_psi, !is.null(private$hyper_covid))
  hyper = matrix(self$prior$hyper[, ncol(self$prior$hyper)])
  init  = .Call(`_bsvarSIGNs_narrow_hyper`, model, hyper)
  self$prior$covid = private$hyper_covid
  prior = self$prior$get_prior()
  prior$B = t(prior$A); prior$Ysoc = t(prior$Ysoc); prior$Xsoc = t(prior$Xsoc)
  prior$Ysur = t(prior$Ysur); prior$Xsur = t(prior$Xsur)
  lb = rep(0, length(init)); ub = init * 100
  lp = function(x) .Call(`_bsvarSIGNs_log_posterior_hyper`,
                         .Call(`_bsvarSIGNs_extend_hyper`, hyper, model, matrix(x)),
                         model, t(self$prior$Y), t(self$prior$X), prior)
  res = stats::optim(init, function(x) -lp(x), method = "L-BFGS-B", lower = lb, upper = ub)
  list(init = as.vector(init), mode = res$par, value = -res$value, conv = res$convergence,
       msg = res$message, lp = lp, model = model, hyper = hyper, prior = prior)
}
environment(find_mode) <- ns

nm <- c("mu", "delta", "lambda", colnames(m$ext), colnames(m$int))
spec <- specify_bsvarSIGN$new(data = m$int, p = 12, foreign = m$ext)
r <- find_mode(spec)
cat("optim convergence:", r$conv, r$msg, "\n")
print(data.frame(param = nm, init = signif(r$init, 4), mode = signif(r$mode, 4),
                 ratio = signif(r$mode / r$init, 4)))
saveRDS(r[c("init", "mode")], "C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/mode_base.rds")

# Profile: log posterior as a function of psi_brent, everything else at the mode
cat("\nProfile of log posterior over psi_brent (others at mode):\n")
for (v in c(1e-8, 1e-6, 1e-4, 1e-2, 1, 10, 50, 94, 200, 1000)) {
  x <- r$mode; x[6] <- v
  cat(sprintf("psi_brent = %-8g  logpost = %.3f\n", v, r$lp(x)))
}
