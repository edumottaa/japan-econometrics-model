suppressMessages(library(bsvarSIGNs))
ns <- asNamespace("bsvarSIGNs")
base <- "C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/"
m  <- readRDS(paste0(base, "mats.rds"))
nm <- c("mu", "delta", "lambda", colnames(m$ext), colnames(m$int))

lp_of <- function(spec, model) {
  pr <- spec$prior$get_prior()
  pr$B <- t(pr$A); pr$Ysoc <- t(pr$Ysoc); pr$Xsoc <- t(pr$Xsoc)
  pr$Ysur <- t(pr$Ysur); pr$Xsur <- t(pr$Xsur); pr$covid <- 0
  f <- get("_bsvarSIGNs_log_posterior_hyper", ns)
  function(h) .Call(f, matrix(h), model, t(spec$prior$Y), t(spec$prior$X), pr)
}

# 1) Why does the chain jump so far? Look at the proposal scale the package builds
spec <- specify_bsvarSIGN$new(data = m$int, p = 12, foreign = m$ext)
lp  <- lp_of(spec, c(1,1,1,1,0))
init <- spec$prior$hyper[1:13, 1]
res <- optim(init, function(x) -lp(c(x, 1,1,1,0.8)), method = "L-BFGS-B",
             lower = 0, upper = init * 100, hessian = TRUE)
e <- eigen(res$hessian)
W <- e$vectors %*% diag(1 / abs(e$values)) %*% t(e$vectors)
cat("optim convergence code:", res$convergence, "(0 = converged, 1 = hit iteration limit)\n")
cat("Hessian eigenvalues (smallest 4):", signif(tail(sort(abs(e$values), decreasing = TRUE), 4), 3), "\n")
print(data.frame(param = nm, mode = signif(res$par, 4), proposal_sd = signif(sqrt(diag(W)), 4)))

run <- function(label, spec, model, S = 4000, burn = 2000) {
  set.seed(123)
  invisible(capture.output(spec$estimate_hyper(S = S, burn_in = burn)))
  H <- spec$prior$hyper
  lp <- lp_of(spec, model)
  vals <- apply(H, 2, lp)
  cat(sprintf("\n=== %s ===\n draws in the bug region (log posterior > 0): %.1f%%\n", label, 100 * mean(vals > 0)))
  s <- t(apply(H[1:13, ], 1, function(x) signif(c(mean = mean(x), min = min(x), max = max(x)), 4)))
  rownames(s) <- nm
  print(s)
}

# 2) Baseline (what you ran)
run("A. baseline: data in log x 100, all hyperparameters", 
    specify_bsvarSIGN$new(data = m$int, p = 12, foreign = m$ext), c(1,1,1,1,0))

# 3) Workaround: keep psi fixed at the OLS variances (package default before estimation)
run("B. psi fixed (hyper_psi = FALSE)",
    specify_bsvarSIGN$new(data = m$int, p = 12, foreign = m$ext, hyper_psi = FALSE), c(1,1,1,0,0))

# 4) Workaround: data in log units (divide by 100) - the scale the default hyperpriors assume
run("C. data / 100, all hyperparameters",
    specify_bsvarSIGN$new(data = m$int / 100, p = 12, foreign = m$ext / 100), c(1,1,1,1,0))
