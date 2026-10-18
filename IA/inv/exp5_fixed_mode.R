suppressMessages(library(bsvarSIGNs))
ns <- asNamespace("bsvarSIGNs")
base <- "C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/"
m  <- readRDS(paste0(base, "mats.rds"))
nm <- c("mu", "delta", "lambda", colnames(m$ext), colnames(m$int))
N <- 10; p <- 12
routines <- names(getDLLRegisteredRoutines("bsvarSIGNs")$.Call)
cat("log_prior_hyper registered:", "_bsvarSIGNs_log_prior_hyper" %in% routines, "\n")

spec <- specify_bsvarSIGN$new(data = m$int, p = 12, foreign = m$ext)
pr <- spec$prior$get_prior()
pr$B <- t(pr$A); pr$Ysoc <- t(pr$Ysoc); pr$Xsoc <- t(pr$Xsoc)
pr$Ysur <- t(pr$Ysur); pr$Xsur <- t(pr$Xsur); pr$covid <- 0
Y <- t(spec$prior$Y); X <- t(spec$prior$X)
f_ml <- get("_bsvarSIGNs_log_ml", ns)
f_pr <- get("_bsvarSIGNs_log_prior_hyper", ns)

# Corrected log posterior: same formula as the package, but a numerical failure in
# EITHER marginal likelihood is treated as "forbidden" (-1e10) instead of flipping sign.
lp_ok <- function(h) {
  if (any(h <= 0)) return(-1e10)
  v <- diag(pr$V); v[1:(N*p)] <- h[3]^2 * v[1:(N*p)] * rep(1/h[4:13], p)
  Ys <- rbind(pr$Ysoc / h[1], pr$Ysur / h[2]); Xs <- rbind(pr$Xsoc / h[1], pr$Xsur / h[2])
  plus <- .Call(f_ml, pr$B, diag(v), diag(h[4:13]), pr$nu, rbind(Ys, Y), rbind(Xs, X))
  star <- .Call(f_ml, pr$B, diag(v), diag(h[4:13]), pr$nu, Ys, Xs)
  if (plus <= -1e10 + 1 || star <= -1e10 + 1) return(-1e10)
  out <- .Call(f_pr, matrix(c(h, 1,1,1,0.8)), c(1,1,1,1,0), pr) + plus - star
  if (!is.finite(out)) -1e10 else out
}

init <- spec$prior$hyper[1:13, 1]
t_eval <- system.time(for (i in 1:20) lp_ok(init))["elapsed"] / 20
cat("seconds per log-posterior evaluation:", round(t_eval, 4), "\n")

res <- optim(log(init), function(z) -lp_ok(exp(z)), method = "BFGS",
             control = list(maxit = 2000, reltol = 1e-10))
mode <- exp(res$par)
cat("optim convergence:", res$convergence, " log posterior at mode:", -res$value, "\n")
print(data.frame(param = nm, init = signif(init, 4), mode = signif(mode, 4)))
H <- optimHess(res$par, function(z) -lp_ok(exp(z)))
cat("Hessian (log scale) positive definite:", all(eigen(H)$values > 0), "\n")
saveRDS(list(mode = mode, H = H), paste0(base, "mode_valid.rds"))

spec_mode <- specify_bsvarSIGN$new(data = m$int, p = 12, foreign = m$ext)
spec_mode$prior$hyper <- matrix(c(mode, 1, 1, 1, 0.8), ncol = 1)
set.seed(123)
tt <- system.time(invisible(capture.output(post_mode <- estimate(spec_mode, S = 50, show_progress = FALSE))))
cat("\nestimate() S = 50 with hyper at the valid mode:", round(tt["elapsed"], 1), "s\n")
cat("any NA in posterior draws:", anyNA(post_mode$posterior$B), "\n")
