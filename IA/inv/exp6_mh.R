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


# Random-walk Metropolis on log(hyper) with the corrected log posterior
mv <- readRDS(paste0(base, "mode_valid.rds"))
L  <- t(chol(solve(mv$H))) * 2.38 / sqrt(13)
target <- function(z) lp_ok(exp(z)) + sum(z)   # + Jacobian of the log transform
set.seed(123)
S <- 15000; burn <- 5000
z <- log(mv$mode); cur <- target(z); draws <- matrix(NA, 13, S); acc <- 0
t_mh <- system.time(for (s in 1:S) {
  prop <- z + as.vector(L %*% rnorm(13))
  val  <- target(prop)
  if (log(runif(1)) < val - cur) { z <- prop; cur <- val; acc <- acc + 1 }
  draws[, s] <- exp(z)
})["elapsed"]
kept <- draws[, -(1:burn)]
cat("time:", round(t_mh), "s  acceptance rate:", round(acc / S, 3), "\n")
cat("forbidden draws kept:", sum(apply(kept, 2, lp_ok) <= -1e10 + 1), "\n")
half <- ncol(kept) / 2
print(data.frame(param = nm,
                 mean_1st_half = signif(rowMeans(kept[, 1:half]), 4),
                 mean_2nd_half = signif(rowMeans(kept[, -(1:half)]), 4),
                 q05 = signif(apply(kept, 1, quantile, 0.05), 4),
                 q95 = signif(apply(kept, 1, quantile, 0.95), 4)))
saveRDS(kept, paste0(base, "hyper_draws_valid.rds"))
