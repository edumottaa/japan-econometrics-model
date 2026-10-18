suppressMessages(library(bsvarSIGNs))
ns <- asNamespace("bsvarSIGNs")
base <- "C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/"
m  <- readRDS(paste0(base, "mats.rds"))
nm <- c("mu", "delta", "lambda", colnames(m$ext), colnames(m$int))
N <- 10; p <- 12

spec <- specify_bsvarSIGN$new(data = m$int, p = 12, foreign = m$ext)
pr <- spec$prior$get_prior()
pr$B <- t(pr$A); pr$Ysoc <- t(pr$Ysoc); pr$Xsoc <- t(pr$Xsoc)
pr$Ysur <- t(pr$Ysur); pr$Xsur <- t(pr$Xsur); pr$covid <- 0
Y <- t(spec$prior$Y); X <- t(spec$prior$X)
lp <- function(h) .Call(get("_bsvarSIGNs_log_posterior_hyper", ns), matrix(c(h, 1,1,1,0.8)),
                        c(1,1,1,1,0), Y, X, pr)
star <- function(h) {
  v <- diag(pr$V); v[1:(N*p)] <- h[3]^2 * v[1:(N*p)] * rep(1/h[4:13], p)
  .Call(get("_bsvarSIGNs_log_ml", ns), pr$B, diag(v), diag(h[4:13]), pr$nu,
        rbind(pr$Ysoc / h[1], pr$Ysur / h[2]), rbind(pr$Xsoc / h[1], pr$Xsur / h[2]))
}

# 1) Mode with optim run to convergence (package stops at 100 iterations)
init <- spec$prior$hyper[1:13, 1]
res <- optim(init, function(x) -lp(x), method = "L-BFGS-B", lower = 1e-6, upper = init * 100,
             control = list(maxit = 2000))
cat("optim convergence:", res$convergence, res$message, " log posterior at mode:", -res$value, "\n")
cat("log_ml_star at mode (must NOT be -1e10):", star(res$par), "\n")
print(data.frame(param = nm, init = signif(init, 4), mode = signif(res$par, 4)))
saveRDS(res$par, paste0(base, "mode_converged.rds"))

# 2) Speed: estimate() with hyperparameters fixed at the mode vs the trapped chain
spec_mode <- specify_bsvarSIGN$new(data = m$int, p = 12, foreign = m$ext)
spec_mode$prior$hyper <- matrix(c(res$par, 1, 1, 1, 0.8), ncol = 1)
set.seed(123)
t_mode <- system.time(invisible(capture.output(post_mode <- estimate(spec_mode, S = 50, show_progress = FALSE))))
cat("\nestimate() S=50 with hyper at the mode:", round(t_mode["elapsed"], 1), "s\n")

spec_trap <- specify_bsvarSIGN$new(data = m$int, p = 12, foreign = m$ext)
set.seed(123)
invisible(capture.output(spec_trap$estimate_hyper(S = 4000, burn_in = 2000)))
set.seed(123)
t_trap <- system.time(invisible(capture.output(post_trap <- estimate(spec_trap, S = 50, show_progress = FALSE))))
cat("estimate() S=50 with the trapped hyper chain:", round(t_trap["elapsed"], 1), "s\n")
