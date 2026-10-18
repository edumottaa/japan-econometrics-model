# Diagnostic estimator: same bsvarSIGNs likelihood and prior, with parameter
# transformations applied consistently to optimization and Metropolis proposals.

.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
suppressPackageStartupMessages(library(bsvarSIGNs))
library(coda)

setwd("C:/Users/dudup/OneDrive/NT Macro/work/psi_diagnostic")
source("stable_likelihood.R")
scenario <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "baseline"
d <- readRDS("data_bundle.rds")
spec <- readRDS(paste0(scenario, "_spec.rds"))
o <- readRDS(paste0(scenario, "_optim.rds"))
covid <- scenario == "covid"
N <- ncol(d$Y)
n_positive <- N + 3L
n <- n_positive + if (covid) 4L else 0L
model <- as.numeric(c(TRUE, TRUE, TRUE, TRUE, covid))
prior <- spec$prior$get_prior()
prior$B <- t(prior$A)
prior$Ysoc <- t(prior$Ysoc)
prior$Xsoc <- t(prior$Xsoc)
prior$Ysur <- t(prior$Ysur)
prior$Xsur <- t(prior$Xsur)
Y <- t(spec$prior$Y)
X <- t(spec$prior$X)
native <- get("_bsvarSIGNs_log_posterior_hyper", envir = asNamespace("bsvarSIGNs"))
stable_posterior <- stable_hyper_target(spec, covid)
fixed_full <- c(rep(NA_real_, n_positive), 1, 1, 1, .8)
forward <- function(z) {
  full <- fixed_full
  full[1:n_positive] <- exp(z[1:n_positive])
  if (covid) {
    full[(n_positive + 1):(n_positive + 3)] <- 1 + exp(z[(n_positive + 1):(n_positive + 3)])
    full[n_positive + 4] <- plogis(z[n_positive + 4])
  }
  full
}
inverse <- function(h) {
  z <- log(h[1:n_positive])
  if (covid) z <- c(z, log(pmax(h[(n_positive + 1):(n_positive + 3)] - 1, 1e-5)),
                   qlogis(h[n_positive + 4]))
  z
}
log_target <- function(z) {
  full <- forward(z)
  if (any(!is.finite(full)) || any(full <= 0)) return(-1e10)
  lp <- stable_posterior(full)
  if (!is.finite(lp)) return(-1e10)
  jac <- sum(z[1:n_positive])
  if (covid) {
    rho <- full[n_positive + 4]
    jac <- jac + sum(z[(n_positive + 1):(n_positive + 3)]) + log(rho) + log1p(-rho)
  }
  if (!is.finite(jac)) return(-1e10)
  lp + jac
}
cat("SCENARIO", scenario, "optimization in transformed parameters\n")
set.seed(123)
opt <- optim(inverse(as.vector(o$result$par)), function(z) -log_target(z), method = "BFGS",
             control = list(maxit = 1000, reltol = 1e-9), hessian = TRUE)
ev <- eigen(opt$hessian, symmetric = TRUE)
cat("OPTIM convergence", opt$convergence, "value", opt$value, "counts", opt$counts, "\n")
cat("HESSIAN eigenvalue range", range(ev$values), "\n")
print(data.frame(parameter = c("mu", "delta", "lambda", paste0("psi_", colnames(d$Y)),
    if (covid) paste0("covid", 1:4)), optimized = forward(opt$par)[1:n]), row.names = FALSE)
saveRDS(opt, paste0(scenario, "_reliable_optim.rds"))
if (opt$convergence != 0 || any(ev$values <= 0)) stop("Transformed optimization or curvature check failed.")
W <- ev$vectors %*% diag(1/ev$values) %*% t(ev$vectors)
z <- opt$par
zbar <- z
current_lp <- log_target(z)
log_scale <- log(2.38/sqrt(n))
S <- 15000L
burn <- 5000L
draws <- matrix(NA_real_, S, length(fixed_full))
draws[1, ] <- forward(z)
accepted <- logical(S)
L <- t(chol(W))
cat("SAMPLING 15000 iterations, burn-in 5000\n")
flush.console()
timing <- system.time({
  for (t in 2:S) {
    proposal <- z + exp(log_scale) * as.vector(L %*% rnorm(n))
    proposal_lp <- log_target(proposal)
    log_a <- min(0, proposal_lp - current_lp)
    a <- exp(log_a)
    if (log(runif(1)) < log_a) {
      z <- proposal
      current_lp <- proposal_lp
      accepted[t] <- TRUE
    }
    draws[t, ] <- forward(z)
    if (t <= burn) {
      delta_z <- z - zbar
      zbar <- zbar + delta_z/t
      W <- W*(t-1)/t + tcrossprod(delta_z)*(t-1)/(t*t)
      log_scale <- log_scale + t^(-.6) * (a - .234)
      if (t %% 25L == 0L || t == burn) L <- t(chol(W + diag(1e-8, n)))
    }
    if (t %% 3000L == 0L) { cat("ITERATION", t, "\n"); flush.console() }
  }
})
print(timing)
h <- draws[-(1:burn), , drop = FALSE]
colnames(h) <- c("mu", "delta", "lambda", paste0("psi_", colnames(d$Y)), paste0("covid", 1:4))
spec$prior$hyper <- t(h)
saveRDS(spec, paste0(scenario, "_reliable_spec.rds"))
saveRDS(h, paste0(scenario, "_reliable_hyper.rds"))
acceptance <- mean(accepted[-(1:burn)])
cat("RETAINED acceptance", acceptance, "unique states", nrow(unique(h)), "\n")
s <- t(apply(h, 2, function(x) c(median = median(x), p025 = quantile(x, .025),
    p975 = quantile(x, .975), min = min(x), max = max(x))))
print(s)
ess <- effectiveSize(h[, 1:n])
cat("ESS\n"); print(ess)
write.csv(data.frame(parameter = rownames(s), s, row.names = NULL),
          paste0(scenario, "_reliable_summary.csv"), row.names = FALSE)
write.csv(data.frame(parameter = names(ess), effective_sample_size = as.vector(ess)),
          paste0(scenario, "_reliable_ess.csv"), row.names = FALSE)
saveRDS(list(acceptance = acceptance, unique_states = nrow(unique(h)), ess = ess,
             optimizer_convergence = opt$convergence), paste0(scenario, "_reliable_diagnostics.rds"))
png(paste0(scenario, "_reliable_traces.png"), width = 1800, height = 1200, res = 150)
par(mfrow = c(5, 2), mar = c(2, 4, 2, 1))
for (i in 4:13) plot(h[, i], type = "l", main = colnames(h)[i], xlab = "", ylab = "psi")
dev.off()
cat("DONE reliable diagnostic\n")
