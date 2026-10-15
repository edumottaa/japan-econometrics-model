.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
suppressPackageStartupMessages(library(bsvarSIGNs))
setwd("C:/Users/dudup/OneDrive/NT Macro/work/psi_diagnostic")
scenario <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "baseline"
d <- readRDS("data_bundle.rds")
spec <- readRDS(paste0(scenario, "_spec.rds"))
o <- readRDS(paste0(scenario, "_optim.rds"))
initial <- spec$prior$hyper[, 1, drop = FALSE]
model <- c(TRUE, TRUE, TRUE, TRUE, scenario == "covid")
sym <- function(n) get(n, envir = asNamespace("bsvarSIGNs"))
mode <- .Call(sym("_bsvarSIGNs_extend_hyper"), initial, model, matrix(o$result$par))
e <- eigen(o$result$hessian, symmetric = TRUE)
W_raw <- e$vectors %*% diag(1/abs(e$values)) %*% t(e$vectors)
D <- diag(as.vector(1/o$result$par))
W_log <- D %*% W_raw %*% D
prior <- spec$prior$get_prior()
prior$B <- t(prior$A)
prior$Ysoc <- t(prior$Ysoc)
prior$Xsoc <- t(prior$Xsoc)
prior$Ysur <- t(prior$Ysur)
prior$Xsur <- t(prior$Xsur)
cat("SCENARIO", scenario, "corrected covariance scale\n")
cat("PROPOSAL standard deviations in log space\n")
print(sqrt(diag(W_log)))
set.seed(123)
timing <- system.time({
  draws <- .Call(sym("_bsvarSIGNs_sample_hyper"), 15000L, 5000L, as.vector(mode),
    as.numeric(model), t(spec$prior$Y), t(spec$prior$X), W_log, prior)
})
print(timing)
spec$prior$hyper <- draws[, -(1:5000), drop = FALSE]
h <- t(spec$prior$hyper)
colnames(h) <- c("mu", "delta", "lambda", paste0("psi_", colnames(d$Y)), paste0("covid", 1:4))
saveRDS(spec, paste0(scenario, "_covariance_corrected_spec.rds"))
saveRDS(h, paste0(scenario, "_covariance_corrected_hyper.rds"))
cat("RETAINED transitions", sum(rowSums(abs(h[-1, ] - h[-nrow(h), ])) > 0), "out of", nrow(h) - 1, "\n")
cat("RETAINED unique joint states", nrow(unique(h)), "\n")
s <- t(apply(h, 2, function(x) c(median = median(x), p025 = quantile(x, .025),
                               p975 = quantile(x, .975), min = min(x), max = max(x))))
print(s)
write.csv(data.frame(parameter = rownames(s), s, row.names = NULL),
  paste0(scenario, "_covariance_corrected_summary.csv"), row.names = FALSE)
if (requireNamespace("coda", quietly = TRUE)) {
  cat("EFFECTIVE sample sizes\n")
  print(coda::effectiveSize(h[, if (scenario == "covid") 1:17 else 1:13]))
}
png(paste0(scenario, "_covariance_corrected_traces.png"), width = 1800, height = 1200, res = 150)
par(mfrow = c(5, 2), mar = c(2, 4, 2, 1))
for (i in 4:13) plot(h[, i], type = "l", main = colnames(h)[i], xlab = "", ylab = "psi")
dev.off()
cat("DONE covariance comparison\n")
