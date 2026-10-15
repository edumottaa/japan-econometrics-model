.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
suppressPackageStartupMessages(library(bsvarSIGNs))
setwd("C:/Users/dudup/OneDrive/NT Macro/work/psi_diagnostic")
h <- readRDS("baseline_hyper.rds")
o <- readRDS("baseline_optim.rds")
print(data.frame(parameter = colnames(h)[1:13], initial = o$initial,
                 optimized = o$result$par, upper_bound = o$initial * 100), row.names = FALSE)
print(o$result[c("convergence", "message", "counts", "value")])
e <- eigen(o$result$hessian, symmetric = TRUE)
W <- e$vectors %*% diag(1/abs(e$values)) %*% t(e$vectors)
print(data.frame(parameter = colnames(h)[1:13], proposal_sd = sqrt(diag(W)),
                 optimized = o$result$par,
                 proposal_sd_over_level = sqrt(diag(W)) / o$result$par), row.names = FALSE)
changes <- which(rowSums(abs(h[-1, ] - h[-nrow(h), ])) > 0)
cat("RETAINED transitions", length(changes), "out of", nrow(h) - 1, "\n")
cat("RETAINED unique joint states", nrow(unique(h)), "\n")
print(changes)
cat("INVERSE GAMMA HELPERS\n")
print(get("igamma_shape", asNamespace("bsvarSIGNs")))
print(get("igamma_scale", asNamespace("bsvarSIGNs")))
cat("shape", get("igamma_shape", asNamespace("bsvarSIGNs"))(.02^2, .02^2),
    "scale", get("igamma_scale", asNamespace("bsvarSIGNs"))(.02^2, .02^2), "\n")
