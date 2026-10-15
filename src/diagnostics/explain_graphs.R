task_root <- "C:/Users/dudup/OneDrive/NT Macro"
task_work <- file.path(task_root, "work", "psi_diagnostic")
h <- readRDS(file.path(task_work, "covid_reliable_hyper.rds"))
diag <- readRDS(file.path(task_work, "covid_reliable_diagnostics.rds"))
cat("CORRECTED iterations", nrow(h), "parameters", ncol(h), "\n")
print(t(apply(h[, 1:3], 2, function(x) c(median = median(x), sd = sd(x),
                                     p025 = quantile(x, .025), p975 = quantile(x, .975)))))
cat("CORRELATION between successive changes\n")
print(cor(h[-1, 1:3] - h[-nrow(h), 1:3]))
cat("ESS\n"); print(diag$ess[1:3])
cat("Retained accepted proposals", diag$acceptance * nrow(h), "\n")
png(file.path(task_root, "Modelagem", "mu_delta_lambda_corrigidos.png"),
    width = 1500, height = 1200, res = 150)
par(mfrow = c(3, 1), mar = c(3, 4, 2, 1))
for (j in 1:3) plot(h[, j], type = "l", col = "#D4008D",
  xlab = "Iteracoes depois do burn-in", ylab = "Valor do hiperparametro",
  main = paste(colnames(h)[j], "- estimacao corrigida, com COVID"))
dev.off()
cat("PLOT SAVED\n")
.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
cat("User library exists", dir.exists("C:/Users/dudup/AppData/Local/R/win-library/4.6"), "\n")
if (requireNamespace("bsvarSIGNs", quietly = TRUE)) {
  cat("PACKAGE", as.character(packageVersion("bsvarSIGNs")), "\n")
  print(names(bsvarSIGNs::specify_prior_bsvarSIGN$public_methods))
  print(formals(bsvarSIGNs::specify_bsvarSIGN$public_methods$initialize)[c("hyper_mu","hyper_delta","hyper_lambda","hyper_psi")])
}
