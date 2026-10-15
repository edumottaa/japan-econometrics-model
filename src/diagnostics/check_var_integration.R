.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
suppressPackageStartupMessages(library(bsvarSIGNs))
setwd("C:/Users/dudup/OneDrive/NT Macro/work/psi_diagnostic")
spec <- readRDS("covid_reliable_spec.rds")
# The model plan starts with the reduced form, before structural identification.
spec$identification$sign_structural[,] <- NA_real_
set.seed(123)
post <- estimate(spec, S = 1000, show_progress = FALSE)
saveRDS(post, "covid_reliable_post.rds")
stopifnot(all(is.finite(post$posterior$A)), all(is.finite(post$posterior$B)))
cat("VAR INTEGRATION PASSED\n")
cat("Posterior A", dim(post$posterior$A), "B", dim(post$posterior$B), "\n")
