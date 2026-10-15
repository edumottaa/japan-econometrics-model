.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
suppressPackageStartupMessages(library(bsvarSIGNs))
source("C:/Users/dudup/OneDrive/NT Macro/Modelagem/corrigir_psi.R")
setwd("C:/Users/dudup/OneDrive/NT Macro/work/psi_diagnostic")
d <- readRDS("data_bundle.rds")
spec <- specify_bsvarSIGN$new(data = d$matrix_interna, p = 12, foreign = d$matrix_externa,
                             hyper_covid = 231L)
set.seed(123)
estimar_hyper_corrigido(spec, S = 1000L, burn_in = 500L, inicio_covid = 231L)
stopifnot(identical(dim(spec$prior$hyper), c(17L, 500L)), all(is.finite(spec$prior$hyper)),
          nrow(unique(t(spec$prior$hyper))) > 50L)
cat("STANDALONE HELPER INTEGRATION PASSED\n")
