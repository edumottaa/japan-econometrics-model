.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
suppressPackageStartupMessages(library(bsvarSIGNs))
setwd("C:/Users/dudup/OneDrive/NT Macro/work/psi_diagnostic")
source("stable_likelihood.R")
for (scenario in c("baseline", "covid")) {
  spec <- readRDS(paste0(scenario, "_spec.rds"))
  o <- readRDS(paste0(scenario, "_optim.rds"))
  p <- spec$prior$get_prior()
  p$B <- t(p$A); p$Ysoc <- t(p$Ysoc); p$Xsoc <- t(p$Xsoc)
  p$Ysur <- t(p$Ysur); p$Xsur <- t(p$Xsur)
  target <- stable_hyper_target(spec, scenario == "covid")
  native <- get("_bsvarSIGNs_log_posterior_hyper", asNamespace("bsvarSIGNs"))
  h <- c(as.vector(o$result$par), if (scenario == "baseline") c(1, 1, 1, .8))
  cat("SCENARIO", scenario, "\n")
  set.seed(123)
  for (i in 1:4) {
    htest <- h
    htest[1:13] <- h[1:13]*exp(rnorm(13, sd = .1))
    a <- target(htest)
    b <- .Call(native, htest, as.numeric(c(TRUE,TRUE,TRUE,TRUE,scenario == "covid")),
               t(spec$prior$Y), t(spec$prior$X), p)
    cat("stable", a, "native", b, "difference", a-b, "\n")
    stopifnot(abs(a-b) < .001)
  }
  extreme <- readRDS(paste0(scenario, "_reliable_optim.rds"))
  hbad <- exp(extreme$par[1:13])
  if (scenario == "covid") hbad <- c(hbad, 1 + exp(extreme$par[14:16]), plogis(extreme$par[17]))
  else hbad <- c(hbad, 1, 1, 1, .8)
  cat("previous invalid optimum stable", target(hbad), "\n")
}
cat("LIKELIHOOD MATCH PASSED\n")
