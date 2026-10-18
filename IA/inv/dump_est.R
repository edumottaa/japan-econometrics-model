suppressMessages(library(bsvarSIGNs))
ns <- asNamespace("bsvarSIGNs")
for (f in c("estimate.BSVARSIGN", "estimate_par", "importance_sampling")) {
  cat("\n=====", f, "=====\n"); print(get(f, ns))
}
