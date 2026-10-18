cat("BVAR installed:", requireNamespace("BVAR", quietly = TRUE), "\n")
if (requireNamespace("BVAR", quietly = TRUE)) {
  cat("BVAR version:", as.character(packageVersion("BVAR")), "\n")
  print(args(BVAR::bv_priors)); print(args(BVAR::bv_psi)); print(args(BVAR::bv_metropolis))
  b <- deparse(BVAR::bv_priors); cat(grep("hyper|full|auto|psi", b, value = TRUE), sep = "\n")
}
