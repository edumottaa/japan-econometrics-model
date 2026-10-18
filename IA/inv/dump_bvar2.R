b <- deparse(BVAR::bvar)
cat(paste(80:130, b[80:130]), sep = "\n")
cat("\n==== bv_metropolis ====\n"); print(BVAR::bv_metropolis)
