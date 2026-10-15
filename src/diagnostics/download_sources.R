setwd("C:/Users/dudup/OneDrive/NT Macro/work/psi_diagnostic")
for (f in c("bsvars_sign.cpp")) {
  download.file(paste0("https://raw.githubusercontent.com/bsvars/bsvarSIGNs/main/src/", f),
                paste0("source_", f), mode = "wb", quiet = TRUE)
  cat("SAVED", f, file.info(paste0("source_", f))$size, "bytes\n")
}
