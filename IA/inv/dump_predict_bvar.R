suppressMessages(library(BVAR))
ns <- asNamespace("BVAR")
pat <- "rnorm|rmvn|draw|Phi|sigma|cov|solve|chol|sum"
for (f in c("compute_fcast", "predict.bvar")) {
  d <- deparse(get(f, ns))
  cat("\n=====", f, "(", length(d), "linhas) =====\n")
  hits <- grep(pat, d, value = TRUE)
  cat(head(hits, 40), sep = "\n")
}
