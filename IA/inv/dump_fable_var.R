suppressMessages(library(fable))
ns <- asNamespace("fable")
nm <- ls(ns, all.names = TRUE)
cat("funcoes do fable ligadas a VAR:\n"); print(nm[grepl("VAR|var_", nm)])
pat <- "rnorm|rmvn|mvnorm|sim|Phi|cov|sigma|vars::|chol|predict"
for (f in nm[grepl("VAR", nm) & grepl("forecast|fc|predict|generate|sim", nm)]) {
  g <- get(f, ns)
  if (!is.function(g)) next
  d <- deparse(g)
  cat("\n=====", f, "(", length(d), "linhas) =====\n")
  cat(head(grep(pat, d, value = TRUE), 25), sep = "\n")
}
