p <- "C:/Users/dudup/OneDrive/NT Macro/model_psi_corrigido.R"
exprs <- parse(p)
local_env <- new.env(parent = baseenv())
expected <- c("stable_hyper_target", "estimar_hyper_corrigido")
found <- character()
for (e in exprs) {
  if (!is.call(e)) next
  stopifnot(!identical(e[[1]], as.name("source")))
  if (identical(e[[1]], as.name("<-")) && is.symbol(e[[2]]) &&
      as.character(e[[2]]) %in% expected) {
    eval(e, envir = local_env)
    found <- c(found, as.character(e[[2]]))
  }
}
stopifnot(setequal(found, expected), all(vapply(expected, function(n)
  is.function(get(n, envir = local_env, inherits = FALSE)), logical(1))))
cat("VALIDADO: sintaxe correta, duas funcoes definidas no proprio arquivo, sem source().\n")
