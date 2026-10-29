# Reunir lotes independentes antes de aplicar os pesos do prior.
Sys.setlocale("LC_CTYPE", "English_United States.utf8")
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
library(bsvarSIGNs)
library(jsonlite)
pasta <- "data/processed/premissas_inflacao_boj/lag6"
arquivos <- list.files(pasta, "^propostas_[0-9]+[.]rds$", full.names = TRUE)
stopifnot(length(arquivos) > 0L)
lotes <- lapply(arquivos, readRDS)
ajuste <- readRDS(file.path(pasta, "modelo_tendencia.rds"))
stopifnot(all(vapply(lotes, function(x) identical(x$configuracao, ajuste$configuracao) &&
  identical(x$dados_modelo, ajuste$dados_modelo), logical(1))),
  !anyDuplicated(vapply(lotes, function(x) x$semente, integer(1))))
reunir <- function(xs) {
  d <- dim(xs[[1]])
  if (is.null(d)) return(unlist(xs, use.names = FALSE))
  stopifnot(all(vapply(xs, function(x) identical(head(dim(x), -1L), head(d, -1L)), logical(1))))
  array(unlist(xs, use.names = FALSE), c(head(d, -1L), sum(vapply(xs, function(x) tail(dim(x), 1L), integer(1)))))
}
post <- setNames(lapply(names(lotes[[1]]$propostas), function(nome)
  reunir(lapply(lotes, function(x) x$propostas[[nome]]))), names(lotes[[1]]$propostas))
validas <- which(is.finite(post$w) & post$w > 0)
stopifnot(length(validas) >= 50L)
pesos <- post$w[validas]
post$w <- NULL
for (nome in names(post)) {
  x <- post[[nome]]
  if (length(dim(x)) == 3L) post[[nome]] <- x[, , validas, drop = FALSE]
  if (length(dim(x)) == 2L) post[[nome]] <- x[, validas, drop = FALSE]
}
post$ess <- sum(pesos)^2 / sum(pesos^2)
ajuste$post <- ajuste$post$clone(deep = TRUE)
ajuste$post$posterior <- post
ajuste$pesos_propostas <- pesos / sum(pesos)
ajuste$lotes_propostas <- arquivos
saveRDS(ajuste, file.path(pasta, "modelo_propostas.rds"))
write_json(list(lotes = length(lotes), propostas = length(pesos), ess_nativo = post$ess),
  file.path(pasta, "diagnostico_propostas.json"), auto_unbox = TRUE, pretty = TRUE)
print(c(lotes = length(lotes), propostas = length(pesos), ess_nativo = post$ess))
