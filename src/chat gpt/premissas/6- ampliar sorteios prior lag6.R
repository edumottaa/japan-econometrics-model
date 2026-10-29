# Guardar propostas e pesos nativos antes de reamostrar; mesma especificacao.
Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(TZ = "America/Sao_Paulo")
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
library(bsvarSIGNs)
library(jsonlite)
args <- commandArgs(trailingOnly = TRUE)
lote <- if (length(args)) as.integer(args[1]) else 1L
stopifnot(lote > 0L, packageVersion("bsvarSIGNs") == "3.0")
pasta <- "data/processed/premissas_inflacao_boj/lag6"
arquivo <- file.path(pasta, paste0("propostas_", lote, ".rds"))
if (file.exists(arquivo)) quit(status = 0)
ajuste <- readRDS(file.path(pasta, "modelo_tendencia.rds"))
spec <- ajuste$post$last_draw$clone(deep = TRUE)
stopifnot(spec$p == 6L, nrow(ajuste$dados_modelo) == 320L)

# Mesma funcao instalada, retornando qqq antes do importance_sampling.
amostrar <- get("estimate.BSVARSIGN", asNamespace("bsvarSIGNs"))
expressoes <- as.list(body(amostrar))
fim <- head(which(vapply(expressoes, function(x) is.call(x) &&
  as.character(x[[1]]) %in% c("=", "<-") && identical(x[[2]], as.name("output")), logical(1)))
  , 1L)
stopifnot(length(fim) == 1L, grepl("importance_sampling", paste(deparse(body(amostrar)), collapse = " ")))
body(amostrar) <- as.call(c(expressoes[seq_len(fim - 1L)], list(quote(qqq))))
set.seed(600L + lote)
raw <- amostrar(spec, S = 2500L, show_progress = TRUE)$posterior
stopifnot(length(raw$w) == 2500L, all(is.finite(raw$w)), all(raw$w >= 0), sum(raw$w) > 0,
  all(is.finite(raw$A)), all(is.finite(raw$Sigma)))
saveRDS(list(propostas = raw, lote = lote, semente = 600L + lote,
  configuracao = ajuste$configuracao, dados_modelo = ajuste$dados_modelo), arquivo)
write_json(list(estado = "concluido", lote = lote, propostas = length(raw$w),
  ess_nativo = sum(raw$w)^2 / sum(raw$w^2)), file.path(pasta, paste0("status_lote", lote, ".json")),
  pretty = TRUE, auto_unbox = TRUE)
