# Reutilizar o prior de crescimento de tendência do experimento anterior.
source("src/chat gpt/premissas/5- amostrar prior tendencia.R", local = TRUE, encoding = "UTF-8")

aplicar_prior_longo_prazo <- function(post, centro = 2, desvio = .5, S = 2500L) {
  stopifnot(centro > -100, desvio > 0, S >= 50L)
  p <- post$last_draw$p
  N <- nrow(post$posterior$A)
  stopifnot(N == 7L, dim(post$posterior$A)[2] == N * p + 2L)
  w <- as.numeric(post$posterior$w)
  stopifnot(length(w) == dim(post$posterior$A)[3], all(is.finite(w)), all(w >= 0))
  post$posterior$w <- NULL
  validas <- which(w > 0)
  if (length(validas) < 50L) return(list(post = NULL, ess = 0))
  for (nome in names(post$posterior)) {
    x <- post$posterior[[nome]]
    if (length(dim(x)) == 3L) post$posterior[[nome]] <- x[, , validas, drop = FALSE]
    if (length(dim(x)) == 2L) post$posterior[[nome]] <- x[, validas, drop = FALSE]
  }
  w <- w[validas]
  n <- length(w)
  lags <- unlist(lapply(0:(p - 1L), function(j) 3:N + N * j))
  estaveis <- vapply(seq_len(n), function(s) {
    A <- post$posterior$A[, , s]
    companheira <- rbind(A[3:N, lags], cbind(diag(5 * (p - 1L)), matrix(0, 5 * (p - 1L), 5)))
    soma <- Reduce(`+`, lapply(seq_len(p), function(j) A[3:N, N * (j - 1L) + 3:N]))
    max(Mod(eigen(companheira, only.values = TRUE)$values)) < 1 && rcond(diag(5) - soma) > 1e-8
  }, logical(1))
  if (sum(estaveis) < 50L) return(list(post = NULL, ess = 0))
  chaves <- vapply(seq_len(n), function(s) digest::digest(post$posterior$A[, , s], algo = "xxhash64"), character(1))
  # O alvo é o crescimento anual implícito: (I - soma dos lags domésticos)^(-1) * tendência.
  # O fator de prior é normal na escala logarítmica: centro = 100*log(1.02), desvio = .5/1.02.
  # Define o longo prazo condicional a Treasury e Brent estabilizados, sem alterar suas curvas.
  rb <- prior_tendencia(post, p, estaveis, chaves, centro, desvio)
  pesos <- w * rb$pesos
  stopifnot(all(is.finite(pesos)), sum(pesos) > 0)
  pesos <- pesos / sum(pesos)
  ess <- 1 / sum(tapply(pesos, chaves, sum)^2)
  if (ess < 50) return(list(post = NULL, ess = ess))
  set.seed(321)
  indices <- findInterval(runif(1) / S + (0:(S - 1L)) / S, c(0, cumsum(pesos)), rightmost.closed = TRUE)
  stopifnot(all(estaveis[indices]), all(indices >= 1L & indices <= n))
  for (nome in names(post$posterior)) {
    x <- post$posterior[[nome]]
    if (length(dim(x)) == 3L) post$posterior[[nome]] <- x[, , indices, drop = FALSE]
    if (length(dim(x)) == 2L) post$posterior[[nome]] <- x[, indices, drop = FALSE]
  }
  post$posterior$w <- NULL
  post$posterior$ess <- ess
  X <- post$last_draw$data_matrices$X
  Y <- post$last_draw$data_matrices$Y
  for (s in seq_len(S)) {
    post$posterior$A[, N * p + 1:2, s] <- matrix(sortear_tendencia(rb$condicionais[[indices[s]]]), N, 2L)
    escala <- escala_covid(post$posterior$hyper[, s], post$last_draw$prior$covid, ncol(Y), N)
    post$posterior$sigma[, , s] <- matrix(rep(escala, each = N), N)
    post$posterior$shocks[, , s] <- post$posterior$B[, , s] %*%
      sweep(Y - post$posterior$A[, , s] %*% X, 2, escala, "/")
  }
  for (r in post$last_draw$identification$sign_narrative) {
    meses <- r$start - p + seq_len(r$periods) - 1L
    stopifnot(all(r$sign * post$posterior$shocks[r$shock, meses, ] >= -1e-8))
  }
  stopifnot(all(is.finite(post$posterior$A)), all(is.finite(post$posterior$shocks)))
  attr(post, "prior_inflacao_lp") <- list(centro = centro, desvio = desvio, ess = ess,
    externas_longo_prazo = "estabilizadas", escala = "fator normal sobre crescimento logaritmico anual")
  list(post = post, ess = ess)
}

estimar_prior_longo_prazo <- function(spec, S = 2500L, centro = 2, desvio = .5, max_lotes = 4L) {
  # Guardar propostas e pesos antes da reamostragem nativa, como no experimento anterior.
  stopifnot(packageVersion("bsvarSIGNs") == "3.0", max_lotes >= 1L)
  if (!is.null(spec$mc.cores) && spec$mc.cores != 1L) stop("Use um núcleo nesta estimação com prior de longo prazo.")
  amostrar <- get("estimate.BSVARSIGN", asNamespace("bsvarSIGNs"))
  expressoes <- as.list(body(amostrar))
  fim <- which(vapply(expressoes, function(x) is.call(x) &&
    as.character(x[[1]]) %in% c("=", "<-") && identical(x[[2]], as.name("output")), logical(1)))[1]
  stopifnot(!is.na(fim), grepl("importance_sampling", paste(deparse(body(amostrar)), collapse = " ")))
  body(amostrar) <- as.call(c(expressoes[seq_len(fim - 1L)], list(quote(qqq))))
  propostas <- NULL
  for (lote in seq_len(max_lotes)) {
    message("Prior de inflação: lote ", lote, "/", max_lotes)
    set.seed(600L + lote)
    novo <- amostrar(spec, S = S, show_progress = TRUE)$posterior
    novo$w <- as.numeric(novo$w)
    if (is.null(propostas)) propostas <- novo else for (nome in names(propostas)) {
      x <- propostas[[nome]]; y <- novo[[nome]]
      d <- dim(x)
      propostas[[nome]] <- if (is.null(d)) c(x, y) else
        array(c(x, y), c(head(d, -1L), tail(d, 1L) + tail(dim(y), 1L)))
    }
    post <- get("specify_posterior_bsvarSIGN", asNamespace("bsvarSIGNs"))$new(spec, propostas)
    ajuste <- aplicar_prior_longo_prazo(post, centro, desvio, S)
    message("ESS do prior: ", round(ajuste$ess, 1))
    if (!is.null(ajuste$post)) return(ajuste$post)
  }
  stop("O prior não atingiu ESS 50 após os lotes; aumente max_lotes antes de interpretar o forecast.")
}
