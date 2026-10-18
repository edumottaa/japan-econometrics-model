# Substitui spec$estimate_hyper() na versao 3.0 do bsvarSIGNs.
# Mantem a prior e os dados do modelo; corrige o calculo numerico e as propostas.

library(coda)

estimar_hyper_corrigido <- function(spec, S = 15000L, burn_in = 5000L,
                                   inicio_covid = NULL) {
  stopifnot(S > burn_in, burn_in > 0, S %% 1 == 0, burn_in %% 1 == 0)
  covid <- !is.null(inicio_covid)
  if (covid) stopifnot(inicio_covid > 0, inicio_covid <= ncol(spec$prior$Y),
                      inicio_covid %% 1 == 0)
  spec$prior$covid <- inicio_covid
  posterior_estavel <- stable_hyper_target(spec, covid)
  N <- nrow(spec$prior$Y)
  nb <- N + 3L
  n <- nb + if (covid) 4L else 0L
  forward <- function(z) {
    h <- c(exp(z[1:nb]), 1, 1, 1, .8)
    if (covid) h[(nb + 1):(nb + 4)] <- c(1 + exp(z[(nb + 1):(nb + 3)]), plogis(z[nb + 4]))
    h
  }
  inverse <- function(h) {
    z <- log(h[1:nb])
    if (covid) z <- c(z, log(pmax(h[(nb + 1):(nb + 3)] - 1, .1)), qlogis(h[nb + 4]))
    z
  }
  log_target <- function(z) {
    h <- forward(z)
    lp <- posterior_estavel(h)
    if (!is.finite(lp)) return(-1e10)
    jac <- sum(z[1:nb])
    if (covid) jac <- jac + sum(z[(nb + 1):(nb + 3)]) + log(h[nb + 4]) + log1p(-h[nb + 4])
    if (!is.finite(jac)) return(-1e10)
    lp + jac
  }
  cat("Encontrando o ponto inicial dos hiperparametros...\n")
  opt <- optim(inverse(spec$prior$hyper[, ncol(spec$prior$hyper)]),
               function(z) -log_target(z), method = "BFGS",
               control = list(maxit = 1000, reltol = 1e-9), hessian = TRUE)
  ev <- eigen(opt$hessian, symmetric = TRUE)
  if (opt$convergence != 0 || any(ev$values <= 0)) {
    stop("O ponto inicial nao passou nas verificacoes numericas. A estimacao foi interrompida.")
  }
  W <- ev$vectors %*% diag(1/ev$values) %*% t(ev$vectors)
  z <- opt$par
  zbar <- z
  lp <- log_target(z)
  log_scale <- log(2.38/sqrt(n))
  L <- t(chol(W))
  h <- matrix(NA_real_, S, N + 7L)
  h[1, ] <- forward(z)
  accepted <- logical(S)
  cat("Sorteando", S, "combinacoes de hiperparametros...\n")
  for (iter in 2:S) {
    proposal <- z + exp(log_scale)*as.vector(L %*% rnorm(n))
    new_lp <- log_target(proposal)
    log_a <- min(0, new_lp - lp)
    if (log(runif(1)) < log_a) {
      z <- proposal
      lp <- new_lp
      accepted[iter] <- TRUE
    }
    h[iter, ] <- forward(z)
    if (iter <= burn_in) {
      dz <- z - zbar
      zbar <- zbar + dz/iter
      W <- W*(iter - 1)/iter + tcrossprod(dz)*(iter - 1)/(iter*iter)
      log_scale <- log_scale + iter^(-.6)*(exp(log_a) - .234)
      if (iter %% 25L == 0L || iter == burn_in) L <- t(chol(W + diag(1e-8, n)))
    }
    if (iter %% max(1L, floor(S/5)) == 0L) cat("Progresso:", round(100*iter/S), "%\n")
  }
  retained <- h[-(1:burn_in), , drop = FALSE]
  spec$prior$hyper <- t(retained)
  acceptance <- mean(accepted[-(1:burn_in)])
  cat("Taxa de movimentos aceitos:", round(100*acceptance, 1), "%\n")
  if (requireNamespace("coda", quietly = TRUE)) {
    ess <- effectiveSize(retained[, 1:n, drop = FALSE])
    cat("Menor tamanho efetivo de amostra:", round(min(ess)), "\n")
    if (min(ess) < 50) warning("Pouca informacao independente em alguns parametros; aumente S e confira os graficos.")
  }
  invisible(spec)
}
