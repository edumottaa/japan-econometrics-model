# Integrar o coeficiente de tendencia antes de reponderar os demais parametros.
# Condicional normal derivada da mesma posterior Minnesota/NIW do pacote.
escala_covid <- function(hyper, covid, T, N) {
  escala <- rep(1, T)
  if (!is.null(covid) && covid > 0 && covid <= T) {
    for (j in 0:2) if (covid + j <= T) escala[covid + j] <- hyper[N + 4L + j]
    if (covid + 3L <= T) escala[(covid + 3L):T] <-
      1 + (hyper[N + 6L] - 1) * hyper[N + 7L]^seq_len(T - covid - 2L)
  }
  escala
}

prior_tendencia <- function(post, p, estaveis, chaves, centro, desvio) {
  N <- nrow(post$posterior$A)
  K <- dim(post$posterior$A)[2]
  S <- dim(post$posterior$A)[3]
  Y <- post$last_draw$data_matrices$Y
  X <- post$last_draw$data_matrices$X
  pr <- post$last_draw$prior
  lista <- post$last_draw$identification$sign_narrative
  stopifnot(all(vapply(lista, function(x) x$type == "S", logical(1))))
  narrativas <- do.call(rbind, lapply(lista, function(x)
    c(1, x$sign, 0, x$shock, x$start - p, x$periods - 1L)))
  centro_log <- 100 * log1p(centro / 100)
  desvio_log <- desvio / (1 + centro / 100)
  log_pesos <- rep(-Inf, S)
  condicionais <- vector("list", S)
  cache <- new.env(parent = emptyenv())
  for (s in which(estaveis)) {
    chave <- chaves[s]
    if (exists(chave, envir = cache, inherits = FALSE)) {
      z <- get(chave, envir = cache)
    } else {
      A <- post$posterior$A[, , s]
      sigma <- escala_covid(post$posterior$hyper[, s], pr$covid, ncol(Y), N)
      Xw <- sweep(X, 2, sigma, "/")
      Yw <- sweep(Y, 2, sigma, "/")
      hyper <- post$posterior$hyper[, s]
      variancias <- diag(pr$V) * c(hyper[3]^2 * rep(1 / hyper[4:(N + 3)], p), rep(1, K - N * p))
      Xd <- cbind(pr$Xsoc / hyper[1], pr$Xsur / hyper[2])
      Yd <- cbind(pr$Ysoc / hyper[1], pr$Ysur / hyper[2])
      det <- (K - 1L):K
      autoreg <- seq_len(K - 2L)
      q <- Xw %*% t(Xw[det, , drop = FALSE]) + Xd %*% t(Xd[det, , drop = FALSE])
      q[det, ] <- q[det, ] + diag(1 / variancias[det])
      m_coluna <- Yw %*% t(Xw[det, , drop = FALSE]) + Yd %*% t(Xd[det, , drop = FALSE]) +
        pr$A[, det, drop = FALSE] %*% diag(1 / variancias[det])
      V_colunas <- solve(q[det, ])
      m <- c((m_coluna - A[, autoreg, drop = FALSE] %*% q[autoreg, ]) %*% V_colunas)
      V <- kronecker(V_colunas, post$posterior$Sigma[, , s])
      soma <- Reduce(`+`, lapply(seq_len(p), function(j) A[3:N, N * (j - 1L) + 3:N, drop = FALSE]))
      l <- c(rep(0, N), 0, 0, solve(diag(N - 2L) - soma)[2, ])
      v_g <- drop(t(l) %*% V %*% l)
      media_g <- sum(l * m)
      ganho <- drop(V %*% l) / (v_g + desvio_log^2)
      m1 <- m + ganho * (centro_log - media_g)
      V1 <- V - tcrossprod(drop(V %*% l)) / (v_g + desvio_log^2)
      V1 <- (V1 + t(V1)) / 2
      C <- matrix(numeric(), 0, 2L * N)
      d <- numeric()
      B <- post$posterior$B[, , s]
      for (r in seq_len(nrow(narrativas))) for (j in 0:narrativas[r, 6]) {
        t <- as.integer(narrativas[r, 5] + j)
        choque <- as.integer(narrativas[r, 4])
        sinal <- narrativas[r, 2]
        residuo <- Y[, t] - A[, autoreg, drop = FALSE] %*% X[autoreg, t]
        C <- rbind(C, sinal * c(B[choque, ], X[K, t] * B[choque, ]))
        d <- c(d, sinal * sum(B[choque, ] * residuo))
      }
      stopifnot(all(drop(C %*% c(A[, det])) <= d + 1e-8))
      probabilidade <- function(media, covariancia) as.numeric(mvtnorm::pmvnorm(
        upper = d, mean = drop(C %*% media), sigma = C %*% covariancia %*% t(C),
        algorithm = mvtnorm::TVPACK()))
      P0 <- probabilidade(m, V)
      P1 <- probabilidade(m1, V1)
      stopifnot(is.finite(P0), P0 > 1e-14, is.finite(P1), P1 >= 0)
      log_peso <- dnorm(centro_log, media_g, sqrt(v_g + desvio_log^2), log = TRUE) + log(P1) - log(P0)
      z <- list(m = m1, chol = chol(V1), C = C, d = d, log_peso = log_peso, prob = P1)
      assign(chave, z, envir = cache)
    }
    condicionais[[s]] <- z
    log_pesos[s] <- z$log_peso
  }
  pesos <- exp(log_pesos - max(log_pesos))
  pesos <- pesos / sum(pesos)
  list(pesos = pesos, condicionais = condicionais)
}

sortear_tendencia <- function(condicional) {
  N <- length(condicional$m)
  for (tentativa in seq_len(10000L)) {
    candidatos <- sweep(t(condicional$chol) %*% matrix(rnorm(N * 64L), N), 1, condicional$m, "+")
    aceitos <- which(colSums(sweep(condicional$C %*% candidatos, 1, condicional$d, "-") > 1e-10) == 0)
    if (length(aceitos)) return(candidatos[, aceitos[1]])
  }
  stop("Amostragem da tendencia com narrativas nao convergiu.")
}
