# Stable evaluation of the same Minnesota and dummy-observation prior.
# QR decompositions avoid the invalid-matrix sentinel used by the package.
stable_hyper_target <- function(spec, covid = FALSE) {
  prior <- spec$prior$get_prior()
  prior$B <- t(prior$A)
  prior$Ysoc <- t(prior$Ysoc)
  prior$Xsoc <- t(prior$Xsoc)
  prior$Ysur <- t(prior$Ysur)
  prior$Xsur <- t(prior$Xsur)
  Y <- t(spec$prior$Y)
  X <- t(spec$prior$X)
  N <- ncol(Y)
  K <- ncol(X)
  p <- spec$p
  zeros <- matrix(0, K, N)
  identity <- diag(K)
  gamma_n <- function(n, x) {
    if (n == 1L) return(lgamma(x))
    c <- floor((n - 1)/2)
    c * log(pi) + lgamma(x - c) + gamma_n(n - 1L, x)
  }
  log_ml <- function(YY, XX, v, psi) {
    TT <- nrow(YY)
    ZZ <- sweep(XX, 2, sqrt(v), "*")
    R0 <- YY - XX %*% prior$B
    q <- qr(rbind(ZZ, identity), LAPACK = TRUE)
    C <- qr.coef(q, rbind(R0, zeros))
    residual <- R0 - ZZ %*% C
    logdet_m <- 2 * sum(log(abs(diag(qr.R(q)))))
    qa <- qr(rbind(residual, C, diag(sqrt(psi))), LAPACK = TRUE)
    logdet_a <- 2 * sum(log(abs(diag(qr.R(qa)))))
    nu <- prior$nu
    -N*TT/2*log(pi) + gamma_n(N, (TT + nu)/2) - gamma_n(N, nu/2) +
      nu/2*sum(log(psi)) - N/2*logdet_m - (TT + nu)/2*logdet_a
  }
  function(h) {
    if (any(!is.finite(h)) || any(h <= 0)) return(-Inf)
    psi <- h[4:(N + 3)]
    lp <- 0
    for (i in 1:3) {
      pref <- c("mu", "delta", "lambda")[i]
      shape <- prior[[paste0(pref, ".shape")]]
      scale <- prior[[paste0(pref, ".scale")]]
      lp <- lp + dgamma(h[i], shape = shape, scale = scale, log = TRUE)
    }
    alpha <- prior$psi.shape
    beta <- prior$psi.scale
    lp <- lp + sum(alpha*log(beta) - (alpha + 1)*log(psi) - beta/psi - lgamma(alpha))
    YY <- Y
    XX <- X
    jacobian <- 0
    if (covid) {
      cv <- h[(N + 4):(N + 7)]
      if (any(cv[1:3] < 1) || cv[4] <= 0 || cv[4] >= 1) return(-Inf)
      lp <- lp - 2*sum(log(cv[1:3])) + 2*log(cv[4]) + .5*log1p(-cv[4])
      j <- prior$covid
      scale <- rep(1, nrow(Y))
      for (i in 0:2) if (j + i <= length(scale)) scale[j + i] <- cv[i + 1]
      if (j + 3 <= length(scale)) {
        idx <- (j + 3):length(scale)
        scale[idx] <- 1 + (cv[3] - 1)*cv[4]^(idx - j - 2)
      }
      YY <- Y/scale
      XX <- X/scale
      jacobian <- -N*sum(log(scale))
    }
    v <- diag(prior$V)
    v[1:(N*p)] <- h[3]^2*v[1:(N*p)]*rep(1/psi, p)
    Ystar <- rbind(prior$Ysoc/h[1], prior$Ysur/h[2])
    Xstar <- rbind(prior$Xsoc/h[1], prior$Xsur/h[2])
    result <- lp + log_ml(rbind(Ystar, YY), rbind(Xstar, XX), v, psi) -
      log_ml(Ystar, Xstar, v, psi) + jacobian
    if (is.finite(result)) result else -Inf
  }
}
