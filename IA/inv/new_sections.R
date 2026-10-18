# 5 Build the model data matrix (foreign block first, then domestic block)
dados <- data_bal |>
  as_tibble() |>
  select(ust2y, ip_mundial, brent, vix,
         atividade, cpi_core, juro_curto, inclinacao, usdjpy, topix) |>
  as.matrix()

# 6 Specify the BVAR (Minnesota + sum-of-coefficients + single-unit-root prior, GLP 2015)
# TODO (Jota): block exogeneity (plan 3.3). `foreign` in bsvarSIGNs 3.0 imposes it by
# importance sampling, which degenerates here (effective sample size ~2 of 200 draws),
# so the 10 variables enter as a single block for now.
# TODO (Jota): COVID adjustment (hyper_covid).
spec <- specify_bsvarSIGN$new(data = dados, p = 12)

# 7 Posterior of the hyperparameters and of the VAR parameters
# Three routines of bsvarSIGNs 3.0 are replaced (details in the PDF):
#  - estimate_hyper(): a failed evaluation of the dummy-observation likelihood returns
#    -1e10, which is subtracted and becomes +1e10, so the sampler gets stuck;
#  - estimate(): computes the posterior scale of Sigma as a difference of large matrices,
#    which stops being positive definite for the hyperparameters favoured by the data;
#  - forecast(): starts one period early and, for p > 1, only updates the first lag.
# The model is unchanged: same prior and likelihood, from the package's own functions.
make_bvar <- function(spec) {
  ns     <- asNamespace("bsvarSIGNs")
  log_ml <- get("_bsvarSIGNs_log_ml", ns)
  log_pr <- get("_bsvarSIGNs_log_prior_hyper", ns)
  prior  <- spec$prior$get_prior()
  Y  <- t(spec$prior$Y)
  X  <- t(spec$prior$X)
  N  <- ncol(Y)
  K  <- ncol(X)
  p  <- spec$p
  B0 <- t(prior$A)
  v0 <- diag(prior$V)
  Ysoc <- t(prior$Ysoc); Xsoc <- t(prior$Xsoc)
  Ysur <- t(prior$Ysur); Xsur <- t(prior$Xsur)

  # prior variances and dummy observations for h = (mu, delta, lambda, psi_1, ..., psi_N)
  setup <- function(h) {
    v <- v0
    v[1:(N * p)] <- h[3]^2 * v0[1:(N * p)] * rep(1 / h[-(1:3)], p)
    list(v = v, psi = h[-(1:3)],
         Yd = rbind(Ysoc / h[1], Ysur / h[2]), Xd = rbind(Xsoc / h[1], Xsur / h[2]))
  }

  # log posterior of the hyperparameters; a failed evaluation is an impossible point
  log_post <- function(h) {
    s <- setup(h)
    ml_all   <- .Call(log_ml, B0, diag(s$v), diag(s$psi), prior$nu, rbind(s$Yd, Y), rbind(s$Xd, X))
    ml_dummy <- .Call(log_ml, B0, diag(s$v), diag(s$psi), prior$nu, s$Yd, s$Xd)
    if (ml_all <= -1e10 || ml_dummy <= -1e10) return(-1e10)
    out <- .Call(log_pr, matrix(c(h, 1, 1, 1, 0.8)), c(1, 1, 1, 1, 0), prior) + ml_all - ml_dummy
    if (is.finite(out)) out else -1e10
  }

  # one draw of (B, Sigma) from the conjugate posterior, given h
  draw <- function(h) {
    s     <- setup(h)
    Yp    <- rbind(s$Yd, Y)
    Xp    <- rbind(s$Xd, X)
    V_inv <- diag(1 / s$v)
    R     <- chol(V_inv + crossprod(Xp))                    # posterior precision of B = R'R
    B_hat <- backsolve(R, forwardsolve(t(R), crossprod(Xp, Yp) + V_inv %*% B0))
    E     <- Yp - Xp %*% B_hat
    S     <- diag(s$psi) + crossprod(E) + t(B_hat - B0) %*% V_inv %*% (B_hat - B0)
    Sigma <- solve(rWishart(1, prior$nu + nrow(Yp), solve(S))[, , 1])
    B     <- B_hat + backsolve(R, matrix(rnorm(K * N), K, N)) %*% chol(Sigma)
    list(B = B, Sigma = Sigma)
  }

  list(log_post = log_post, draw = draw)
}

bv      <- make_bvar(spec)
target  <- function(z) bv$log_post(exp(z)) + sum(z)   # hyperparameters on the log scale (+ Jacobian)
n_hyper <- 3 + ncol(dados)

# 7.1 Posterior mode of the hyperparameters, starting from the package defaults
opt <- optim(log(spec$prior$hyper[1:n_hyper, 1]), function(z) -target(z),
             method = "BFGS", control = list(maxit = 2000))
stopifnot(opt$convergence == 0)

# 7.2 Random-walk Metropolis (GLP 2015): proposal = inverse Hessian at the mode, scaled by 2.38 / sqrt(d)
L <- t(chol(solve(optimHess(opt$par, function(z) -target(z))))) * 2.38 / sqrt(n_hyper)

n_draw <- 15000
n_burn <- 5000
n_keep <- 1000

set.seed(123)
z <- opt$par
lp <- target(z)
accepted <- 0
draws <- matrix(NA, n_hyper, n_draw)
for (i in 1:n_draw) {
  z_new  <- z + drop(L %*% rnorm(n_hyper))
  lp_new <- target(z_new)
  if (log(runif(1)) < lp_new - lp) {
    z <- z_new
    lp <- lp_new
    accepted <- accepted + 1
  }
  draws[, i] <- exp(z)
}
accepted / n_draw   # acceptance rate, should be around 0.2-0.4

hyper <- draws[, seq(n_burn + 1, n_draw, by = (n_draw - n_burn) / n_keep)]
rownames(hyper) <- c("mu", "delta", "lambda", paste0("psi_", colnames(dados)))

# 7.3 Inspect the hyperparameter draws
round(t(apply(hyper, 1, quantile, probs = c(0.05, 0.5, 0.95))), 4)
plot.ts(t(hyper[c("lambda", "mu", "delta"), ]), main = "Hyperparameter draws")

# 7.4 VAR parameters: one draw of (B, Sigma) per hyperparameter draw
set.seed(123)
post <- lapply(seq_len(n_keep), function(s) bv$draw(hyper[, s]))

# 8 Forecast: simulate the VAR forward from the last observation, once per posterior draw
forecast_var <- function(post, data, p, horizon) {
  N  <- ncol(data)
  T  <- nrow(data)
  x0 <- c(t(data[T:(T - p + 1), ]), 1)         # (y_T, y_T-1, ..., y_T-p+1, constant)
  out <- array(NA, c(horizon, N, length(post)), dimnames = list(NULL, colnames(data), NULL))
  for (s in seq_along(post)) {
    x <- x0
    L <- t(chol(post[[s]]$Sigma))
    for (h in 1:horizon) {
      y <- drop(crossprod(post[[s]]$B, x) + L %*% rnorm(N))
      out[h, , s] <- y
      x <- c(y, x[1:(N * (p - 1))], 1)
    }
  }
  out
}

horizon <- 24
set.seed(123)
fc <- forecast_var(post, dados, p = 12, horizon = horizon)

# Fan chart: median and 90% band, history since 2021
fc_bands <- map_dfr(colnames(dados), function(v) tibble(
  date     = as.Date(max(data_bal$date) + 1:horizon),
  variavel = v,
  q05      = apply(fc[, v, ], 1, quantile, 0.05),
  q50      = apply(fc[, v, ], 1, median),
  q95      = apply(fc[, v, ], 1, quantile, 0.95)
))

history <- data_bal |>
  as_tibble() |>
  filter(year(date) >= 2021) |>
  mutate(date = as.Date(date)) |>
  pivot_longer(-date, names_to = "variavel", values_to = "valor")

ggplot() +
  geom_ribbon(data = fc_bands, aes(date, ymin = q05, ymax = q95), fill = "#F500BD", alpha = 0.25) +
  geom_line(data = fc_bands, aes(date, q50), colour = "#F500BD") +
  geom_line(data = history, aes(date, valor)) +
  facet_wrap(~variavel, scales = "free_y")



#Cross Validation Test CPI
