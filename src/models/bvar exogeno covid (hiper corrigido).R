library(bsvarSIGNs)
library(bsvars)
library(tidyverse)
library(fpp3)

# Mesma especificação de "bvar exogeno covid.R" (9 variáveis, foreign, COVID), com os
# hiperparâmetros estimados: mu, delta, lambda, psi e COVID. Prior e verossimilhança são as do pacote.
# O que muda em relação ao bsvarSIGNs 3.0:
#  1. Hiperparâmetros: Metropolis simples sobre a log-posterior do modelo COM foreign.
#     - Verossimilhança marginal com foreign = a do VAR sem restrição (do pacote) x razão de
#       Savage-Dickey: densidade a posteriori / a priori de "defasagens domésticas nas equações
#       externas = 0". O pacote usa a do VAR sem restrição, e os pesos do foreign degeneram (ESS ~ 2).
#     - Ponto em que a conta falha = impossível. O pacote devolve +1e10 e o amostrador fica preso.
#  2. Sorteios: mesma distribuição e mesmo peso do foreign do pacote, calculados pela decomposição
#     de Cholesky da precisão. O pacote usa diferenças de números enormes, que deixam de ser
#     positivas definidas com mu pequeno (o valor preferido pelos dados).
# psi e mu estimados: log do fator de Bayes (Laplace) de +294 contra psi fixo e de +3,7 contra mu = 1.
# A posterior de mu tem duas modas (~0,004 com ~75% da massa, ~0,5 com ~25%); o Metropolis simples
# troca pouco de moda (ESS de mu baixo), mas as previsões das duas quase não diferem.

pacote <- asNamespace("bsvarSIGNs")
cpp <- function(nome) get(paste0("_bsvarSIGNs_", nome), pacote)   # funções internas do pacote
log_det <- function(M) 2 * sum(log(diag(chol(M))))

estimar_bvar_exogeno_covid_corrigido <- function(dados, p = 6, S = 2500, n_mh = 22000, aquecimento = 2000) {
  dados <- dados |> as_tibble() |> arrange(date)
  externas <- c("ust2y", "ip_mundial", "brent", "vix")
  domesticas <- c("atividade", "cpi_core", "juro_curto", "inclinacao", "usdjpy")
  Y <- dados |> select(all_of(c(externas, domesticas))) |> as.matrix()
  covid <- match(as.Date("2020-03-01"), as.Date(dados$date)) - p   # contado depois das p defasagens

  set.seed(123)
  especificacao <- specify_bsvarSIGN$new(
    data = Y[, domesticas], foreign = Y[, externas], p = p,
    hyper_covid = covid, sign_structural = matrix(NA_real_, 9, 9)
  )
  especificacao$prior$covid <- covid
  prior <- especificacao$prior$get_prior()
  Yt <- t(especificacao$prior$Y); Xt <- t(especificacao$prior$X)
  N <- ncol(Yt); K <- ncol(Xt); Tn <- nrow(Yt)
  B0 <- t(prior$A); v0 <- diag(prior$V)
  Ysoc <- t(prior$Ysoc); Xsoc <- t(prior$Xsoc); Ysur <- t(prior$Ysur); Xsur <- t(prior$Xsur)
  nomes <- c("mu", "delta", "lambda", paste0("psi_", colnames(Y)), "covid_s0", "covid_s1", "covid_s2", "covid_rho")
  # Linhas dos coeficientes: R2 = defasagens domésticas (zeradas nas 4 equações externas); R1 = o resto
  R2 <- as.vector(outer(5:N, N * (0:(p - 1)), "+")); R1 <- setdiff(1:K, R2)
  n1 <- length(R1); n2 <- length(R2); ordem <- c(R1, R2)

  # Prior para h = (mu, delta, lambda, psi_1, ..., psi_9, COVID s0, s1, s2, rho), como no pacote
  monta <- function(h) {
    psi <- h[4:(N + 3)]; s <- h[(N + 4):(N + 7)]
    v <- v0; v[1:(N * p)] <- h[3]^2 * v0[1:(N * p)] * rep(1 / psi, p)
    escala <- rep(1, Tn); escala[covid + 0:2] <- s[1:3]                 # choques maiores na COVID
    t_dep <- (covid + 3):Tn; escala[t_dep] <- 1 + (s[3] - 1) * s[4]^(t_dep - covid - 2)
    Yd <- rbind(Ysoc / h[1], Ysur / h[2]); Xd <- rbind(Xsoc / h[1], Xsur / h[2])   # observações artificiais
    list(V = diag(v), V_inv = diag(1 / v), S = diag(psi), escala = escala, Yd = Yd, Xd = Xd,
         Yp = rbind(Yd, Yt / escala), Xp = rbind(Xd, Xt / escala))
  }

  # Posterior normal-inversa-Wishart dados (Yy, Xx) e log da densidade de "R2 = 0" nas 4 equações
  # externas (t matricial), tudo por uma decomposição de Cholesky da precisão.
  niw <- function(Yy, Xx, m) {
    U <- chol((m$V_inv + crossprod(Xx))[ordem, ordem])        # precisão = U'U, linhas em (R1, R2)
    B_hat <- matrix(0, K, N)
    B_hat[ordem, ] <- backsolve(U, forwardsolve(t(U), (crossprod(Xx, Yy) + m$V_inv %*% B0)[ordem, ]))
    E <- Yy - Xx %*% B_hat
    S_hat <- m$S + crossprod(E) + t(B_hat - B0) %*% m$V_inv %*% (B_hat - B0)   # pelos resíduos
    nu <- prior$nu + nrow(Yy)
    U22 <- U[-(1:n1), -(1:n1)]; nu_f <- nu - (N - 4)
    lmg <- function(a) sum(lgamma(a + (1 - 1:4) / 2))
    log_zero <- -n2 * 4 / 2 * log(pi) + lmg((nu_f + n2) / 2) - lmg(nu_f / 2) + 4 * sum(log(diag(U22))) +
      nu_f / 2 * log_det(S_hat[1:4, 1:4]) -
      (nu_f + n2) / 2 * log_det(S_hat[1:4, 1:4] + crossprod(U22 %*% B_hat[R2, 1:4]))
    list(U = U, B_hat = B_hat, S_hat = S_hat, nu = nu, log_zero = log_zero)
  }

  # log-posterior dos hiperparâmetros no modelo com foreign; falha numérica = impossível
  log_post <- function(h) {
    m <- monta(h)
    if (any(m$escala <= 0)) return(-Inf)
    ml_tudo  <- .Call(cpp("log_ml"), B0, m$V, m$S, prior$nu, m$Yp, m$Xp)
    ml_prior <- .Call(cpp("log_ml"), B0, m$V, m$S, prior$nu, m$Yd, m$Xd)
    if (ml_tudo <= -1e10 || ml_prior <= -1e10) return(-Inf)
    savage_dickey <- niw(m$Yp, m$Xp, m)$log_zero - niw(m$Yd, m$Xd, m)$log_zero
    lp <- .Call(cpp("log_prior_hyper"), matrix(h), c(1, 1, 1, 1, 1), prior) +
      ml_tudo - ml_prior - N * sum(log(m$escala)) + savage_dickey
    if (is.finite(lp)) lp else -Inf
  }

  # 1. Metropolis simples em log(h): começa na moda; proposta = inversa da hessiana x 2,38/sqrt(16)
  alvo <- function(z) log_post(exp(z)) + sum(z)        # + sum(z): mudança de variável para log
  h0 <- especificacao$prior$hyper[, 1]; h0[13:16] <- c(1.5, 1.5, 1.5, 0.8)
  # A posterior de mu tem duas modas (perto de 0,5 e perto de 0,003): procurar a partir de
  # mu = 1 (padrão do pacote) e mu = 0,01, e começar na de maior log-posterior.
  otimos <- lapply(c(1, 0.01), function(mu0) {
    optim(log(replace(h0, 1, mu0)), \(z) -alvo(z), method = "BFGS", control = list(maxit = 3000))
  })
  z <- otimos[[which.min(sapply(otimos, `[[`, "value"))]]$par
  L <- t(chol(solve(optimHess(z, \(z) -alvo(z))))) * 2.38 / sqrt(length(z))
  d <- alvo(z); Z <- matrix(NA_real_, length(z), n_mh, dimnames = list(nomes, NULL)); aceitos <- 0
  for (i in 1:n_mh) {
    z_novo <- z + as.vector(L %*% rnorm(length(z))); d_novo <- alvo(z_novo)
    if (log(runif(1)) < d_novo - d) { z <- z_novo; d <- d_novo; aceitos <- aceitos + 1 }
    Z[, i] <- z
  }
  hiper <- exp(Z[, -(1:aquecimento)])
  hiper <- hiper[, round(seq(1, ncol(hiper), length.out = S))]   # S sorteios espaçados

  # 2. Para cada h: Sigma, coeficientes com a restrição do foreign e o peso
  #    (mesmas contas de sample_restricted_B_cpp do pacote)
  A <- array(NA_real_, c(N, K, S)); Sigma <- Theta0 <- B <- array(NA_real_, c(N, N, S)); log_w <- numeric(S)
  for (s in 1:S) {
    m <- monta(hiper[, s]); post <- niw(m$Yp, m$Xp, m)
    U <- post$U; U11 <- U[1:n1, 1:n1]; U12 <- U[1:n1, -(1:n1)]; U22 <- U[-(1:n1), -(1:n1)]
    Sig <- solve(rWishart(1, post$nu, solve(post$S_hat))[, , 1])

    # Equações externas: linhas R1 dado R2 = 0
    Ls <- chol(Sig[1:4, 1:4]); M2 <- post$B_hat[R2, 1:4]
    Bf <- matrix(0, K, 4)
    Bf[R1, ] <- post$B_hat[R1, 1:4] + backsolve(U11, U12 %*% M2) +
      backsolve(U11, matrix(rnorm(n1 * 4), n1, 4)) %*% Ls
    # Peso: densidade de "R2 = 0" dado Sigma, dividida pela mesma densidade sem condicionar em Sigma
    log_w[s] <- 4 * sum(log(diag(U22))) - n2 * sum(log(diag(Ls))) -
      0.5 * sum((U22 %*% M2 %*% solve(Ls))^2) - n2 * 4 / 2 * log(2 * pi) - post$log_zero

    # Equações domésticas: condicionais aos coeficientes externos sorteados
    S11_S12 <- solve(Sig[1:4, 1:4], Sig[1:4, 5:N])
    W <- matrix(0, K, N - 4); W[ordem, ] <- backsolve(U, matrix(rnorm(K * (N - 4)), K, N - 4))
    Bd <- post$B_hat[, 5:N] + (Bf - post$B_hat[, 1:4]) %*% S11_S12 +
      W %*% chol(Sig[5:N, 5:N] - Sig[5:N, 1:4] %*% S11_S12)

    A[, , s] <- t(cbind(Bf, Bd)); Sigma[, , s] <- Sig
    Theta0[, , s] <- t(chol(Sig))                    # choques externos primeiro: bloco-triangular
    B[, , s] <- solve(Theta0[, , s])
  }

  # 3. Objeto e reamostragem por importância do próprio pacote; pesos divididos pelo maior (sem estouro)
  posterior <- specify_posterior_bsvarSIGN$new(especificacao, list(
    w = exp(log_w - max(log_w)), hyper = hiper, A = A, B = B, Sigma = Sigma, Theta0 = Theta0,
    Q = array(diag(N), c(N, N, S))
  ))
  posterior <- pacote$importance_sampling(posterior)

  # Mesma checagem do script original
  coeficientes <- matrix(posterior$posterior$A, ncol = S)
  lags_domesticos <- unlist(lapply(0:(p - 1), function(j) 5:9 + 9 * j))
  diagnostico <- tibble(
    sorteios = S, aceitacao_mh = aceitos / n_mh, ess_ponderacao = posterior$posterior$ess,
    conjuntos_distintos = ncol(unique(coeficientes, MARGIN = 2)),
    valores_validos = all(is.finite(coeficientes)) && all(is.finite(posterior$posterior$Sigma)),
    coef_dom_nas_externas = max(abs(posterior$posterior$A[1:4, lags_domesticos, ])),
    impacto_dom_nas_externas = max(abs(posterior$posterior$Theta0[1:4, 5:9, ]))
  )
  if (!diagnostico$valores_validos || diagnostico$ess_ponderacao < 50 || diagnostico$conjuntos_distintos < 50 ||
      diagnostico$coef_dom_nas_externas > 1e-8 || diagnostico$impacto_dom_nas_externas > 1e-8) {
    print(diagnostico, width = Inf)
    stop("O ajuste nao passou pela checagem numerica; nao interpretar as previsoes.")
  }
  list(posterior = posterior, dados_treino = Y, hiperparametros = t(hiper),
       cadeia_hiper = t(exp(Z)), diagnostico = diagnostico,
       log_posterior_hiper = log_post)   # para comparar especificações (ex.: psi fixo)
}

# Previsão. O forecast() do bsvarSIGNs 3.0 erra com p > 1 (só atualiza a primeira defasagem):
# com os mesmos coeficientes, prevê cpi_core em 463,9 em 12 meses; iterando o VAR, 474,3.
# Mesma saída de forecast()$forecasts (variáveis x meses x sorteios), iterando o VAR com choques sorteados.
prever_bvar_exogeno <- function(ajuste, horizonte = 12) {
  post <- ajuste$posterior$posterior; Y <- ajuste$dados_treino
  N <- ncol(Y); p <- ajuste$posterior$last_draw$p; S <- dim(post$A)[3]
  Tn <- nrow(Y) - p; covid <- ajuste$posterior$last_draw$prior$covid
  previsao <- array(NA_real_, c(N, horizonte, S), dimnames = list(colnames(Y), NULL, NULL))
  for (s in 1:S) {
    x <- c(t(Y[nrow(Y):(nrow(Y) - p + 1), ]), 1)        # últimos p meses + constante
    L <- t(chol(post$Sigma[, , s]))
    s2 <- post$hyper["covid_s2", s]; rho <- post$hyper["covid_rho", s]
    for (j in 1:horizonte) {
      escala <- 1 + (s2 - 1) * rho^(Tn + j - covid - 2)  # efeito da COVID nos choques (hoje ~ 1)
      y <- post$A[, , s] %*% x + escala * L %*% rnorm(N)
      previsao[, j, s] <- y
      x <- c(y, x[1:(N * (p - 1))], 1)                   # empurra as defasagens um mês
    }
  }
  previsao
}

# Para estimar e prever:
# ajuste <- estimar_bvar_exogeno_covid_corrigido(data_bal, p = 6)
# sorteios <- prever_bvar_exogeno(ajuste, horizonte = 12)   # no lugar de bsvarSIGNs::forecast()$forecasts
# Gráficos dos sorteios dos hiperparâmetros (cadeia inteira, com o aquecimento):
# plot.ts(ajuste$cadeia_hiper[, 1:8], col = "#F500BD", bty = "n", xlab = "")
# plot.ts(ajuste$cadeia_hiper[, 9:16], col = "#F500BD", bty = "n", xlab = "")
