# BVAR sem tendência; prior adicional sobre a inflação de equilíbrio.
library(tidyverse)
library(bsvarSIGNs)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")

# Atualizar somente as domesticas, com corte em setembro/2026; estimar ate o ultimo mes completo.
source("src/1- data_collection/1- data_collection bvar estrutural.R", local = TRUE, encoding = "UTF-8")

# Inflação mensal anualizada em pontos logarítmicos; o CPI original já está em 100 * log.
dados_boj_prior <- dados_boj |>
  mutate(inflacao = 12 * (cpi_core - lag(cpi_core))) |>
  slice(-1)

# 1. Construção das matrizes dos blocos externo e doméstico

matrix_externa <- dados_boj_prior |>
  as_tibble() |>
  select(ust2y, brent) |>
  as.matrix()

matrix_interna <- dados_boj_prior |>
  as_tibble() |>
  select(atividade, inflacao, juro_curto, inclinacao, usdjpy) |>
  as.matrix()

#2. Seis defasagens mensais; sete variáveis e sete choques estruturais.
p <- 6

# Priors manuais; lambda menor aumenta o shrinkage geral.
lambda <- 0.2
mu <- 1
delta <- 1
stopifnot(lambda > 0, mu > 0, delta > 0)

#3 Estimação dos hiperparametros da COVID
# COVID contada após as p observações iniciais usadas como defasagens.
inicio_covid <- match(as.Date("2020-03-01"), as.Date(dados_boj_prior$date)) - p
if (is.na(inicio_covid) || inicio_covid <= 0) inicio_covid <- NULL
# Estimar os quatro parâmetros COVID somente com quatro meses já observados.
if (!is.null(inicio_covid) && nrow(dados_boj_prior) - p - inicio_covid + 1L < 4L) inicio_covid <- NULL

# Respostas x choques x horizontes; posições 1:13 representam meses 0:12.
# Variáveis: ust2y, brent, atividade, inflacao, juro_curto, inclinacao, usdjpy.
# Choques: Fed, petróleo, residual doméstico 1, residual doméstico 2, BoJ, prêmio fiscal/prazo, residual de fluxo/UIP.
sinais_boj <- array(NA_real_, dim = c(7, 7, 13))

# BoJ (5): juro sobe, curva achata; atividade e inflação caem nos meses 3 a 12.
sinais_boj[5, 5, 1:2] <- 1
sinais_boj[6, 5, 1:2] <- -1
sinais_boj[c(3, 4), 5, 4:13] <- -1

# Choques 3 e 4: residuais domesticos, sem sinais, como no teste 1 do CV.

# Prêmio fiscal/de prazo (6): curva inclina e USDJPY sobe.
# Esses sinais não provam uma causa fiscal exclusiva.
sinais_boj[6, 6, 1:2] <- 1
sinais_boj[7, 6, 1:2] <- 1

# Fed (1): Treasury de 2 anos sobe e Brent cai; respostas japonesas livres.
sinais_boj[1, 1, 1:2] <- 1
sinais_boj[2, 1, 1:2] <- -1

# Petróleo (2): Brent sobe; respostas japonesas livres.
# Sem IP mundial, esse sinal sozinho não separa oferta de petróleo de demanda global.
sinais_boj[2, 2, 1:2] <- 1

# Fluxo/UIP (7): residual, sem restricoes de sinal ou narrativas.

# 4. Restrições Narrativas 

# Demanda global e risk-off não são identificados separadamente no modelo de sete variáveis.

# 4.3. Fim do juro negativo e do YCC em março/2024: aperto do BoJ (choque 5).
# Decisão em 19/03/2024; o evento não prova que a surpresa mensal tenha sido positiva.
# Fonte: https://www.boj.or.jp/en/mopo/mpmdeci/state_2024/k240319a.htm
inicio_fim_ycc <- match(as.Date("2024-03-01"), as.Date(dados_boj_prior$date))
narrativa_boj <- list()
if (!is.na(inicio_fim_ycc) && inicio_fim_ycc > p) {
  narrativa_boj <- list(bsvarSIGNs::specify_narrative(
    start = inicio_fim_ycc, periods = 1, type = "S", sign = 1, shock = 5
  ))
}

# 4.4. Eleição de Takaichi para a presidência do LDP em outubro/2025: prêmio positivo (choque 6).
# Eleição em 04/10/2025; o sinal fiscal/de prazo é a hipótese do plano, não um fato confirmado pela data.
# Fonte: https://www.jimin.jp/news/information/211511.html
inicio_takaichi <- match(as.Date("2025-10-01"), as.Date(dados_boj_prior$date))
narrativa_takaichi <- list()
if (!is.na(inicio_takaichi) && inicio_takaichi > p) {
  narrativa_takaichi <- list(bsvarSIGNs::specify_narrative(
    start = inicio_takaichi, periods = 1, type = "S", sign = 1, shock = 6
  ))
}

narrativas <- c(narrativa_boj, narrativa_takaichi)

# Os choques identificados entram juntos na mesma especificação e na mesma estimação.
spec_boj_prior = specify_bsvarSIGN$new(
  data = matrix_interna,
  foreign = matrix_externa,
  stationary = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
  p = p,
  hyper_mu = FALSE,
  hyper_delta = FALSE,
  hyper_lambda = FALSE,
  hyper_psi = FALSE,
  hyper_covid = inicio_covid,
  sign_irf = sinais_boj,
  sign_narrative = narrativas,
  sign_structural = matrix(NA_real_, 7, 7)
)

# Aplicar os valores manuais antes da estimacao.
spec_boj_prior$prior$hyper[1:3, ] <- c(mu, delta, lambda)

# 4. Estimação e inspeção dos sorteios dos hiperparâmetros
set.seed(123)
if (!is.null(inicio_covid)) spec_boj_prior$estimate_hyper(S = 5000, burn_in = 2500)

dim(spec_boj_prior$prior$hyper)
hyper_draws = t(spec_boj_prior$prior$hyper)

# Trajetórias dos quatro hiperparâmetros COVID estimados
if (!is.null(inicio_covid)) {
  hyper_covid_draws <- hyper_draws[, (ncol(hyper_draws) - 3):ncol(hyper_draws), drop = FALSE]
  colnames(hyper_covid_draws) <- c("COVID março", "COVID abril", "COVID maio", "Decaimento")
  plot.ts(hyper_covid_draws, nc = 1, col = "black",
          main = "Sorteios dos hiperparâmetros COVID", xlab = "Iteração")
}

# 5. Estimacao conjunta: Fed, petroleo, BoJ e premio identificados, como no CV.
spec_boj_prior$identification$max_tries <- 1000
post_boj_sem_prior = bsvars::estimate(spec_boj_prior, S = 2500, show_progress = TRUE)

# 5.1. Checagem simples dos resultados da estimação
coeficientes_sorteados <- matrix(
  post_boj_sem_prior$posterior$A,
  ncol = dim(post_boj_sem_prior$posterior$A)[3]
)
lags_domesticos <- unlist(lapply(0:(p - 1), function(j) 3:7 + 7 * j))

diagnostico_estimacao <- tibble(
  sorteios = ncol(coeficientes_sorteados),
  valores_validos = all(is.finite(coeficientes_sorteados)) &&
    all(is.finite(post_boj_sem_prior$posterior$Sigma)),
  conjuntos_distintos = ncol(unique(coeficientes_sorteados, MARGIN = 2)),
  ess_ponderacao = post_boj_sem_prior$posterior$ess,
  coef_dom_nas_externas = max(abs(post_boj_sem_prior$posterior$A[1:2, lags_domesticos, ])),
  impacto_dom_nas_externas = max(abs(post_boj_sem_prior$posterior$Theta0[1:2, 3:7, ]))
)

diagnostico_estimacao

if (!diagnostico_estimacao$valores_validos || !is.finite(diagnostico_estimacao$ess_ponderacao) ||
    diagnostico_estimacao$ess_ponderacao < 50 || diagnostico_estimacao$conjuntos_distintos < 50 ||
    diagnostico_estimacao$coef_dom_nas_externas > 1e-8 || diagnostico_estimacao$impacto_dom_nas_externas > 1e-8) {
  stop("O ajuste nao passou pela checagem numerica; nao interpretar as previsoes.")
}

# 6. Fator de prior normal, centro 2% e desvio 0.5 p.p., sobre a inflação de equilíbrio.
# Reponderação de parâmetros; não condiciona as trajetórias do forecast.
aplicar_prior_inflacao <- function(post, externas, p, centro = 2, desvio = 0.5) {
  N <- nrow(post$posterior$A)
  S <- dim(post$posterior$A)[3]
  stopifnot(N == 7L, length(externas) == 2L, desvio > 0)
  inflacao_lp <- rep(NA_real_, S)
  colunas <- unlist(lapply(0:(p - 1), function(j) 3:N + N * j))
  for (s in seq_len(S)) {
    A <- post$posterior$A[, , s]
    companheira <- rbind(A[3:N, colunas],
      cbind(diag((N - 2) * (p - 1)), matrix(0, (N - 2) * (p - 1), N - 2)))
    if (max(Mod(eigen(companheira, only.values = TRUE)$values)) >= 1) next
    soma_dom <- Reduce(`+`, lapply(0:(p - 1), function(j) A[3:N, 3:N + N * j]))
    soma_ext <- Reduce(`+`, lapply(0:(p - 1), function(j) A[3:N, 1:2 + N * j]))
    sistema <- diag(N - 2) - soma_dom
    if (rcond(sistema) < 1e-8) next
    equilibrio <- solve(sistema, A[3:N, N * p + 1] + soma_ext %*% externas)
    inflacao_lp[s] <- 100 * expm1(equilibrio[2] / 100)
  }

  log_pesos <- dnorm(inflacao_lp, centro, desvio, log = TRUE)
  log_pesos[!is.finite(log_pesos)] <- -Inf
  if (!any(is.finite(log_pesos))) stop("Nenhum sorteio com equilíbrio e peso finitos.")
  pesos <- exp(log_pesos - max(log_pesos))
  pesos <- pesos / sum(pesos)
  coeficientes <- matrix(post$posterior$A, ncol = S)
  chaves <- vapply(seq_len(S), function(s) digest::digest(coeficientes[, s]), character(1))
  ess_prior <- 1 / sum(pesos^2)
  ess_distintos <- 1 / sum(tapply(pesos, chaves, sum)^2)
  diagnostico <- tibble(estaveis = sum(is.finite(inflacao_lp)),
    ess_original = post$posterior$ess, ess_prior, ess_distintos,
    inflacao_lp = sum(pesos[pesos > 0] * inflacao_lp[pesos > 0]))
  print(diagnostico)
  if (min(diagnostico$ess_original, ess_prior, ess_distintos) < 30) {
    stop("Pouca diversidade para o prior; não interpretar o ajuste.")
  }
  if (min(diagnostico$ess_original, ess_prior, ess_distintos) < 50) {
    warning("Ajuste exploratório: ESS entre 30 e 50.")
  }

  indices <- sample.int(S, S, replace = TRUE, prob = pesos)
  novo <- post$clone(deep = TRUE)
  for (nome in names(post$posterior)) {
    x <- post$posterior[[nome]]
    if (length(dim(x)) == 3L) novo$posterior[[nome]] <- x[, , indices, drop = FALSE]
    if (length(dim(x)) == 2L) novo$posterior[[nome]] <- x[, indices, drop = FALSE]
  }
  novo$posterior$ess <- min(post$posterior$ess, ess_prior, ess_distintos)

  # Recalcular os campos auxiliares que o resampling nativo da versão 3.0 não alinha.
  X <- novo$last_draw$data_matrices$X
  Y <- novo$last_draw$data_matrices$Y
  covid <- novo$last_draw$prior$covid
  for (s in seq_len(S)) {
    escala <- rep(1, ncol(Y))
    hyper <- novo$posterior$hyper[, s]
    if (!is.null(covid)) {
      escala[covid + 0:2] <- hyper[N + 4:6]
      meses <- seq.int(covid + 3, ncol(Y))
      escala[meses] <- 1 + (hyper[N + 6] - 1) * hyper[N + 7]^seq_along(meses)
    }
    novo$posterior$sigma[, , s] <- matrix(rep(escala, each = N), N)
    novo$posterior$shocks[, , s] <- novo$posterior$B[, , s] %*%
      sweep(Y - novo$posterior$A[, , s] %*% X, 2, escala, "/")
  }
  list(post = novo, diagnostico = diagnostico,
       inflacao_lp = inflacao_lp, pesos = pesos, indices = indices)
}

# Equilíbrio condicional: Treasury e Brent constantes nos últimos valores observados.
centro_inflacao <- 2
desvio_inflacao <- 0.5
externas_longo_prazo <- drop(tail(matrix_externa, 1))
set.seed(321)
ajuste_prior <- aplicar_prior_inflacao(post_boj_sem_prior, externas_longo_prazo, p,
                                     centro_inflacao, desvio_inflacao)
post_boj_prior <- ajuste_prior$post
diagnostico_prior <- ajuste_prior$diagnostico
