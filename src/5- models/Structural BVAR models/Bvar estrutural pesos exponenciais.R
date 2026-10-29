library(tidyverse)
library(bsvarSIGNs)
# Pesos por precisão: Var(erro_t) = escala_COVID_t^2 * Sigma / peso_t.

# 1. Construção das matrizes dos blocos externo e doméstico

matrix_externa <- dados_boj |>
  as_tibble() |>
  select(ust2y, brent) |>
  as.matrix()

matrix_interna <- dados_boj |>
  as_tibble() |>
  select(atividade, cpi_core, juro_curto, inclinacao, usdjpy) |>
  as.matrix()

# Tendência em anos, centrada no último mês; coeficientes estimados pelo modelo
tendencia <- matrix((seq_len(nrow(matrix_interna)) - nrow(matrix_interna)) / 12, ncol = 1)

#2. Seis defasagens mensais; sete variáveis e sete choques estruturais
p <- 6

# Priors 
lambda <- 0.2
mu <- 1
delta <- 1
rho_peso <- 0.995  # Desconto mensal: 1 = pesos iguais; menor = mais peso ao recente
stopifnot(rho_peso > 0, rho_peso <= 1)
stopifnot(lambda > 0, mu > 0, delta > 0)

#3 Estimação dos hiperparametros da COVID
# COVID contada após as p observações iniciais usadas como defasagens
inicio_covid <- match(as.Date("2020-03-01"), as.Date(dados_boj$date)) - p
if (is.na(inicio_covid) || inicio_covid <= 0) inicio_covid <- NULL
# Estimar os quatro parâmetros COVID somente com quatro meses já observados
if (!is.null(inicio_covid) && nrow(dados_boj) - p - inicio_covid + 1L < 4L) inicio_covid <- NULL

# Respostas x choques x horizontes; posições 1:13 representam meses 0:12
# Variáveis: ust2y, brent, atividade, cpi_core, juro_curto, inclinacao, usdjpy
# Choques: Fed, petróleo, residual doméstico 1, residual doméstico 2, BoJ, prêmio fiscal/prazo, residual de fluxo/UIP
sinais_boj <- array(NA_real_, dim = c(7, 7, 13))

# BoJ (5): juro sobe, curva achata; atividade e CPI caem nos meses 3 a 12
sinais_boj[5, 5, 1:2] <- 1
sinais_boj[6, 5, 1:2] <- -1
sinais_boj[c(3, 4), 5, 4:13] <- -1

# Choques 3 e 4: residuais domesticos, sem sinais, como no teste 1 do CV

# Prêmio fiscal/de prazo (6): curva inclina e USDJPY sobe
# Esses sinais não provam uma causa fiscal exclusiva
sinais_boj[6, 6, 1:2] <- 1
sinais_boj[7, 6, 1:2] <- 1

# Fed (1): Treasury de 2 anos sobe e Brent cai; respostas japonesas livres
sinais_boj[1, 1, 1:2] <- 1
sinais_boj[2, 1, 1:2] <- -1

# Petróleo (2): Brent sobe; respostas japonesas livres

sinais_boj[2, 2, 1:2] <- 1

# Fluxo/UIP (7): residual, sem restricoes de sinal ou narrativas

# 4. Restrições Narrativas 

# Demanda global e risk-off não são identificados separadamente no modelo de sete variáveis

# 4.3. Fim do juro negativo e do YCC em março/2024: aperto do BoJ (choque 5).
# Decisão em 19/03/2024; o evento não prova que a surpresa mensal tenha sido positiva.
# Fonte: https://www.boj.or.jp/en/mopo/mpmdeci/state_2024/k240319a.htm
inicio_fim_ycc <- match(as.Date("2024-03-01"), as.Date(dados_boj$date))
narrativa_boj <- list()
if (!is.na(inicio_fim_ycc) && inicio_fim_ycc > p) {
  narrativa_boj <- list(bsvarSIGNs::specify_narrative(
    start = inicio_fim_ycc, periods = 1, type = "S", sign = 1, shock = 5
  ))
}

# 4.4. Eleição de Takaichi para a presidência do LDP em outubro/2025: prêmio positivo (choque 6).
# Eleição em 04/10/2025; o sinal fiscal/de prazo é a hipótese do plano, não um fato confirmado pela data.
# Fonte: https://www.jimin.jp/news/information/211511.html
inicio_takaichi <- match(as.Date("2025-10-01"), as.Date(dados_boj$date))
narrativa_takaichi <- list()
if (!is.na(inicio_takaichi) && inicio_takaichi > p) {
  narrativa_takaichi <- list(bsvarSIGNs::specify_narrative(
    start = inicio_takaichi, periods = 1, type = "S", sign = 1, shock = 6
  ))
}

narrativas <- c(narrativa_boj, narrativa_takaichi)

# Os choques identificados entram juntos na mesma especificação e na mesma estimação
spec_boj = specify_bsvarSIGN$new(
  data = matrix_interna,
  foreign = matrix_externa,
  exogenous = tendencia,
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
spec_boj$prior$hyper[1:3, ] <- c(mu, delta, lambda)

# 4. Pesos exponenciais, depois de construir as defasagens
# Multiplicar a equação inteira por sqrt(peso) dá menor precisão aos meses antigos.
Y_boj <- spec_boj$data_matrices$Y
X_boj <- spec_boj$data_matrices$X
pesos <- rho_peso^rev(seq_len(ncol(Y_boj)) - 1)
stopifnot(all(is.finite(pesos)), all(pesos > 0))
spec_boj$data_matrices$Y <- sweep(Y_boj, 2, sqrt(pesos), "*")
spec_boj$data_matrices$X <- sweep(X_boj, 2, sqrt(pesos), "*")
spec_boj$prior$Y <- spec_boj$data_matrices$Y
spec_boj$prior$X <- spec_boj$data_matrices$X

# Estimação e inspeção dos sorteios dos hiperparâmetros COVID com os mesmos pesos
set.seed(123)
if (!is.null(inicio_covid)) spec_boj$estimate_hyper(S = 5000, burn_in = 2500)

dim(spec_boj$prior$hyper)
hyper_draws = t(spec_boj$prior$hyper)

# Trajetórias dos quatro hiperparâmetros COVID estimados
if (!is.null(inicio_covid)) {
  hyper_covid_draws <- hyper_draws[, (ncol(hyper_draws) - 3):ncol(hyper_draws), drop = FALSE]
  colnames(hyper_covid_draws) <- c("COVID março", "COVID abril", "COVID maio", "Decaimento")
  plot.ts(hyper_covid_draws, nc = 1, col = "black",
          main = "Sorteios dos hiperparâmetros COVID", xlab = "Iteração")
}

# 5. Estimacao dos parametros 
spec_boj$identification$max_tries <- 1000
post_boj = bsvars::estimate(spec_boj, S = 2500, show_progress = TRUE)

# Recuperar o histórico original para o forecast.
post_boj$last_draw$data_matrices$Y <- Y_boj
post_boj$last_draw$data_matrices$X <- X_boj
post_boj$last_draw$prior$Y <- Y_boj
post_boj$last_draw$prior$X <- X_boj

# Recuperar as escalas e os choques de cada sorteio nas unidades do histórico.
# O pacote reamostra os parâmetros; recalcular mantém os choques alinhados a eles.
for (s in seq_len(dim(post_boj$posterior$A)[3])) {
  escala <- rep(1, ncol(Y_boj))
  covid <- tail(post_boj$posterior$hyper[, s], 4)
  if (!is.null(inicio_covid)) {
    escala[inicio_covid + 0:2] <- covid[1:3]
    if (inicio_covid + 3 <= length(escala)) {
      meses <- (inicio_covid + 3):length(escala)
      escala[meses] <- 1 + (covid[3] - 1) * covid[4]^seq_along(meses)
    }
  }
  escala <- escala / sqrt(pesos)
  post_boj$posterior$sigma[, , s] <- matrix(escala, nrow(Y_boj), ncol(Y_boj), byrow = TRUE)
  post_boj$posterior$shocks[, , s] <- post_boj$posterior$B[, , s] %*%
    sweep(Y_boj - post_boj$posterior$A[, , s] %*% X_boj, 2, escala, "/")
}
pesos_exponenciais <- tibble(date = dados_boj$date[-seq_len(p)], peso = pesos)
attr(post_boj, "pesos_exponenciais") <- list(rho = rho_peso, dados = pesos_exponenciais,
  metodo = "Precisão temporal: Var(erro_t) = escala_COVID_t^2 * Sigma / peso_t")

# 5.1. Checagem simples dos resultados da estimação
coeficientes_sorteados <- matrix(
  post_boj$posterior$A,
  ncol = dim(post_boj$posterior$A)[3]
)
lags_domesticos <- unlist(lapply(0:(p - 1), function(j) 3:7 + 7 * j))

diagnostico_estimacao <- tibble(
  sorteios = ncol(coeficientes_sorteados),
  valores_validos = all(is.finite(coeficientes_sorteados)) &&
    all(is.finite(post_boj$posterior$Sigma)),
  conjuntos_distintos = ncol(unique(coeficientes_sorteados, MARGIN = 2)),
  ess_ponderacao = post_boj$posterior$ess,
  coef_dom_nas_externas = max(abs(post_boj$posterior$A[1:2, lags_domesticos, ])),
  impacto_dom_nas_externas = max(abs(post_boj$posterior$Theta0[1:2, 3:7, ]))
)

diagnostico_estimacao

if (!diagnostico_estimacao$valores_validos || !is.finite(diagnostico_estimacao$ess_ponderacao) ||
    diagnostico_estimacao$ess_ponderacao < 50 || diagnostico_estimacao$conjuntos_distintos < 50 ||
    diagnostico_estimacao$coef_dom_nas_externas > 1e-8 || diagnostico_estimacao$impacto_dom_nas_externas > 1e-8) {
  stop("O ajuste nao passou pela checagem numerica; nao interpretar as previsoes.")
}

# IRFs e gráficos se localizam no script: src/Outputs/IRF 

# Previsões condicionais ex post no CV estrutural: Treasury de 2 anos e Brent realizados
# IP mundial e VIX não fazem parte desta especificação, como no teste 1 reduzido