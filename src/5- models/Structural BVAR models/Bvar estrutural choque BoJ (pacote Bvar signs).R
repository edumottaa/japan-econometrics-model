


library(tidyverse)
library(bsvars)
library(bsvarSIGNs)




# 1. Construção das matrizes dos blocos externo e doméstico

matrix_externa <- data_bal |>
  as_tibble() |>
  select(ust2y, brent) |>
  as.matrix()

matrix_interna <- data_bal |>
  as_tibble() |>
  select(atividade, cpi_core, juro_curto, inclinacao, usdjpy) |>
  as.matrix()

  
#2. Seis defasagens mensais; sete variáveis e sete choques estruturais.
p <- 6  


#3 Estimação dos hiperparametros da COVID
# COVID contada após as p observações iniciais usadas como defasagens.
inicio_covid <- match(as.Date("2020-03-01"), as.Date(data_bal$date)) - p
if (is.na(inicio_covid) || inicio_covid <= 0) inicio_covid <- NULL
# Estimar os quatro parâmetros COVID somente com quatro meses já observados.
if (!is.null(inicio_covid) && nrow(data_bal) - p - inicio_covid + 1L < 4L) inicio_covid <- NULL




# Respostas x choques x horizontes; posições 1:13 representam meses 0:12.
# Variáveis: ust2y, brent, atividade, cpi_core, juro_curto, inclinacao, usdjpy.
# Choques: Fed, petróleo, demanda doméstica, oferta doméstica, BoJ, prêmio fiscal/prazo, fluxo/UIP.
sinais_boj <- array(NA_real_, dim = c(7, 7, 13))

# BoJ (5): juro sobe, curva achata; atividade e CPI caem nos meses 3 a 12.
sinais_boj[5, 5, 1:2] <- 1
sinais_boj[6, 5, 1:2] <- -1
sinais_boj[c(3, 4), 5, 4:13] <- -1

# Demanda doméstica (3): atividade, CPI e juro sobem.
sinais_boj[3, 3, 1:2] <- 1
sinais_boj[4, 3, 1:2] <- 1
sinais_boj[5, 3, 1:2] <- 1

# Oferta doméstica (4): atividade sobe e CPI cai.
sinais_boj[3, 4, 1:2] <- 1
sinais_boj[4, 4, 1:2] <- -1

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

# Fluxo/UIP (7): USDJPY sobe, inclinação cai e CPI sobe nos meses 3 a 12.
sinais_boj[7, 7, 1:2] <- 1
sinais_boj[6, 7, 1:2] <- -1
sinais_boj[4, 7, 4:13] <- 1

# 4. Restrições Narrativas 

# Demanda global e risk-off não são identificados separadamente no modelo de sete variáveis.

# 4.3. Fim do juro negativo e do YCC em março/2024: aperto do BoJ (choque 5).
# Decisão em 19/03/2024; o evento não prova que a surpresa mensal tenha sido positiva.
# Fonte: https://www.boj.or.jp/en/mopo/mpmdeci/state_2024/k240319a.htm
inicio_fim_ycc <- match(as.Date("2024-03-01"), as.Date(data_bal$date))
narrativa_boj <- list()
if (!is.na(inicio_fim_ycc) && inicio_fim_ycc > p) {
  narrativa_boj <- list(bsvarSIGNs::specify_narrative(
    start = inicio_fim_ycc, periods = 1, type = "S", sign = 1, shock = 5
  ))
}

# 4.4. Eleição de Takaichi para a presidência do LDP em outubro/2025: prêmio positivo (choque 6).
# Eleição em 04/10/2025; o sinal fiscal/de prazo é a hipótese do plano, não um fato confirmado pela data.
# Fonte: https://www.jimin.jp/news/information/211511.html
inicio_takaichi <- match(as.Date("2025-10-01"), as.Date(data_bal$date))
narrativa_takaichi <- list()
if (!is.na(inicio_takaichi) && inicio_takaichi > p) {
  narrativa_takaichi <- list(bsvarSIGNs::specify_narrative(
    start = inicio_takaichi, periods = 1, type = "S", sign = 1, shock = 6
  ))
}

# 4.5. Intervenções do MoF: compra de ienes, fluxo negativo (choque 7).
# Meses: set/out de 2022 e abr/mai/jul de 2024; uma restrição por mês.
# Fonte: https://www.mof.go.jp/english/policy/international_policy/reference/feio/foreign_exchange_intervention_operations.csv
meses_mof <- as.Date(c("2022-09-01", "2022-10-01", "2024-04-01", "2024-05-01", "2024-07-01"))
inicio_mof <- match(meses_mof, as.Date(data_bal$date))
inicio_mof <- inicio_mof[!is.na(inicio_mof) & inicio_mof > p]
narrativa_mof <- list()
for (i in seq_along(inicio_mof)) {
  narrativa_mof[[i]] <- bsvarSIGNs::specify_narrative(
    start = inicio_mof[i], periods = 1, type = "S", sign = -1, shock = 7
  )
}


narrativas <- c(narrativa_boj, narrativa_takaichi, narrativa_mof)

# Os choques identificados entram juntos na mesma especificação e na mesma estimação.
spec_boj = specify_bsvarSIGN$new(
  data = matrix_interna,
  foreign = matrix_externa,
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

# 4. Estimação e inspeção dos sorteios dos hiperparâmetros
set.seed(123)
if (!is.null(inicio_covid)) spec_boj$estimate_hyper(S = 5000, burn_in = 2500)

dim(spec_boj$prior$hyper)
hyper_draws = t(spec_boj$prior$hyper)

# Trajetórias dos quatro hiperparâmetros COVID estimados, após o aquecimento.
if (!is.null(inicio_covid)) {
  hyper_covid_draws <- hyper_draws[, (ncol(hyper_draws) - 3):ncol(hyper_draws), drop = FALSE]
  colnames(hyper_covid_draws) <- c("COVID março", "COVID abril", "COVID maio", "Decaimento")
  plot.ts(hyper_covid_draws, nc = 1, col = "black",
          main = "Sorteios dos hiperparâmetros COVID", xlab = "Iteração")
}

# 5. Estimação conjunta dos sete choques, com as narrativas do plano
post_boj = bsvars::estimate(spec_boj, S = 2500, show_progress = FALSE)

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

# Previsões condicionais ex post no CV estrutural: Treasury de 2 anos e Brent realizados.
# IP mundial e VIX não fazem parte desta especificação, como no teste 1 reduzido.