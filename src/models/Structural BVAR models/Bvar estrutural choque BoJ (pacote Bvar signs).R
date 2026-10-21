


library(tidyverse)
library(bsvars)
library(bsvarSIGNs)




# 1. Construção das matrizes dos blocos externo e doméstico

matrix_externa <- data_bal |>
  as_tibble() |>
  select(ust2y, ip_mundial, brent, vix) |>
  as.matrix()

matrix_interna <- data_bal |>
  as_tibble() |>
  select(atividade, cpi_core, juro_curto, inclinacao, usdjpy) |>
  as.matrix()


#2. Seleção dos lags
p <- 6  


#3 Estimação dos hiperparametros da COVID
# COVID contada após as p observações iniciais usadas como defasagens.
inicio_covid <- match(as.Date("2020-03-01"), as.Date(data_bal$date)) - p
if (is.na(inicio_covid) || inicio_covid <= 0) inicio_covid <- NULL
# Estimar os quatro parâmetros COVID somente com quatro meses já observados.
if (!is.null(inicio_covid) && nrow(data_bal) - p - inicio_covid + 1L < 4L) inicio_covid <- NULL




#  3 Especificação do BVAR estrutural - choques domésticos e externos: 
# Restrições de sinal: variável que responde x choque x horizonte.
# Ordem: ust2y, ip_mundial, brent, vix, atividade, cpi_core, juro_curto, inclinacao, usdjpy.



#  3.1 choque monetário do BoJ (coluna 7)


# Posição 1 = impacto (h=0); posição 13 = mês 12.
sinais_boj <- array(NA_real_, dim = c(9, 9, 13))
sinais_boj[7, 7, 1:2] <-  1         # Juro sobe no impacto e no mês seguinte.
sinais_boj[8, 7, 1:2] <- -1         # Inclinação cai nesses dois meses.
sinais_boj[c(5, 6), 7, 4:13] <- -1   # Atividade e CPI caem nos meses 3 a 12.

# USDJPY fica livre no choque do BoJ; as narrativas são definidas adiante.



# 3.2. Choque positivo de demanda doméstica (coluna 5)
# Impacto e mês seguinte: atividade, CPI e juro curto sobem, conforme o plano.
sinais_boj[5, 5, 1:2] <- 1
sinais_boj[6, 5, 1:2] <- 1
sinais_boj[7, 5, 1:2] <- 1

# 3.3. Choque favorável de oferta doméstica (coluna 6)
# Impacto e mês seguinte: atividade sobe e CPI cai; demais respostas ficam livres.
sinais_boj[5, 6, 1:2] <-  1
sinais_boj[6, 6, 1:2] <- -1

# 3.4. Choque de prêmio fiscal/de prazo (coluna 8)
# Impacto e mês seguinte: curva inclina e o iene deprecia (USDJPY sobe).
# Esses sinais são compatíveis com prêmio fiscal/de prazo, sem provar uma causa fiscal exclusiva.
sinais_boj[8, 8, 1:2] <- 1
sinais_boj[9, 8, 1:2] <- 1


# 3.5. Choque positivo de demanda global (coluna 1, bloco externo)
# Impacto e mês seguinte: US2y, produção mundial e petróleo sobem; VIX cai.
sinais_boj[1:3, 1, 1:2] <-  1
sinais_boj[4, 1, 1:2] <- -1
# As respostas japonesas à demanda global ficam livres, conforme o plano.


# 3.6. Choque adverso de oferta de petróleo (coluna 2)
# Brent sobe no impacto e no mês seguinte; IP mundial cai entre os meses 1 e 6.
sinais_boj[3, 2, 1:2] <-  1
sinais_boj[2, 2, 2:7] <- -1

# 3.7. Choque de aperto monetário do Fed (coluna 3)
# Impacto e mês seguinte: US2y e VIX sobem, Brent cai.
sinais_boj[1, 3, 1:2] <-  1
sinais_boj[4, 3, 1:2] <-  1
sinais_boj[3, 3, 1:2] <- -1
sinais_boj[2, 3, 4:13] <- -1  # IP mundial cai nos meses 3 a 12.

# 3.8. Choque de risk-off global (coluna 4)
# Impacto e mês seguinte: US2y e Brent caem, VIX sobe.
sinais_boj[c(1, 3), 4, 1:2] <- -1
sinais_boj[4, 4, 1:2] <- 1



# 3.9. Choque de fluxo/UIP do iene (coluna 9)
# Positivo: USDJPY sobe e a inclinação cai no impacto e no mês seguinte.
# CPI sobe nos meses 3 a 12; demais respostas ficam livres, sem TOPIX.
sinais_boj[9, 9, 1:2] <-  1
sinais_boj[8, 9, 1:2] <- -1
sinais_boj[6, 9, 4:13] <- 1





# 4. Restrições Narrativas 

# 4.1 desmonte do carry em agosto/2024, risk-off positivo.
# Fonte: BIS Bulletin 90, publicado em 27/08/2024.
# https://www.bis.org/publications/bulletin-90-market-turbulence-and-carry-trade-unwind-august-2024
# Hipótese de identificação: sinal positivo, sem impor magnitude nem dominância.
inicio_carry <- match(as.Date("2024-08-01"), as.Date(data_bal$date))
narrativa_carry <- list()
if (!is.na(inicio_carry) && inicio_carry > p) {
  # start usa a posição na amostra original; o pacote desconta p internamente.
  narrativa_carry <- list(bsvarSIGNs::specify_narrative(
    start = inicio_carry, periods = 1, type = "S", sign = 1, shock = 4
  ))
}
# Se agosto/2024 ainda não estiver na amostra, a narrativa não é aplicada.

# 4.2. Tarifas americanas de abril/2025: risk-off positivo (choque 4).
# Anúncio em 02/04/2025; a restrição mensal é uma hipótese, apesar das reversões no mês.
# Fonte: https://www.bis.org/publications/qr-202509/understanding-swift-market-recovery-after-april-2025-tariff-shock
inicio_tarifas <- match(as.Date("2025-04-01"), as.Date(data_bal$date))
narrativa_tarifas <- list()
if (!is.na(inicio_tarifas) && inicio_tarifas > p) {
  narrativa_tarifas <- list(bsvarSIGNs::specify_narrative(
    start = inicio_tarifas, periods = 1, type = "S", sign = 1, shock = 4
  ))
}

# 4.3. Fim do juro negativo e do YCC em março/2024: aperto do BoJ (choque 7).
# Decisão em 19/03/2024; o evento não prova que a surpresa mensal tenha sido positiva.
# Fonte: https://www.boj.or.jp/en/mopo/mpmdeci/state_2024/k240319a.htm
inicio_fim_ycc <- match(as.Date("2024-03-01"), as.Date(data_bal$date))
narrativa_boj <- list()
if (!is.na(inicio_fim_ycc) && inicio_fim_ycc > p) {
  narrativa_boj <- list(bsvarSIGNs::specify_narrative(
    start = inicio_fim_ycc, periods = 1, type = "S", sign = 1, shock = 7
  ))
}

# 4.4. Eleição de Takaichi para a presidência do LDP em outubro/2025: prêmio positivo (choque 8).
# Eleição em 04/10/2025; o sinal fiscal/de prazo é a hipótese do plano, não um fato confirmado pela data.
# Fonte: https://www.jimin.jp/news/information/211511.html
inicio_takaichi <- match(as.Date("2025-10-01"), as.Date(data_bal$date))
narrativa_takaichi <- list()
if (!is.na(inicio_takaichi) && inicio_takaichi > p) {
  narrativa_takaichi <- list(bsvarSIGNs::specify_narrative(
    start = inicio_takaichi, periods = 1, type = "S", sign = 1, shock = 8
  ))
}

# 4.5. Intervenções do MoF: compra de ienes, fluxo negativo (choque 9).
# Meses: set/out de 2022 e abr/mai/jul de 2024; uma restrição por mês.
# Fonte: https://www.mof.go.jp/english/policy/international_policy/reference/feio/foreign_exchange_intervention_operations.csv
meses_mof <- as.Date(c("2022-09-01", "2022-10-01", "2024-04-01", "2024-05-01", "2024-07-01"))
inicio_mof <- match(meses_mof, as.Date(data_bal$date))
inicio_mof <- inicio_mof[!is.na(inicio_mof) & inicio_mof > p]
narrativa_mof <- list()
for (i in seq_along(inicio_mof)) {
  narrativa_mof[[i]] <- bsvarSIGNs::specify_narrative(
    start = inicio_mof[i], periods = 1, type = "S", sign = -1, shock = 9
  )
}


narrativas <- c(narrativa_boj, narrativa_carry, narrativa_tarifas, narrativa_takaichi, narrativa_mof)

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
  sign_structural = matrix(NA_real_, 9, 9)
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

# 5. Estimação conjunta dos nove choques, com as narrativas do plano
post_boj = bsvars::estimate(spec_boj, S = 2500, show_progress = FALSE)

# 5.1. Checagem simples dos resultados da estimação
coeficientes_sorteados <- matrix(
  post_boj$posterior$A,
  ncol = dim(post_boj$posterior$A)[3]
)
lags_domesticos <- unlist(lapply(0:(p - 1), function(j) 5:9 + 9 * j))

diagnostico_estimacao <- tibble(
  sorteios = ncol(coeficientes_sorteados),
  valores_validos = all(is.finite(coeficientes_sorteados)) &&
    all(is.finite(post_boj$posterior$Sigma)),
  conjuntos_distintos = ncol(unique(coeficientes_sorteados, MARGIN = 2)),
  ess_ponderacao = post_boj$posterior$ess,
  coef_dom_nas_externas = max(abs(post_boj$posterior$A[1:4, lags_domesticos, ])),
  impacto_dom_nas_externas = max(abs(post_boj$posterior$Theta0[1:4, 5:9, ]))
)

diagnostico_estimacao

if (!diagnostico_estimacao$valores_validos || !is.finite(diagnostico_estimacao$ess_ponderacao) ||
    diagnostico_estimacao$ess_ponderacao < 50 || diagnostico_estimacao$conjuntos_distintos < 50 ||
    diagnostico_estimacao$coef_dom_nas_externas > 1e-8 || diagnostico_estimacao$impacto_dom_nas_externas > 1e-8) {
  stop("O ajuste nao passou pela checagem numerica; nao interpretar as previsoes.")
}

# IRFs e gráficos se localizam no script: src/Outputs/IRF 