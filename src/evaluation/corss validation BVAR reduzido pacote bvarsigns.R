# 9. Validação temporal: BVAR exógeno COVID (bsvarSIGNs), VAR e univariados
#TODO: outras métricas do plano



library(tidyverse)
library(fpp3)
library(fabletools)
library(distributional)
library(bsvars)
library(bsvarSIGNs)

# Mesmo calendário do CV original; previsões mensais até 24 meses.
p <- 6
h <- 24
stopifnot("Use o bsvarSIGNs 3.0 da biblioteca padrão do R." =
            as.character(packageVersion("bsvarSIGNs")) == "3.0")
# Correção das defasagens em R no bloco 9.3.2; pacote oficial sem modificações.

# Usar a amostra comum data_bal, preparada no feature_engineering.R.

# Lacunas internas alterariam o significado das defasagens e das datas previstas.
if (any(tsibble::has_gaps(data_bal)$.gaps)) {
  stop("A remoção dos NAs deixou meses ausentes no meio da amostra. Corrija essas lacunas antes do CV.")
}

cv_data <- data_bal |>
  stretch_tsibble(.init = 180, .step = 3)   # Primeira janela: 180 observações completas. (2000 até 2014)
                                            # Avançar três meses por janela para reduzir o tempo de estimação.

# 9.1. Modelos univariados de referência para o CPI
cv_fable <- cv_data |>
  model(
    ar    = ARIMA(cpi_core ~ pdq(d = 1, q = 0) + PDQ(D = 0, Q = 0)),
    naive = NAIVE(cpi_core),
    drift = RW(cpi_core ~ drift())
  )


# Calcular e guardar as métricas dos modelos univariados
acc_fable <- cv_fable |>
  forecast(h = h) |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))



# Mesmos benchmarks para a atividade, usando as mesmas janelas.
cv_fable_atividade <- cv_data |>
  model(
    ar = ARIMA(atividade ~ pdq(d = 1, q = 0) + PDQ(D = 0, Q = 0)),
    naive = NAIVE(atividade),
    drift = RW(atividade ~ drift())
  )

acc_fable_atividade <- cv_fable_atividade |>
  forecast(h = h) |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))

# 9.2. VAR de referência com atividade, CPI e juro curto
cv_var <- cv_data |>
  model(var = VAR(vars(atividade, cpi_core, juro_curto)))



# Prever o VAR uma única vez
previsoes_var <- cv_var |>
  forecast(h = h) |>
  filter(date <= max(data_bal$date))

# RMSE e MAE: calcular diretamente e selecionar o CPI
acc_var <- previsoes_var |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE)) |>
  filter(.response == "cpi_core")

# CRPS: extrair a distribuição marginal do CPI, a segunda variável do VAR
param_var_cv <- parameters(previsoes_var$.distribution)

previsoes_var_cpi <- tibble(
  .id = previsoes_var$.id,
  .model = previsoes_var$.model,
  date = previsoes_var$date,
  cpi_core = dist_normal(
    mu = map_dbl(param_var_cv$mu, 2),
    sigma = sqrt(map_dbl(param_var_cv$sigma, ~ .x[2, 2]))
  )
) |>
  as_tsibble(key = c(.id, .model), index = date) |>
  as_fable(response = "cpi_core", distribution = "cpi_core")

crps_var <- previsoes_var_cpi |>
  accuracy(data_bal, measures = list(CRPS = CRPS)) |>
  select(.model, .type, CRPS)

# Reunir as três métricas do CPI para a tabela comparativa
acc_var <- acc_var |>
  left_join(crps_var, by = c(".model", ".type"))

# A atividade é a primeira variável do mesmo VAR: extrair sua distribuição marginal.
previsoes_var_atividade <- tibble(
  .id = previsoes_var$.id,
  .model = previsoes_var$.model,
  date = previsoes_var$date,
  atividade = dist_normal(
    mu = map_dbl(param_var_cv$mu, 1),
    sigma = sqrt(map_dbl(param_var_cv$sigma, ~ .x[1, 1]))
  )
) |>
  as_tsibble(key = c(.id, .model), index = date) |>
  as_fable(response = "atividade", distribution = "atividade")

acc_var_atividade <- previsoes_var_atividade |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))

# 9.3. Validação temporal do BVAR exógeno com COVID
# Reestimar no treino; lambda/mu/delta/psi fixos e apenas os quatro parâmetros COVID estimados.

set.seed(123)

janelas <- unique(cv_data$.id)
resultados_bvar <- vector("list", length(janelas))
resultados_bvar_atividade <- vector("list", length(janelas))

for (i in seq_along(janelas)) {

  # 9.3.1. Preparar a janela de treino
  treino <- cv_data |>
    filter(.id == janelas[i]) |>
    as_tibble() |>
    arrange(date)

  origem <- max(treino$date)
  if (origem >= max(data_bal$date)) next

  message("Janela ", i, "/", length(janelas), " - origem: ", origem)

  dados_treino <- treino |>
    select(
      ust2y, ip_mundial, brent, vix,
      atividade, cpi_core, juro_curto,
      inclinacao, usdjpy
    ) |>
    as.matrix()

  pos_cpi <- match("cpi_core", colnames(dados_treino))
  pos_atividade <- match("atividade", colnames(dados_treino))

  # 9.3.2. Estimar o BVAR exógeno com COVID, como no script do modelo
  matrix_externa <- dados_treino[, 1:4, drop = FALSE]
  matrix_interna <- dados_treino[, 5:9, drop = FALSE]

  inicio_covid <- match(as.Date("2020-03-01"), as.Date(treino$date)) - p
  if (is.na(inicio_covid) || inicio_covid <= 0) inicio_covid <- NULL
  # Esperar quatro meses observados para estimar o perfil COVID de quatro parâmetros.
  if (!is.null(inicio_covid) && nrow(treino) - p - inicio_covid + 1L < 4L) inicio_covid <- NULL

  set.seed(123 + i)
  spec_cv <- bsvarSIGNs::specify_bsvarSIGN$new(
    data = matrix_interna,
    foreign = matrix_externa,
    p = p,
    hyper_mu = FALSE,
    hyper_delta = FALSE,
    hyper_lambda = FALSE,
    hyper_psi = FALSE,
    hyper_covid = inicio_covid,
    sign_structural = matrix(NA_real_, 9, 9)
  )
  if (!is.null(inicio_covid)) spec_cv$estimate_hyper(S = 5000, burn_in = 2500)
  post_cv <- bsvars::estimate(spec_cv, S = 2500, show_progress = FALSE)

  set.seed(123)
  previsao_cv <- bsvarSIGNs::forecast(post_cv, horizon = h)

  # Correção em R: manter os choques nativos, iniciar no último dado e deslocar os lags.
  # Sem regressores exógenos adicionais; a escala COVID dos choques vem do pacote.
  choques_cv <- previsao_cv$forecasts - previsao_cv$forecast_mean
  stopifnot(all(is.finite(choques_cv)))
  estado_inicial_cv <- c(t(dados_treino[nrow(dados_treino) - (0:(p - 1)), , drop = FALSE]), 1)
  for (s in seq_len(dim(previsao_cv$forecasts)[3])) {
    estado_cv <- estado_inicial_cv
    for (j in seq_len(h)) {
      previsao_cv$forecast_mean[, j, s] <- drop(post_cv$posterior$A[, , s] %*% estado_cv)
      previsao_cv$forecasts[, j, s] <- previsao_cv$forecast_mean[, j, s] + choques_cv[, j, s]
      estado_cv <- c(previsao_cv$forecasts[, j, s], head(estado_cv, ncol(dados_treino) * (p - 1)), 1)
    }
  }
  # forecasts: variáveis (externas primeiro) x horizontes x sorteios.
  sorteios_cpi <- matrix(previsao_cv$forecasts[pos_cpi, , ], nrow = h)

  resultados_bvar[[i]] <- tibble(
    .id = janelas[i],
    .model = "BVAR exógeno COVID",
    date = origem + seq_len(h),
    h = seq_len(h),
    sorteios = split(sorteios_cpi, row(sorteios_cpi))
  )

  # Avaliar a atividade usando os mesmos sorteios do BVAR exógeno.
  sorteios_atividade <- matrix(previsao_cv$forecasts[pos_atividade, , ], nrow = h)
  resultados_bvar_atividade[[i]] <- tibble(
    .id = janelas[i],
    .model = "BVAR exógeno COVID",
    date = origem + seq_len(h),
    h = seq_len(h),
    sorteios = split(sorteios_atividade, row(sorteios_atividade))
  )
}

# 9.3.3. Adaptar as distribuições marginais do CPI para accuracy()
previsoes_bvar <- bind_rows(resultados_bvar) |>
  mutate(
    cpi_core = dist_sample(sorteios),
    .mean = mean(cpi_core)
  ) |>
  select(-sorteios) |>
  filter(date <= max(data_bal$date)) |>
  as_tsibble(key = c(.id, .model), index = date) |>
  as_fable(response = "cpi_core", distribution = "cpi_core")

acc_bvar <- previsoes_bvar |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))


# Adaptar também as distribuições marginais da atividade.
previsoes_bvar_atividade <- bind_rows(resultados_bvar_atividade) |>
  mutate(
    atividade = dist_sample(sorteios),
    .mean = mean(atividade)
  ) |>
  select(-sorteios) |>
  filter(date <= max(data_bal$date)) |>
  as_tsibble(key = c(.id, .model), index = date) |>
  as_fable(response = "atividade", distribution = "atividade")

acc_bvar_atividade <- previsoes_bvar_atividade |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))

# 9.4. Comparar as métricas de previsão do CPI
cv_results <- bind_rows(
  acc_fable,
  acc_var,
  acc_bvar
) |>
  select(.model, .type, RMSE, MAE, CRPS) |>
  arrange(RMSE)

cv_results





cv_results |>
  select(.model, RMSE, MAE, CRPS) |>
  pivot_longer(-.model, names_to = "metrica", values_to = "valor") |>
  ggplot(aes(x = .model, y = valor, fill = .model)) +
  geom_col(na.rm = TRUE) +
  facet_wrap(~ metrica, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "CV do CPI — menor é melhor",
    x = NULL, y = NULL
  )

# 9.5. Comparar as métricas de previsão da atividade
cv_results_atividade <- bind_rows(
  acc_fable_atividade,
  acc_var_atividade,
  acc_bvar_atividade
) |>
  select(.model, .type, RMSE, MAE, CRPS) |>
  arrange(RMSE)

cv_results_atividade

cv_results_atividade |>
  select(.model, RMSE, MAE, CRPS) |>
  pivot_longer(-.model, names_to = "metrica", values_to = "valor") |>
  ggplot(aes(x = .model, y = valor, fill = .model)) +
  geom_col(na.rm = TRUE) +
  facet_wrap(~ metrica, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "CV da atividade — menor é melhor",
    x = NULL, y = NULL
  )
