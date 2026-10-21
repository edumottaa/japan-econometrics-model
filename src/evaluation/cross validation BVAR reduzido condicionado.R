# 9. Validação temporal: BVAR condicionado (BVAR), VAR e univariados
#TODO: outras métricas do plano



library(tidyverse)
library(fpp3)
library(fabletools)
library(distributional)
library(BVAR)

# Mesmo calendário do CV original; previsões mensais até 24 meses.
p <- 6
h <- 24

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

# 9.3. Validação temporal do BVAR condicionado
# Lambda/SOC/SUR estimados no treino; psi fixo, sem tratamento direto de COVID.

priors_cv <- bv_priors(
  hyper = c("lambda", "soc", "sur"),
  mn = bv_minnesota(lambda = bv_lambda(), psi = bv_psi()),
  soc = bv_soc(),
  sur = bv_sur(min = 0.001)
)

mh_cv <- bv_mh(
  adjust_acc = TRUE,
  acc_lower = 0.2,
  acc_upper = 0.4,
  acc_change = 0.1
)

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

  # 9.3.2. Estimar o BVAR condicionado pelo pacote BVAR
  set.seed(123 + i)
  post_condicionado_cv <- BVAR::bvar(
    dados_treino, lags = p, n_draw = 5000,
    n_burn = 2500, n_thin = 1, priors = priors_cv, mh = mh_cv
  )

  # Selecionar os lags do VAR externo por AICc somente no treino desta janela.
  # Não usar o var_ex da amostra completa nem valores externos realizados no teste.
  previsoes_externas_cv <- treino |>
    select(-.id) |>
    as_tsibble(index = date) |>
    model(aicc = VAR(vars(ip_mundial, brent, vix, ust2y))) |>
    forecast(h = h)

  caminhos_externos_cv <- previsoes_externas_cv |>
    as_tibble() |>
    arrange(date) |>
    pull(.mean)

  # As trajetórias médias são fixas: não propagam a incerteza do VAR externo.
  set.seed(123)
  previsao_condicionada_cv <- stats::predict(
    post_condicionado_cv,
    horizon = h,
    cond_path = caminhos_externos_cv,
    cond_vars = colnames(caminhos_externos_cv)
  )

  # fcast: sorteios x horizontes x variáveis.
  sorteios_cpi <- t(previsao_condicionada_cv$fcast[, , pos_cpi])
  resultados_bvar[[i]] <- tibble(
    .id = janelas[i],
    .model = "BVAR condicionado",
    date = origem + seq_len(h),
    h = seq_len(h),
    sorteios = split(sorteios_cpi, row(sorteios_cpi))
  )

  sorteios_atividade <- t(previsao_condicionada_cv$fcast[, , pos_atividade])
  resultados_bvar_atividade[[i]] <- tibble(
    .id = janelas[i],
    .model = "BVAR condicionado",
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
  select(.model, RMSE, CRPS) |>
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
  select(.model, RMSE, CRPS) |>
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
