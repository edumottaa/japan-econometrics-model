# 10. Avaliação com uma amostra final de teste

library(tidyverse)
library(fpp3)
library(fabletools)
library(distributional)
library(BVAR)

# Reservar os últimos 24 meses para teste
h <- 24

treino <- data_bal |>
  slice_head(n = nrow(data_bal) - h)

teste <- data_bal |>
  slice_tail(n = h)

medidas <- list(RMSE = RMSE, MAE = MAE, CRPS = CRPS)


# 10.1. Modelos univariados de referência
fit_fable <- treino |>
  model(
    ar = ARIMA(cpi_core ~ pdq(d = 1, q = 0) + PDQ(D = 0, Q = 0)),
    naive = NAIVE(cpi_core),
    drift = RW(cpi_core ~ drift())
  )

prev_fable <- fit_fable |>
  forecast(h = h)

# 10.2. VAR de referência
fit_var <- treino |>
  model(var = VAR(vars(atividade, cpi_core, juro_curto)))

prev_var <- fit_var |>
  forecast(h = h)

# Extrair a distribuição marginal do CPI para calcular o CRPS do VAR
param_var <- parameters(prev_var$.distribution)

# O CPI é a segunda variável na fórmula do VAR
prev_var_cpi <- tibble(
  .model = prev_var$.model,
  date = prev_var$date,
  cpi_core = dist_normal(
    mu = map_dbl(param_var$mu, 2),
    sigma = sqrt(map_dbl(param_var$sigma, ~ .x[2, 2]))
  )
) |>
  as_tsibble(key = .model, index = date) |>
  as_fable(response = "cpi_core", distribution = "cpi_core")





# 10.3. Estimar o BVAR reduzido com as dez variáveis
dados_treino <- treino |>
  as_tibble() |>
  select(
    ust2y, ip_mundial, brent, vix,
    atividade, cpi_core, juro_curto,
    inclinacao, usdjpy, topix
  ) |>
  as.matrix()

# Estimar lambda, SOC e SUR no treino; psi automático e fixo na estimação.
priors_holdout <- bv_priors(
  hyper = c("lambda", "soc", "sur"),
  mn = bv_minnesota(lambda = bv_lambda(), psi = bv_psi()),
  soc = bv_soc(),
  sur = bv_sur(min = 0.001)
)

mh_holdout <- bv_mh(
  adjust_acc = TRUE,
  acc_lower = 0.2,
  acc_upper = 0.4,
  acc_change = 0.1
)

set.seed(123)
post_holdout <- bvar(
  dados_treino, lags = 6, n_draw = 5000, n_burn = 2500,
  n_thin = 1, priors = priors_holdout, mh = mh_holdout
)

# 10.4. Previsão do BVAR reduzido sem restrição de exogeneidade
set.seed(123)
prev_bvar <- predict(post_holdout, horizon = h)

# fcast: sorteios x horizontes x variáveis; selecionar a marginal do CPI.
pos_cpi <- match("cpi_core", colnames(dados_treino))
sorteios_cpi <- t(prev_bvar$fcast[, , pos_cpi])

prev_bvar_cpi <- tibble(
  .model = "BVAR",
  date = teste$date,
  cpi_core = dist_sample(split(sorteios_cpi, row(sorteios_cpi)))
) |>
  as_tsibble(key = .model, index = date) |>
  as_fable(response = "cpi_core", distribution = "cpi_core")

# 10.5. Previsão do mesmo BVAR condicionada ao VAR externo
# Estimar o VAR externo e selecionar os lags por AICc apenas no treino.
previsoes_externas_holdout <- treino |>
  model(aicc = VAR(vars(ip_mundial, brent, vix, ust2y))) |>
  forecast(h = h)

caminhos_externos_holdout <- previsoes_externas_holdout |>
  as_tibble() |>
  arrange(date) |>
  pull(.mean)

# Usar médias previstas, sem os valores externos observados no teste.
# Esta versão trata as trajetórias como fixas, sem transmitir sua incerteza.
# Reutilizar os coeficientes já estimados para comparar o efeito do condicionamento.
set.seed(123)
prev_bvar_condicionado <- predict(
  post_holdout,
  horizon = h,
  cond_path = caminhos_externos_holdout,
  cond_vars = colnames(caminhos_externos_holdout)
)

sorteios_cpi <- t(prev_bvar_condicionado$fcast[, , pos_cpi])

prev_bvar_condicionado_cpi <- tibble(
  .model = "BVAR condicionado",
  date = teste$date,
  cpi_core = dist_sample(split(sorteios_cpi, row(sorteios_cpi)))
) |>
  as_tsibble(key = .model, index = date) |>
  as_fable(response = "cpi_core", distribution = "cpi_core")

# 10.6. Comparar as métricas do CPI nos mesmos 24 meses de teste
acc_fable <- prev_fable |>
  accuracy(data_bal, measures = medidas)

acc_var <- prev_var_cpi |>
  accuracy(data_bal, measures = medidas)

acc_bvar <- prev_bvar_cpi |>
  accuracy(data_bal, measures = medidas)

acc_bvar_condicionado <- prev_bvar_condicionado_cpi |>
  accuracy(data_bal, measures = medidas)

holdout_results <- bind_rows(
  acc_fable, acc_var, acc_bvar, acc_bvar_condicionado
) |>
  select(.model, .type, RMSE, MAE, CRPS) |>
  arrange(RMSE)

holdout_results

# 10.7. Avaliar a atividade no mesmo holdout
# Reutilizar treino, teste e previsões do VAR e dos dois BVARs.
# Estimar apenas os benchmarks univariados específicos da atividade.
fit_fable_atividade <- treino |>
  model(
    ar = ARIMA(atividade ~ pdq(d = 1, q = 0) + PDQ(D = 0, Q = 0)),
    naive = NAIVE(atividade),
    drift = RW(atividade ~ drift())
  )

prev_fable_atividade <- fit_fable_atividade |>
  forecast(h = h)

# A atividade é a primeira variável na fórmula do VAR doméstico.
prev_var_atividade <- tibble(
  .model = prev_var$.model,
  date = prev_var$date,
  atividade = dist_normal(
    mu = map_dbl(param_var$mu, 1),
    sigma = sqrt(map_dbl(param_var$sigma, ~ .x[1, 1]))
  )
) |>
  as_tsibble(key = .model, index = date) |>
  as_fable(response = "atividade", distribution = "atividade")

# Selecionar os sorteios da atividade no BVAR sem condicionamento.
pos_atividade <- match("atividade", colnames(dados_treino))
sorteios_atividade <- t(prev_bvar$fcast[, , pos_atividade])

prev_bvar_atividade <- tibble(
  .model = "BVAR",
  date = teste$date,
  atividade = dist_sample(split(sorteios_atividade, row(sorteios_atividade)))
) |>
  as_tsibble(key = .model, index = date) |>
  as_fable(response = "atividade", distribution = "atividade")

# Selecionar a mesma variável nas previsões condicionadas ao VAR externo.
sorteios_atividade <- t(prev_bvar_condicionado$fcast[, , pos_atividade])

prev_bvar_condicionado_atividade <- tibble(
  .model = "BVAR condicionado",
  date = teste$date,
  atividade = dist_sample(split(sorteios_atividade, row(sorteios_atividade)))
) |>
  as_tsibble(key = .model, index = date) |>
  as_fable(response = "atividade", distribution = "atividade")

acc_fable_atividade <- prev_fable_atividade |>
  accuracy(data_bal, measures = medidas)

acc_var_atividade <- prev_var_atividade |>
  accuracy(data_bal, measures = medidas)

acc_bvar_atividade <- prev_bvar_atividade |>
  accuracy(data_bal, measures = medidas)

acc_bvar_condicionado_atividade <- prev_bvar_condicionado_atividade |>
  accuracy(data_bal, measures = medidas)

# Tabela separada: atividade e CPI estão em escalas diferentes.
holdout_results_atividade <- bind_rows(
  acc_fable_atividade, acc_var_atividade,
  acc_bvar_atividade, acc_bvar_condicionado_atividade
) |>
  select(.model, .type, RMSE, MAE, CRPS) |>
  arrange(RMSE)

holdout_results_atividade
