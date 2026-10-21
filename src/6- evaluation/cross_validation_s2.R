# Validação temporal do S2: três curvas de Phillips contra ARIMA puro e NAIVE
# Mesmo método do código dos EUA (seção 8.3): stretch_tsibble, new_data + left_join,
# forecast() e accuracy().
# Previsão ex-ante: só a informação disponível no fim de cada janela de treino.
# Regressores futuros = último valor da janela (NAIVE), como nos cenários do código dos EUA.
# Executar antes os scripts "src/models/s2 curva de phillips.R" (usa dados_trimestral,
# dados_mensal, dados_longa, pi_core_core_oficial e a função dummy_12m) e
# "src/models/s2 teste de defasagens.R" (fórmulas com as defasagens escolhidas).
# phillips: todos os regressores no mesmo período; phillips_lags: defasagens escolhidas pelo AICc.
# stepwise = FALSE nas curvas de Phillips: a busca rápida dos erros ARIMA falha com inflação persistente.

library(fpp3)

# Métricas da seção 5.9 do FPP3, como no código dos EUA
medidas <- list(
  RMSE       = RMSE,
  MAE        = MAE,
  CRPS       = CRPS,
  Winkler_80 = winkler_score,
  Skill_CRPS = skill_score(CRPS)
)

# Todas as janelas começam a prever depois de dez/2021: o mesmo período de teste nos três modelos.
# As últimas janelas, sem h períodos observados à frente, são descartadas.


# 1. Trimestral (core-core sem fatores institucionais, Tankan)

h_trim <- 8   # 8 trimestres = 2 anos

cv_trim <- dados_trimestral |>
  stretch_tsibble(.init = sum(dados_trimestral$date <= yearquarter("2021 Q4")), .step = 1) |>
  filter(.id <= max(.id) - h_trim)

# Regressores futuros: último valor de cada janela (.id), repetido nos h trimestres
ultimo_trim <- cv_trim |>
  as_tibble() |>
  group_by(.id) |>
  slice_tail(n = 1) |>
  ungroup() |>
  select(.id, expectativa_1ano_tankan, hiato, importacao)

cv_future_trim <- new_data(cv_trim, h_trim) |>
  left_join(ultimo_trim, by = ".id")

cv_fits_trim <- cv_trim |>
  model(
    phillips   = ARIMA(pi_core_core_sem_inst ~ expectativa_1ano_tankan + hiato + importacao +
                         pdq(d = 0) + PDQ(0, 0, 0), stepwise = FALSE),
    phillips_lags = ARIMA(!!formula_lags_trimestral, stepwise = FALSE),
    pure_arima = ARIMA(pi_core_core_sem_inst),
    naive      = NAIVE(pi_core_core_sem_inst)
  )

fc_trim <- cv_fits_trim |>
  forecast(new_data = cv_future_trim)

cv_results_trim <- fc_trim |>
  accuracy(dados_trimestral, measures = medidas, level = 80) |>
  select(.model, RMSE, MAE, CRPS, Winkler_80, Skill_CRPS)

cv_results_trim


# 2. Mensal (core-core sem fatores institucionais, Tankan e hiato pelo último divulgado)

h_mens <- 24   # 24 meses = 2 anos

cv_mens <- dados_mensal |>
  stretch_tsibble(.init = sum(dados_mensal$date <= yearmonth("2021 Dec")), .step = 1) |>
  filter(.id <= max(.id) - h_mens)

ultimo_mens <- cv_mens |>
  as_tibble() |>
  group_by(.id) |>
  slice_tail(n = 1) |>
  ungroup() |>
  select(.id, expectativa_1ano_tankan, hiato, importacao)

cv_future_mens <- new_data(cv_mens, h_mens) |>
  left_join(ultimo_mens, by = ".id")

cv_fits_mens <- cv_mens |>
  model(
    phillips   = ARIMA(pi_core_core_sem_inst ~ expectativa_1ano_tankan + hiato + importacao +
                         pdq(d = 0) + PDQ(0, 0, 0), stepwise = FALSE),
    phillips_lags = ARIMA(!!formula_lags_mensal, stepwise = FALSE),
    pure_arima = ARIMA(pi_core_core_sem_inst),
    naive      = NAIVE(pi_core_core_sem_inst)
  )

fc_mens <- cv_fits_mens |>
  forecast(new_data = cv_future_mens)

cv_results_mens <- fc_mens |>
  accuracy(dados_mensal, measures = medidas, level = 80) |>
  select(.model, RMSE, MAE, CRPS, Winkler_80, Skill_CRPS)

cv_results_mens


# 3. Mensal longa (core-core oficial, expectativa das famílias e dummies)

cv_longa <- dados_longa |>
  stretch_tsibble(.init = sum(dados_longa$date <= yearmonth("2021 Dec")), .step = 1) |>
  filter(.id <= max(.id) - h_mens)

ultimo_longa <- cv_longa |>
  as_tibble() |>
  group_by(.id) |>
  slice_tail(n = 1) |>
  ungroup() |>
  select(.id, expectativa_1ano_cabinet, hiato, importacao)

# As dummies são conhecidas com antecedência (datas das medidas): calcular para as datas futuras
cv_future_longa <- new_data(cv_longa, h_mens) |>
  left_join(ultimo_longa, by = ".id") |>
  mutate(
    imposto_2014     = dummy_12m(date, "2014 Apr"),
    imposto_edu_2019 = dummy_12m(date, "2019 Oct"),
    educacao_2020    = dummy_12m(date, "2020 Apr"),
    celular_2021     = dummy_12m(date, "2021 Apr"),
    merenda_2026     = dummy_12m(date, "2026 Apr")
  )

cv_fits_longa <- cv_longa |>
  model(
    phillips   = ARIMA(pi_core_core ~ expectativa_1ano_cabinet + hiato + importacao +
                         imposto_2014 + imposto_edu_2019 + educacao_2020 +
                         celular_2021 + merenda_2026 +
                         pdq(d = 0) + PDQ(0, 0, 0), stepwise = FALSE),
    phillips_lags = ARIMA(!!formula_lags_longa, stepwise = FALSE),
    pure_arima = ARIMA(pi_core_core),
    naive      = NAIVE(pi_core_core)
  )

fc_longa <- cv_fits_longa |>
  forecast(new_data = cv_future_longa)

cv_results_longa <- fc_longa |>
  accuracy(dados_longa, measures = medidas, level = 80) |>
  select(.model, RMSE, MAE, CRPS, Winkler_80, Skill_CRPS)

cv_results_longa


# 4. Comparação no core-core oficial (MIC), o alvo final dos três modelos
# Modelos 1 e 2: previsão sem fatores + fator institucional realizado.

fator_mensal <- data_modelo_s2_mensal |>
  as_tibble() |>
  select(date, fator_inst_core_core)

fator_trimestral <- data_modelo_s2_trimestral |>
  as_tibble() |>
  select(date, fator_inst_core_core)

# 4.1. Mensal: modelos 2 e 3, com ARIMA puro e NAIVE do core-core oficial como referência

prev_mens_oficial <- fc_mens |>
  as_tibble() |>
  filter(.model %in% c("phillips", "phillips_lags")) |>
  left_join(fator_mensal, by = "date") |>
  transmute(.id, .model = paste("2. mensal", .model), date,
            pi_core_core = pi_core_core_sem_inst + fator_inst_core_core)   # desloca a distribuição

# A distribuição guarda o nome da variável prevista; renomear para juntar com o modelo 3
dimnames(prev_mens_oficial$pi_core_core) <- "pi_core_core"

prev_longa_oficial <- fc_longa |>
  as_tibble() |>
  transmute(.id, .model = if_else(str_starts(.model, "phillips"), paste("3. mensal longa", .model), .model),
            date, pi_core_core)

# origem: último mês do treino de cada janela. Manter só as origens comuns aos modelos.
prev_oficial <- bind_rows(prev_mens_oficial, prev_longa_oficial) |>
  group_by(.model, .id) |>
  mutate(origem = min(date) - 1) |>
  ungroup() |>
  select(-.id) |>
  group_by(origem) |>
  filter(n_distinct(.model) == 6) |>   # 2 versões do modelo 2, 2 do modelo 3, pure_arima e naive
  ungroup()

oficial_mensal <- pi_core_core_oficial |>
  as_tsibble(index = date)

cv_results_oficial_mensal <- prev_oficial |>
  as_tsibble(key = c(origem, .model), index = date) |>
  as_fable(response = "pi_core_core", distribution = "pi_core_core") |>
  accuracy(oficial_mensal, measures = medidas, level = 80) |>
  select(.model, RMSE, MAE, CRPS, Winkler_80, Skill_CRPS) |>
  arrange(RMSE)

cv_results_oficial_mensal

# 4.2. Trimestral: os três modelos
# Previsões mensais → média dos 3 meses de cada trimestre.
# Só origens no fim de um trimestre (mar, jun, set, dez), as mesmas do modelo 1.
# Só RMSE e MAE: a distribuição da média de 3 meses previstos não sai pronta do forecast().

prev_trim_de_mensal <- prev_oficial |>
  filter(month(as.Date(origem)) %in% c(3, 6, 9, 12)) |>
  mutate(origem = yearquarter(origem), date = yearquarter(date), previsao = mean(pi_core_core)) |>
  group_by(.model, origem, date) |>
  summarise(previsao = if (n() == 3L) mean(previsao) else NA_real_, .groups = "drop")

prev_trim_oficial <- fc_trim |>
  as_tibble() |>
  filter(.model %in% c("phillips", "phillips_lags")) |>
  left_join(fator_trimestral, by = "date") |>
  group_by(.model, .id) |>
  mutate(origem = min(date) - 1) |>
  ungroup() |>
  transmute(.model = paste("1. trimestral", .model), origem, date, previsao = .mean + fator_inst_core_core)

# Observado: média trimestral do core-core oficial, como nas séries trimestrais do S2
oficial_trimestral <- pi_core_core_oficial |>
  mutate(date = yearquarter(date)) |>
  group_by(date) |>
  summarise(observado = if (n() == 3L) mean(pi_core_core) else NA_real_)

cv_results_oficial_trimestral <- bind_rows(prev_trim_oficial, prev_trim_de_mensal) |>
  group_by(origem) |>
  filter(n_distinct(.model) == 8) |>   # 2 versões de cada modelo de Phillips, pure_arima e naive
  ungroup() |>
  left_join(oficial_trimestral, by = "date") |>
  drop_na() |>
  group_by(.model) |>
  summarise(RMSE = sqrt(mean((observado - previsao)^2)),
            MAE  = mean(abs(observado - previsao))) |>
  arrange(RMSE)

cv_results_oficial_trimestral
