# Validação temporal do modelo de preços de importação: regressão com erros ARIMA
# contra ARIMA puro e NAIVE.
# Mesmo método do código dos EUA (seção 8.3) e do CV do S2.
# Previsão ex-ante: iene e Brent futuros = último valor de cada janela de treino (NAIVE).
# Executar antes a seção 1 do script "src/models/s2 precos de importacao.R" (cria dados_importacao)
# e o script "src/models/s2 precos de importacao teste de defasagens.R" (fórmula com as defasagens).
# regressao: iene e Brent no mesmo mês; regressao_lags: defasagens escolhidas pelo AICc.

library(fpp3)

# Métricas da seção 5.9 do FPP3
medidas <- list(
  RMSE       = RMSE,
  MAE        = MAE,
  CRPS       = CRPS,
  Winkler_80 = winkler_score,
  Skill_CRPS = skill_score(CRPS)
)

h_imp <- 12   # 12 meses, o mesmo horizonte do CV do núcleo

# Mesmas janelas do CV do núcleo: primeira com 180 meses (2000 a 2014), avançando 3 meses
cv_imp <- dados_importacao |>
  stretch_tsibble(.init = 180, .step = 3)

# Regressores futuros: último valor de cada janela (.id), repetido nos h meses
ultimo_imp <- cv_imp |>
  as_tibble() |>
  group_by(.id) |>
  slice_tail(n = 1) |>
  ungroup() |>
  select(.id, usdjpy, brent)

cv_future_imp <- new_data(cv_imp, h_imp) |>
  left_join(ultimo_imp, by = ".id")

cv_fits_imp <- cv_imp |>
  model(
    regressao  = ARIMA(preco_importacao_iene ~ usdjpy + brent, ic = "aicc"),
    regressao_lags = ARIMA(!!formula_lags_importacao, stepwise = FALSE),
    pure_arima = ARIMA(preco_importacao_iene),
    naive      = NAIVE(preco_importacao_iene)
  )

# Descartar as previsões além do último mês observado
cv_results_imp <- cv_fits_imp |>
  forecast(new_data = cv_future_imp) |>
  filter(date <= max(dados_importacao$date)) |>
  accuracy(dados_importacao, measures = medidas, level = 80) |>
  select(.model, RMSE, MAE, CRPS, Winkler_80, Skill_CRPS) |>
  arrange(RMSE)

cv_results_imp
