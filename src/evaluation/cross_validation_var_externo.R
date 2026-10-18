# Validação temporal do VAR externo (IP mundial, Brent, VIX, Treasury 2 anos)
# contra ARIMA puro e NAIVE de cada variável.
# Mesmo método do CV do núcleo: stretch_tsibble, forecast() e accuracy().
# O VAR não tem regressores externos: a previsão já é ex-ante.
# Executar antes os scripts de dados (1 a 3), que criam data_bal.

library(fpp3)
library(distributional)

medidas <- list(
  RMSE       = RMSE,
  MAE        = MAE,
  CRPS       = CRPS,
  Winkler_80 = winkler_score,
  Skill_CRPS = skill_score(CRPS)
)

h_ext <- 12
variaveis <- c("ip_mundial", "brent", "vix", "ust2y")

dados_ext <- data_bal |>
  select(all_of(variaveis))

# Formato longo: uma linha por mês e variável, para avaliar as quatro de uma vez
dados_ext_longo <- dados_ext |>
  pivot_longer(-date, names_to = "variavel", values_to = "valor")


# 1. Benchmarks univariados: ARIMA puro e NAIVE de cada variável
# Mesmas janelas do CV do núcleo: primeira com 180 meses, avançando 3 meses
acc_uni_ext <- dados_ext_longo |>
  stretch_tsibble(.init = 180, .step = 3) |>
  model(
    pure_arima = ARIMA(valor),
    naive      = NAIVE(valor)
  ) |>
  forecast(h = h_ext) |>
  filter(date <= max(dados_ext$date)) |>
  accuracy(dados_ext_longo, measures = medidas, level = 80)


# 2. VAR externo, com as defasagens escolhidas por AICc em cada janela
previsoes_var_ext <- dados_ext |>
  stretch_tsibble(.init = 180, .step = 3) |>
  model(var = VAR(vars(ip_mundial, brent, vix, ust2y))) |>
  forecast(h = h_ext) |>
  filter(date <= max(dados_ext$date))

# A previsão do VAR é uma normal conjunta das 4 variáveis.
# Para avaliar cada variável, extrair a sua distribuição marginal: média k e variância [k, k].
param_var_ext <- parameters(previsoes_var_ext$.distribution)

previsoes_var_ext_longo <- map_dfr(seq_along(variaveis), function(k) {
  tibble(
    .id      = previsoes_var_ext$.id,
    .model   = "var",
    date     = previsoes_var_ext$date,
    variavel = variaveis[k],
    valor    = dist_normal(
      mu    = map_dbl(param_var_ext$mu, k),
      sigma = sqrt(map_dbl(param_var_ext$sigma, function(s) s[k, k]))
    )
  )
}) |>
  as_tsibble(key = c(.id, .model, variavel), index = date) |>
  as_fable(response = "valor", distribution = "valor")

acc_var_ext <- previsoes_var_ext_longo |>
  accuracy(dados_ext_longo, measures = medidas, level = 80)


# 3. Tabela: comparar os modelos dentro de cada variável
cv_results_var_ext <- bind_rows(acc_uni_ext, acc_var_ext) |>
  select(variavel, .model, RMSE, MAE, CRPS, Winkler_80, Skill_CRPS) |>
  arrange(variavel, RMSE)

cv_results_var_ext
