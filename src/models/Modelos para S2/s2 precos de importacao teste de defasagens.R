library(fpp3)

# S2: escolha das defasagens do iene e do Brent no modelo de preços de importação.
# Mesmo método do teste do S2 e da seção 6 do código dos EUA (FPP3, seção 10.6).
# Executar antes a seção 1 do script "src/models/s2 precos de importacao.R" (cria dados_importacao).

# d = 1 fixo: é a diferenciação que o ARIMA automático escolhe para este modelo.
#   O AICc só compara modelos com a mesma diferenciação (FPP3, seção 9.6).
# AMOSTRA COMUM: as primeiras K = 6 observações da resposta viram NA.
# FAMÍLIA A: termo contemporâneo + defasagens acumuladas.
# FAMÍLIA B: só defasagens, sem termo contemporâneo (menos hipóteses sobre o futuro).
# Cada regressor é testado sozinho, como no código dos EUA.

specs_importacao <- list(
  A_lag0 = 0, A_lag1 = 0:1, A_lag2 = 0:2, A_lag3 = 0:3,
  B_1 = 1, B_13 = 1:3, B_3 = 3, B_36 = 3:6, B_6 = 6
)

# termos(): escreve as defasagens como texto da fórmula. Ex.: termos("brent", 0:1) = "brent + lag(brent, 1)"
termos <- function(x, lags) {
  paste(if_else(lags == 0, x, paste0("lag(", x, ", ", lags, ")")), collapse = " + ")
}

# testa_lags_imp(): uma regressão com erros ARIMA(p, 1, q) para cada especificação; compara AICc e BIC
testa_lags_imp <- function(dados, x, specs, K) {
  dados_comuns <- dados |>
    mutate(y = if_else(row_number() <= K, NA_real_, preco_importacao_iene))

  modelos <- map(specs, function(lags) {
    formula <- as.formula(paste("y ~", termos(x, lags), "+ pdq(d = 1) + PDQ(0, 0, 0)"))
    ARIMA(!!formula, stepwise = FALSE)
  })

  dados_comuns |>
    model(!!!modelos) |>
    glance() |>
    transmute(
      regressor = x,
      spec      = .model,
      n_termos  = map_int(spec, ~ length(specs[[.x]])),
      menor_lag = map_dbl(spec, ~ min(specs[[.x]])),
      AICc, BIC
    ) |>
    arrange(AICc) |>
    mutate(delta_AICc = AICc - min(AICc),
           delta_BIC  = BIC  - min(BIC))
}

# Mesma regra do código dos EUA: (i) menor AICc; (ii) empate com delta_AICc < 2: menos termos;
# (iii) persistindo o empate: maior menor defasagem
escolhe <- function(tabela) {
  tabela |>
    filter(delta_AICc < 2) |>
    arrange(n_termos, desc(menor_lag), AICc) |>
    slice(1)
}

testes_importacao <- map(c("usdjpy", "brent"), ~ testa_lags_imp(
  dados_importacao, .x, specs_importacao, K = 6
))
testes_importacao

escolhas_importacao <- map_dfr(testes_importacao, escolhe)
escolhas_importacao

# Fórmula final com as defasagens escolhidas (usada no CV)
formula_lags_importacao <- as.formula(paste(
  "preco_importacao_iene ~",
  paste(map2_chr(escolhas_importacao$regressor, escolhas_importacao$spec,
                 ~ termos(.x, specs_importacao[[.y]])), collapse = " + "),
  "+ pdq(d = 1) + PDQ(0, 0, 0)"
))
formula_lags_importacao
