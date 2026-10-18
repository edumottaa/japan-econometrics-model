library(fpp3)

# S2: escolha das defasagens de cada regressor da curva de Phillips.
# Mesmo método da seção 6 do código dos EUA (FPP3, seção 10.6, lagged predictors).
# Executar antes o script "src/models/s2 curva de phillips.R" (usa dados_trimestral,
# dados_mensal e dados_longa).

# AMOSTRA COMUM: o AICc só compara modelos estimados na mesma amostra.
#   As primeiras K observações da resposta viram NA, para todas as especificações
#   usarem os mesmos meses, mesmo as que têm defasagens maiores.
# FAMÍLIA A: termo contemporâneo + defasagens acumuladas.
#   Exige hipótese para o valor futuro do regressor desde o primeiro período previsto.
# FAMÍLIA B: só defasagens de 1 período ou mais, sem termo contemporâneo.
#   Os primeiros períodos da previsão usam valores observados, e não hipóteses.
# Cada regressor é testado sozinho, como no código dos EUA (as dummies ficam em todos).


# 1. Especificações testadas: nome = defasagens incluídas (0 = contemporâneo)

specs_trimestral <- list(
  A_lag0 = 0, A_lag1 = 0:1, A_lag2 = 0:2, A_lag3 = 0:3, A_lag4 = 0:4,
  B_1 = 1, B_12 = 1:2, B_2 = 2, B_23 = 2:3, B_3 = 3, B_34 = 3:4, B_4 = 4
)

# Mensal: defasagens em meses, de trimestre em trimestre
specs_mensal <- list(
  A_lag0 = 0, A_lag03 = c(0, 3), A_lag036 = c(0, 3, 6),
  B_1 = 1, B_3 = 3, B_36 = c(3, 6), B_6 = 6, B_69 = c(6, 9), B_9 = 9, B_12 = 12
)


# 2. Função de teste

# termos(): escreve as defasagens como texto da fórmula. Ex.: termos("hiato", 0:1) = "hiato + lag(hiato, 1)"
termos <- function(x, lags) {
  paste(if_else(lags == 0, x, paste0("lag(", x, ", ", lags, ")")), collapse = " + ")
}

# testa_lags(): estima uma regressão com erros ARIMA para cada especificação e compara AICc e BIC.
#   y: variável dependente; x: regressor testado; K: observações omitidas no início;
#   fixos: termos presentes em todos os modelos (dummies do modelo 3).
testa_lags <- function(dados, y, x, specs, K, fixos = NULL) {
  dados_comuns <- dados |>
    mutate(y = if_else(row_number() <= K, NA_real_, .data[[y]]))

  modelos <- map(specs, function(lags) {
    formula <- as.formula(paste("y ~", paste(c(termos(x, lags), fixos), collapse = " + "),
                                "+ pdq(d = 0) + PDQ(0, 0, 0)"))
    # stepwise = FALSE: busca completa dos erros ARIMA. A busca rápida (stepwise) para quando
    # o AR(1) inicial fica com raiz perto da unitária e devolve erros ARIMA(0,0,0) por engano.
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

# escolhe(): mesma regra do código dos EUA, em ordem:
#   (i)   menor AICc;
#   (ii)  empates com delta_AICc < 2: menos termos (parcimônia);
#   (iii) persistindo o empate: maior menor defasagem (menos hipóteses sobre o futuro).
escolhe <- function(tabela) {
  tabela |>
    filter(delta_AICc < 2) |>
    arrange(n_termos, desc(menor_lag), AICc) |>
    slice(1)
}


# 3. Modelo 1: trimestral (K = 4 trimestres)

regressores_tankan <- c("expectativa_1ano_tankan", "hiato", "importacao")

testes_trimestral <- map(regressores_tankan, ~ testa_lags(
  dados_trimestral, "pi_core_core_sem_inst", .x, specs_trimestral, K = 4
))
testes_trimestral

escolhas_trimestral <- map_dfr(testes_trimestral, escolhe)
escolhas_trimestral


# 4. Modelo 2: mensal (K = 12 meses)

testes_mensal <- map(regressores_tankan, ~ testa_lags(
  dados_mensal, "pi_core_core_sem_inst", .x, specs_mensal, K = 12
))
testes_mensal

escolhas_mensal <- map_dfr(testes_mensal, escolhe)
escolhas_mensal


# 5. Modelo 3: mensal longa (K = 12 meses; dummies em todas as especificações)

dummies <- c("imposto_2014", "imposto_edu_2019", "educacao_2020", "celular_2021", "merenda_2026")

testes_longa <- map(c("expectativa_1ano_cabinet", "hiato", "importacao"), ~ testa_lags(
  dados_longa, "pi_core_core", .x, specs_mensal, K = 12, fixos = dummies
))
testes_longa

escolhas_longa <- map_dfr(testes_longa, escolhe)
escolhas_longa


# 6. Fórmulas finais, com a defasagem escolhida para cada regressor (usadas no CV)

formula_final <- function(y, escolhas, specs, fixos = NULL) {
  regressores <- map2_chr(escolhas$regressor, escolhas$spec, ~ termos(.x, specs[[.y]]))
  as.formula(paste(y, "~", paste(c(regressores, fixos), collapse = " + "),
                   "+ pdq(d = 0) + PDQ(0, 0, 0)"))
}

formula_lags_trimestral <- formula_final("pi_core_core_sem_inst", escolhas_trimestral, specs_trimestral)
formula_lags_mensal     <- formula_final("pi_core_core_sem_inst", escolhas_mensal, specs_mensal)
formula_lags_longa      <- formula_final("pi_core_core", escolhas_longa, specs_mensal, fixos = dummies)

formula_lags_trimestral
formula_lags_mensal
formula_lags_longa
