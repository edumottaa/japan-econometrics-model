library(fpp3)

# S2: curva de Phillips do CPI core-core, com regressão e erros ARIMA.
# Referência: https://otexts.com/fpp3/dynamic-regression.html
# Executar antes a coleta, a limpeza e a transformação dos dados (scripts 1 a 3).

# Equação: inflação = expectativa + hiato + preços de importação + erro ARIMA.
# A inflação passada não entra como regressor: a parte AR do erro faz esse papel (inércia).
# d = 0: a variável já é uma taxa de inflação, então não se diferencia.
# PDQ(0, 0, 0): as variações em 12 meses não têm sazonalidade.


# 1. Trimestral: core-core sem fatores institucionais (BoJ), expectativa do Tankan

# Preços de importação: variação em 4 trimestres (já em 100 * log, então a diferença é em %)
dados_trimestral <- data_modelo_s2_trimestral |>
  mutate(importacao = difference(preco_importacao_iene, 4)) |>
  select(date, pi_core_core_sem_inst, expectativa_1ano_tankan, hiato, importacao) |>
  drop_na()

range(dados_trimestral$date)   # amostra usada

modelo_trimestral <- dados_trimestral |>
  model(arima = ARIMA(pi_core_core_sem_inst ~ expectativa_1ano_tankan + hiato + importacao +
                        pdq(d = 0) + PDQ(0, 0, 0)))

report(modelo_trimestral)
modelo_trimestral |> gg_tsresiduals()


# 2. Mensal: mesma equação; Tankan e hiato pelo último resultado divulgado

# Preços de importação: variação em 12 meses
dados_mensal <- data_modelo_s2_mensal |>
  mutate(importacao = difference(preco_importacao_iene, 12)) |>
  select(date, pi_core_core_sem_inst, expectativa_1ano_tankan, hiato, importacao) |>
  drop_na()

range(dados_mensal$date)

modelo_mensal <- dados_mensal |>
  model(arima = ARIMA(pi_core_core_sem_inst ~ expectativa_1ano_tankan + hiato + importacao +
                        pdq(d = 0) + PDQ(0, 0, 0)))

report(modelo_mensal)
modelo_mensal |> gg_tsresiduals()


# 3. Mensal longa: core-core oficial (MIC), expectativa das famílias e dummies

# Core-core oficial em 12 meses (c0178_3); em cada mês, usar a base mais recente disponível
pi_core_core_oficial <- cpi_s2_mic |>
  select(date, base, c0178_3) |>
  pivot_wider(names_from = base, values_from = c0178_3) |>
  transmute(date = yearmonth(date), pi_core_core = coalesce(`2025`, `2020`, `2015`))

# Dummy de 12 meses: um salto no nível do preço muda a variação em 12 meses por 12 meses
dummy_12m <- function(date, inicio) {
  as.numeric(date >= yearmonth(inicio) & date < yearmonth(inicio) + 12)
}

# Medidas do governo (seção 6.2 do plano). CONFERIR cada data com fonte primária.
# As medidas de energia não entram: o core-core já exclui energia.
dados_longa <- data_modelo_s2_mensal |>
  as_tibble() |>
  left_join(pi_core_core_oficial, by = "date") |>
  as_tsibble(index = date) |>
  mutate(
    importacao       = difference(preco_importacao_iene, 12),
    imposto_2014     = dummy_12m(date, "2014 Apr"),  # imposto sobre consumo: 5% → 8%
    imposto_edu_2019 = dummy_12m(date, "2019 Oct"),  # imposto 8% → 10% e pré-escola gratuita, no mesmo mês
    educacao_2020    = dummy_12m(date, "2020 Apr"),  # ensino superior gratuito
    celular_2021     = dummy_12m(date, "2021 Apr"),  # corte das tarifas de celular
    merenda_2026     = dummy_12m(date, "2026 Apr")   # merenda escolar gratuita
  ) |>
  select(date, pi_core_core, expectativa_1ano_cabinet, hiato, importacao,
         imposto_2014, imposto_edu_2019, educacao_2020, celular_2021, merenda_2026) |>
  drop_na()

range(dados_longa$date)

modelo_longa <- dados_longa |>
  model(arima = ARIMA(pi_core_core ~ expectativa_1ano_cabinet + hiato + importacao +
                        imposto_2014 + imposto_edu_2019 + educacao_2020 +
                        celular_2021 + merenda_2026 +
                        pdq(d = 0) + PDQ(0, 0, 0)))

report(modelo_longa)
modelo_longa |> gg_tsresiduals()


#