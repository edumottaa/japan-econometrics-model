library(fpp3)

# S2: preços de importação em ienes, com regressão e erros ARIMA.
# Referência: https://otexts.com/fpp3/dynamic-regression.html
# Executar antes a preparação dos dados, o VAR externo e o BVAR condicionado.

# 1. Histórico observado: preços de importação, iene e petróleo.
# As três séries já estão em 100 * log; não aplicar log novamente.
# Usar apenas estas colunas, sem exigir dados das demais variáveis do S2.
dados_importacao <- data_modelo_s2_mensal |>
  as_tibble() |>
  select(date, preco_importacao_iene) |>
  inner_join(data_bal |> as_tibble() |> select(date, usdjpy, brent), by = "date") |>
  drop_na() |>
  arrange(date) |>
  as_tsibble(index = date)

# A previsão deve começar no mesmo mês que os caminhos do BVAR e do VAR.
if (max(dados_importacao$date) != max(data_bal$date)) {
  stop("Os preços de importação e os modelos explicativos terminam em meses diferentes.")
}
if (any(has_gaps(dados_importacao)$.gaps)) {
  stop("Há meses faltando no histórico da regressão; conferir a preparação dos dados.")
}

# 2. Regressão com erros ARIMA; seleção automática dos erros por AICc.
# O ARIMA avalia a diferenciação e, se necessária, aplica-a à resposta e às explicativas.
modelo_importacao <- dados_importacao |>
  model(arima = ARIMA(preco_importacao_iene ~ usdjpy + brent, ic = "aicc"))

report(modelo_importacao)

# Diagnóstico visual dos resíduos do modelo estimado.
grafico_residuos_importacao <- modelo_importacao |>
  gg_tsresiduals()
print(grafico_residuos_importacao)

# 3. Caminhos futuros: iene do BVAR condicionado e Brent do VAR externo.
# fcast: sorteios x meses x variáveis; calcular a média prevista para cada mês.
iene_condicionado <- apply(previsao_bvar_condicionada$fcast, c(2, 3), mean)
colnames(iene_condicionado) <- modelo_bvar$variables

novos_dados_importacao <- new_data(dados_importacao, nrow(caminhos_externos)) |>
  mutate(usdjpy = iene_condicionado[, "usdjpy"],
         brent = caminhos_externos[, "brent"])

stopifnot(identical(novos_dados_importacao$date, previsoes_externas$date))

# 4. Previsão condicional dos preços de importação.
# As bandas não incluem a incerteza dos caminhos futuros de iene e petróleo.
previsao_importacao <- modelo_importacao |>
  forecast(new_data = novos_dados_importacao)

previsao_importacao

grafico_previsao_importacao <- previsao_importacao |>
  autoplot(dados_importacao |> filter(date >= max(date) - 59), level = c(80, 95)) +
  labs(title = "S2: previsão dos preços de importação em ienes",
       subtitle = "Condicionada ao iene do BVAR e ao Brent do VAR externo",
       x = NULL, y = "100 × log do índice de preços de importação") +
  theme_minimal()

print(grafico_previsao_importacao)
