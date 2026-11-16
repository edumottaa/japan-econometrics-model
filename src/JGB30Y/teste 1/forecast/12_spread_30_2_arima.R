# Spread 30–2 | Regressao sobre o 10–2 do BVAR com erros ARIMA.
source("src/JGB30Y/projecoes/00_dados.R", encoding = "UTF-8")

# AICc escolhe p e q dentro de cada d. O CV escolhe d pelo erro em 12 meses.
ajustar <- function(x) x |>
  model(
    arima_d0 = ARIMA(spread30_2 ~ inclinacao + pdq(0:2, 0, 0:2) + PDQ(0, 0, 0),
                     stepwise = FALSE, approximation = FALSE, order_constraint = p + q <= 2),
    arima_d1 = ARIMA(spread30_2 ~ inclinacao + pdq(0:2, 1, 0:2) + PDQ(0, 0, 0),
                     stepwise = FALSE, approximation = FALSE, order_constraint = p + q <= 2),
    mqo = TSLM(spread30_2 ~ inclinacao),
    constante = NAIVE(spread30_2)
  )

origens <- origens_cv("2021 Dec", passo = 1L)
cv <- pmap_dfr(origens, function(origem, alvo, bloco) {
  treino <- mensal |> filter(mes <= origem)
  ajuste <- ajustar(treino)
  janela <- mensal |> filter(mes > origem, mes <= alvo)
  incl_origem <- last(treino$inclinacao)
  avaliar <- function(x, insumo) {
    forecast(ajuste, new_data = x |> select(mes, inclinacao)) |>
      as_tibble() |> filter(mes == alvo) |>
      transmute(origem, alvo, bloco, insumo, modelo = .model,
                observado = last(janela$spread30_2), previsto = .mean)
  }
  bind_rows(avaliar(janela, "10–2 realizado"),
            avaliar(janela |> mutate(inclinacao = incl_origem), "10–2 fixo na origem"))
})
validacao <- cv |> group_by(bloco, insumo, modelo) |>
  summarise(n = n(), n_validos = sum(is.finite(previsto)),
            rmse_bp = 100 * sqrt(mean((previsto - observado)^2)),
            mae_bp = 100 * mean(abs(previsto - observado)), .groups = "drop")
escolha <- validacao |> filter(bloco == "selecao", insumo == "10–2 realizado",
                              modelo %in% c("arima_d0", "arima_d1"), n_validos == n) |>
  slice_min(rmse_bp, n = 1, with_ties = FALSE) |> pull(modelo)
stopifnot(length(escolha) == 1L)
write_csv(validacao, file.path(saida, "spread_30_2_arima_cv_12m.csv"))

ajuste <- ajustar(mensal) |> select(all_of(escolha))
ordem <- fabletools::model_sum(ajuste[[escolha]][[1]])
pdq_final <- as.integer(stringr::str_match(ordem, "ARIMA\\(([0-9]+),([0-9]+),([0-9]+)\\)")[1, 2:4])
stopifnot(all(is.finite(pdq_final)))
inovacoes <- na.omit(augment(ajuste)$.innov)
lb <- Box.test(inovacoes, lag = 12L, type = "Ljung-Box", fitdf = pdq_final[1] + pdq_final[3])
coeficientes <- tidy(ajuste) |> mutate(ordem, ljung_box_12_p = lb$p.value)
write_csv(coeficientes, file.path(saida, "spread_30_2_arima_coeficientes.csv"))

novo <- futuro |> select(mes, inclinacao) |> as_tsibble(index = mes)
previsao <- forecast(ajuste, new_data = novo) |>
  hilo(level = 80) |> unpack_hilo(`80%`) |> as_tibble() |>
  transmute(mes, spread_bp = 100 * .mean,
            inferior80_bp = 100 * `80%_lower`, superior80_bp = 100 * `80%_upper`)
projecao <- futuro |> select(mes, date, horizonte, inclinacao, extensao_bvar) |>
  left_join(previsao, by = "mes", relationship = "one-to-one") |>
  mutate(mes = as.Date(mes), delta_bp = spread_bp - 100 * base$spread30_2)
stopifnot(all(is.finite(projecao$spread_bp)))
write_csv(projecao, file.path(saida, "spread_30_2_arima_projecao.csv"))

inicio_grafico <- ymd("2022-01-01")
historico <- mensal |> filter(date >= inicio_grafico) |> mutate(spread_bp = 100 * spread30_2)
linha <- projecao |> select(date, spread_bp, extensao_bvar) |>
  bind_rows(tibble(date = base$date, spread_bp = 100 * base$spread30_2, extensao_bvar = FALSE)) |>
  arrange(date)
extensao <- linha |> filter(extensao_bvar | lead(extensao_bvar, default = FALSE))
marcos <- projecao |> filter(horizonte %in% c(12L, 24L)) |>
  mutate(rotulo = scales::label_number(accuracy = 0.1, decimal.mark = ",", suffix = " pb")(spread_bp))

grafico <- ggplot(historico, aes(date, spread_bp)) +
  geom_vline(xintercept = base$date, colour = "grey60", linetype = "dotted") +
  geom_line(colour = "#242A30", linewidth = 0.85) +
  geom_line(data = filter(linha, !extensao_bvar), colour = "#315B7A", linewidth = 1.1) +
  geom_line(data = extensao, colour = "#315B7A", linewidth = 1.1, linetype = "dashed") +
  geom_point(data = marcos, colour = "#315B7A", size = 2.4) +
  geom_text(data = marcos, aes(label = rotulo), vjust = -0.9, colour = "#315B7A", fontface = "bold") +
  scale_x_date(breaks = seq(inicio_grafico, max(projecao$date), by = "year"), date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ","),
                     expand = expansion(mult = c(0.04, 0.14))) +
  labs(title = "JGB30Y menos JGB2Y: histórico e projeção",
       subtitle = paste0("Projeção condicionada ao JGB10Y menos JGB2Y do BVAR Base. Erros ARIMA(",
                         paste(pdq_final, collapse = ","), ")."),
       x = NULL, y = "Pontos-base",
       caption = "Histórico mensal desde 2022. Em set–out/2028, a inclinação 10–2 fica no nível de ago/2028.") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))
ggsave(file.path(saida, "spread_30_2_arima_projecao.png"), grafico,
       width = 10, height = 5, dpi = 190, bg = "white")

print(validacao, n = Inf)
print(coeficientes, width = Inf)
print(projecao |> filter(horizonte %in% c(1L, 12L, 24L)), width = Inf)
