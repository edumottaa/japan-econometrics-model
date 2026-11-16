# 1. Mesma regressao original, estimada desde 2016.
# JGB2_t = constante + beta * OIS2_(t-2) + erro_t; erro ~ ARIMA(0,0,1).
# O juro curto nao entra diretamente: ja afeta o caminho futuro do OIS.
treino2 <- historico |> select(mes, jgb2) |>
  left_join(ois_hist, by = "mes", relationship = "one-to-one") |>
  mutate(ois_lag2 = lag(ois2_pct, 2L)) |> slice(-(1:2)) |>
  as_tsibble(index = mes)
stopifnot(all(is.finite(treino2$ois_lag2)), !any(has_gaps(treino2)$.gaps))
modelo2 <- treino2 |>
  model(arima = ARIMA(jgb2 ~ 1 + ois_lag2 + pdq(0, 0, 1) + PDQ(0, 0, 0)))
write_csv(tidy(modelo2), file.path(saida, "jgb2_coeficientes.csv"))
saveRDS(modelo2, file.path(saida, "modelo_jgb2.rds"))

# 2. Reconstituir OIS2Y par: desconto ACT/365 e dois pagamentos anuais.
# Aproximacao da versao original; nao replica todas as convencoes Bloomberg.
montar_ois2 <- function(x) {
  x |> mutate(
    dias = as.numeric(lead(inicio) - inicio),
    desconto = cumprod(lag(1 / (1 + forward / 100 * dias / 365), default = 1)),
    ano1 = as.numeric(lead(inicio, 12) - inicio) / 365,
    ano2 = as.numeric(lead(inicio, 24) - lead(inicio, 12)) / 365,
    ois2 = 100 * (desconto - lead(desconto, 24)) /
      (ano1 * lead(desconto, 12) + ano2 * lead(desconto, 24)))
}

# 3. OIS2Y do mercado nos primeiros meses, migrando em 3 meses para o OIS2Y do BVAR.
# OIS2Y do BVAR: taxa de 2 anos composta pelo juro curto previsto.
# Depois do horizonte, o juro cresce pelo CAGR do ultimo ano do modelo.
transicao <- 3L
curvas_ois <- purrr::map_dfr(cenarios, function(cen) {
  juro <- bvar |> filter(cenario == cen, mes > origem$mes) |> select(mes, juro_curto)
  cagr <- (last(juro$juro_curto) / nth(juro$juro_curto, -13L))^(1 / 12)
  extra <- tibble(mes = max(juro$mes) + 1:25, juro_curto = last(juro$juro_curto) * cagr^(1:25))
  modelo <- bind_rows(juro, extra) |>
    transmute(inicio = as.Date(mes), mes, forward = juro_curto) |>
    montar_ois2() |> select(mes, ois2_modelo = ois2)
  ois |> montar_ois2() |> select(mes, ois2_mercado = ois2) |>
    inner_join(modelo, by = "mes", relationship = "one-to-one") |>
    mutate(peso = pmin(as.numeric(mes - origem$mes) / transicao, 1),
           ois2 = (1 - peso) * ois2_mercado + peso * ois2_modelo, cenario = cen)
})
write_csv(curvas_ois, file.path(saida, "ois2_cenarios.csv"))

# 4. Forecast condicional: usar as duas ultimas observacoes para criar o lag.
jgb2 <- purrr::map_dfr(cenarios, function(cen) {
  novos <- curvas_ois |> filter(cenario == cen, mes %in% meses) |>
    transmute(mes, ois2_pct = ois2) |>
    bind_rows(ois_hist |> slice_tail(n = 2)) |> arrange(mes) |>
    mutate(ois_lag2 = lag(ois2_pct, 2L)) |> filter(mes %in% meses) |>
    as_tsibble(index = mes)
  stopifnot(nrow(novos) == length(meses), all(is.finite(novos$ois_lag2)))
  forecast(modelo2, new_data = novos) |> as_tibble() |>
    transmute(cenario = cen, mes, jgb2 = .mean)
})
write_csv(jgb2, file.path(saida, "jgb2_cenarios.csv"))
