# 1. Mesmas pontes da curva original, em variacoes mensais desde 2016.
# Delta(5-2) = b1 * Delta(JGB2) + b2 * Delta(30-5), sem intercepto.
# Delta(10-5) = c1 * Delta(JGB2) + c2 * Delta(30-5), sem intercepto.
# Associacoes historicas; nao identificam premio fiscal nem causalidade.
amostra <- historico |>
  mutate(across(c(jgb2, spread5_2, spread10_5, spread30_5),
                ~ .x - lag(.x), .names = "d_{.col}")) |> slice(-1)
modelo5 <- lm(d_spread5_2 ~ 0 + d_jgb2 + d_spread30_5, data = amostra)
modelo10 <- lm(d_spread10_5 ~ 0 + d_jgb2 + d_spread30_5, data = amostra)
write_csv(bind_rows(broom::tidy(modelo5) |> mutate(segmento = "5Y-2Y"),
                    broom::tidy(modelo10) |> mutate(segmento = "10Y-5Y")),
          file.path(saida, "curva_jgb_regressoes.csv"))

# 2. Aplicar a mudanca BVAR ao ultimo fechamento observado.
# BVAR usa medias mensais; a ancora comum Base evita trocar a convencao de nivel.
# Spread30_5(h) = spread30_5 observado + inclinacao(h) - inclinacao Base na origem.
base_bvar <- bvar |> filter(cenario == "Base", mes == origem$mes) |> pull(inclinacao)
stopifnot(length(base_bvar) == 1L)
spreads <- jgb2 |>
  left_join(bvar |> select(cenario, mes, inclinacao),
            by = c("cenario", "mes"), relationship = "one-to-one") |>
  mutate(spread30_5 = origem$spread30_5 + inclinacao - base_bvar,
         d_jgb2 = jgb2 - origem$jgb2,
         d_spread30_5 = spread30_5 - origem$spread30_5,
         spread5_2 = origem$spread5_2 + coef(modelo5)["d_jgb2"] * d_jgb2 +
           coef(modelo5)["d_spread30_5"] * d_spread30_5,
         spread10_5 = origem$spread10_5 + coef(modelo10)["d_jgb2"] * d_jgb2 +
           coef(modelo10)["d_spread30_5"] * d_spread30_5,
         spread10_2 = spread5_2 + spread10_5,
         spread30_10 = spread30_5 - spread10_5,
         spread30_2 = spread30_5 + spread5_2,
         date = as.Date(mes + 1L) - 1L) |>
  select(cenario, mes, date, jgb2, starts_with("spread"))
write_csv(spreads, file.path(saida, "jgb2_e_spreads_cenarios.csv"))
