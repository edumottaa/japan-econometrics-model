# Somente depois dos spreads e do JGB2Y, reconstruir os outros rendimentos.
# JGB5 = JGB2 + (5-2); JGB10 = JGB2 + (10-2); JGB30 = JGB5 + (30-5).
curva <- spreads |>
  mutate(jgb5 = jgb2 + spread5_2, jgb10 = jgb2 + spread10_2,
         jgb30 = jgb5 + spread30_5) |>
  select(cenario, mes, date, all_of(variaveis), starts_with("spread"))
stopifnot(nrow(curva) == length(cenarios) * length(meses),
          all(is.finite(as.matrix(select(curva, all_of(variaveis), starts_with("spread"))))),
          max(abs(curva$jgb30 - curva$jgb10 - curva$spread30_10)) < 1e-10,
          max(abs(curva$jgb30 - curva$jgb2 - curva$spread30_2)) < 1e-10)
write_csv(curva, file.path(saida, "curva_jgb_2_5_10_30_cenarios.csv"))
print(curva |> group_by(cenario) |> slice_tail(n = 1) |> ungroup(), width = Inf)
