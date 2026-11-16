# JGB2Y | Comparar curva original com repasse de 50% do gap BVAR - mercado.
source("src/JGB30Y/teste 1/forecast/09_jgb2_curva_mercado.R", encoding = "UTF-8")

repasse <- 0.5
curto_bvar <- insumos |> filter(!extensao_bvar) |> select(mes, juro_curto)
curva_revisada <- ois |>
  left_join(curto_bvar, by = "mes", relationship = "one-to-one") |>
  mutate(gap_curto_bp = 100 * (juro_curto - curto_mercado_pct),
         curto_mercado_pct = curto_mercado_pct + repasse * coalesce(gap_curto_bp, 0) / 100) |>
  montar_ois2() |>
  transmute(mes, gap_curto_bp, curto_revisado_pct = curto_mercado_pct,
            ois2_revisado_pct = ois2_termo_pct)

modelo_ois <- ajustar_jgb2(historico, inclui_curto = FALSE)
comparacao <- insumos |>
  transmute(mes, date, horizonte, curto_mercado_pct, ois2_mercado_pct = ois2_termo_pct) |>
  left_join(curva_revisada, by = "mes", relationship = "one-to-one") |>
  mutate(jgb2_mercado_pct = projetar(insumos$juro_curto, ois2_mercado_pct, modelo_ois),
         jgb2_revisado_pct = projetar(insumos$juro_curto, ois2_revisado_pct, modelo_ois),
         efeito_jgb2_bp = 100 * (jgb2_revisado_pct - jgb2_mercado_pct),
         gap_ois2_bp = 100 * (ois2_revisado_pct - ois2_mercado_pct),
         mes = as.Date(mes))
stopifnot(all(is.finite(comparacao$efeito_jgb2_bp)))
write_csv(comparacao, file.path(saida, "jgb2_reprecificacao_bvar_50.csv"))

# Historico desde 2022 e dois forecasts a partir do ultimo dado observado.
inicio_grafico <- ymd("2022-01-01")
series <- c("Observado", "Curva Bloomberg", "Revisão")
rotulos <- c("Observado", "Curva Bloomberg original",
             paste0("Curva com ", 100 * repasse, "% do gap BVAR–mercado"))
linhas <- comparacao |>
  select(date, `Curva Bloomberg` = jgb2_mercado_pct, Revisão = jgb2_revisado_pct) |>
  bind_rows(tibble(date = base$date, `Curva Bloomberg` = base$jgb2, Revisão = base$jgb2)) |>
  pivot_longer(-date, names_to = "serie", values_to = "valor") |>
  bind_rows(historico |> filter(date >= inicio_grafico) |>
              transmute(date, serie = "Observado", valor = jgb2))

grafico <- ggplot(linhas, aes(date, valor, colour = serie, linetype = serie)) +
  geom_vline(xintercept = base$date, colour = "grey65", linetype = "dotted") +
  geom_line(data = filter(linhas, serie != "Revisão"), linewidth = 1.15) +
  geom_line(data = filter(linhas, serie == "Revisão"), linewidth = 0.95) +
  scale_colour_manual(values = c("#242A30", "#315B7A", "#A64D34"), limits = series,
                      labels = rotulos) +
  scale_linetype_manual(values = c("solid", "solid", "dashed"), limits = series,
                        labels = rotulos) +
  scale_x_date(breaks = seq(inicio_grafico, max(comparacao$date), by = "year"),
               date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
  labs(title = "JGB2Y observado e projetado",
       subtitle = "Histórico desde janeiro de 2022 e duas projeções até outubro de 2028",
       x = NULL, y = "% ao ano", colour = NULL, linetype = NULL,
       caption = paste("Forwards Bloomberg de", format(first(ois$inicio_ois), "%d/%m/%Y"),
                       "· Revisão só nos meses cobertos pelo BVAR, até",
                       format(as.Date(max(curto_bvar$mes)), "%m/%Y"))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))
ggsave(file.path(saida, "jgb2_reprecificacao_bvar_50.png"), grafico,
       width = 10, height = 4.8, dpi = 190, bg = "white")

print(comparacao |> filter(horizonte %in% c(1L, 12L, 24L)), width = Inf)
print(range(comparacao$efeito_jgb2_bp))
