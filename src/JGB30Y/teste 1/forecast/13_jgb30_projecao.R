# JGB30Y | Soma do JGB2Y e do spread 30–2 projetados na mesma data.
library(fpp3)
library(readr)

saida <- "src/JGB30Y/projecoes/resultados"
historico <- read_csv(file.path(saida, "base_mensal.csv"), show_col_types = FALSE) |>
  select(date, jgb30) |> arrange(date)
base <- historico |> slice_tail(n = 1)
jgb2 <- read_csv(file.path(saida, "jgb2_reprecificacao_bvar_50.csv"), show_col_types = FALSE) |>
  select(mes, date, horizonte, jgb2_mercado_pct, jgb2_revisado_pct)
spread <- read_csv(file.path(saida, "spread_30_2_arima_projecao.csv"), show_col_types = FALSE) |>
  select(mes, date, horizonte, spread_bp, extensao_bvar)

projecao <- inner_join(jgb2, spread, by = c("mes", "date", "horizonte"),
                       relationship = "one-to-one") |>
  arrange(horizonte) |>
  mutate(jgb30_mercado_pct = jgb2_mercado_pct + spread_bp / 100,
         jgb30_revisado_pct = jgb2_revisado_pct + spread_bp / 100,
         delta_mercado_bp = 100 * (jgb30_mercado_pct - base$jgb30),
         delta_revisado_bp = 100 * (jgb30_revisado_pct - base$jgb30))
stopifnot(nrow(projecao) == nrow(jgb2), nrow(projecao) == nrow(spread),
          all(projecao$horizonte == 1:24),
          all(projecao$mes == as.Date(yearmonth(base$date) + projecao$horizonte)),
          all(is.finite(projecao$jgb30_mercado_pct)), all(is.finite(projecao$jgb30_revisado_pct)))
write_csv(projecao, file.path(saida, "jgb30_projecao_spread_arima.csv"))

inicio <- ymd("2022-01-01")
series <- c("Observado", "Curva Bloomberg", "Revisão de 50% do gap")
linhas <- projecao |>
  select(date, `Curva Bloomberg` = jgb30_mercado_pct, `Revisão de 50% do gap` = jgb30_revisado_pct) |>
  bind_rows(tibble(date = base$date, `Curva Bloomberg` = base$jgb30,
                   `Revisão de 50% do gap` = base$jgb30)) |>
  pivot_longer(-date, names_to = "serie", values_to = "jgb30") |>
  bind_rows(historico |> filter(date >= inicio) |> mutate(serie = "Observado"))
taxa <- scales::label_number(accuracy = 0.01, decimal.mark = ",", suffix = "%")
marcos <- projecao |> filter(horizonte %in% c(12L, 24L)) |>
  transmute(date, jgb30 = jgb30_mercado_pct, rotulo = taxa(jgb30))

grafico <- ggplot(linhas, aes(date, jgb30)) +
  geom_vline(xintercept = base$date, colour = "grey65", linetype = "dotted") +
  geom_line(data = filter(linhas, serie != "Revisão de 50% do gap"),
            aes(colour = serie, linetype = serie), linewidth = 1.05) +
  geom_line(data = filter(linhas, serie == "Revisão de 50% do gap"),
            aes(colour = serie, linetype = serie), linewidth = 0.85) +
  geom_text(data = marcos, aes(label = rotulo), vjust = -0.9,
            colour = "#315B7A", fontface = "bold") +
  scale_colour_manual(values = c("#242A30", "#315B7A", "#A64D34"), limits = series) +
  scale_linetype_manual(values = c("solid", "solid", "dashed"), limits = series) +
  scale_x_date(breaks = seq(inicio, max(projecao$date), by = "year"), date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ","),
                     expand = expansion(mult = c(0.04, 0.12))) +
  labs(title = "JGB30Y observado e projetado",
       subtitle = "JGB2Y projetado somado ao spread 30–2 da regressão com erros ARIMA",
       x = NULL, y = "% ao ano", colour = NULL, linetype = NULL,
       caption = "Rótulos da curva Bloomberg. Em set–out/2028, o 10–2 do BVAR fica no nível de agosto.") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))
ggsave(file.path(saida, "jgb30_projecao_spread_arima.png"), grafico,
       width = 10, height = 4.8, dpi = 190, bg = "white")

print(projecao |> filter(horizonte %in% c(12L, 24L)), width = Inf)
