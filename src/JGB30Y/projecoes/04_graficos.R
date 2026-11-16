# Historico desde 2019 e quatro cenarios, com rotulos finais.
cores <- c(Base = "#2166AC", Benigno = "#1B9E77", Adverso = "#D58500",
           "Adverso estrutural" = "#B2182B")
desenhar <- function(campos, titulos, nome, titulo, multiplicador = 1) {
  anterior <- historico |> filter(date >= ymd("2019-01-01")) |>
    select(date, all_of(campos)) |>
    pivot_longer(-date, names_to = "variavel", values_to = "valor")
  futuro <- curva |> select(cenario, date, all_of(campos)) |>
    bind_rows(tidyr::crossing(cenario = cenarios,
                             origem |> select(date, all_of(campos)))) |>
    pivot_longer(-c(cenario, date), names_to = "variavel", values_to = "valor")
  rotulos <- futuro |> group_by(cenario, variavel) |> slice_max(date, n = 1) |> ungroup() |>
    mutate(rotulo = formatC(valor * multiplicador, format = "f",
                            digits = if (multiplicador == 100) 1 else 2, decimal.mark = ",")) |>
    group_by(variavel) |> group_modify(function(x, chave) {
      x <- arrange(x, valor)
      x$rotulo_y <- x$valor * multiplicador
      for (i in seq_len(nrow(x))[-1])
        x$rotulo_y[i] <- max(x$rotulo_y[i], x$rotulo_y[i - 1] +
                              if (multiplicador == 100) 4 else .08)
      x
    }) |> ungroup()
  g <- ggplot() +
    geom_vline(xintercept = origem$date, colour = "grey65", linetype = "dotted") +
    geom_line(data = anterior, aes(date, valor * multiplicador), colour = "#292D32", linewidth = .65) +
    geom_line(data = futuro, aes(date, valor * multiplicador, colour = cenario), linewidth = .8) +
    geom_segment(data = rotulos, aes(x = date, xend = date + 25,
                                    y = valor * multiplicador, yend = rotulo_y, colour = cenario),
                 linewidth = .3, show.legend = FALSE) +
    geom_label(data = rotulos, aes(x = date + 25, y = rotulo_y, colour = cenario, label = rotulo),
               hjust = 0, size = 2.8, show.legend = FALSE) +
    facet_wrap(~factor(variavel, levels = campos), ncol = 2,
               scales = "free_y", labeller = as_labeller(titulos)) +
    scale_colour_manual(values = cores, breaks = names(cores)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0, .13))) +
    scale_y_continuous(expand = expansion(mult = c(.05, .18))) +
    labs(title = titulo,
         subtitle = paste("Observado ate", format(origem$date, "%m/%Y"),
                          "| Projecao ate", format(as.Date(fim_projecao), "%m/%Y")),
         x = NULL, y = if (multiplicador == 100) "pontos-base" else "% ao ano", colour = NULL,
         caption = paste0("JGB2Y: OIS2Y/ARIMA; OIS2Y migra do mercado para o juro do BVAR em 3 meses. 30Y-5Y: BVAR ancorado no fechamento.\n",
                          "5Y-2Y e 10Y-5Y: regressoes mensais desde 2016. Cenarios condicionais; sem bandas conjuntas de incerteza.")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
  ggsave(file.path(saida, nome), g, width = 12, height = 8, dpi = 170, bg = "white")
}
desenhar(variaveis, c(jgb2 = "JGB2Y", jgb5 = "JGB5Y", jgb10 = "JGB10Y", jgb30 = "JGB30Y"),
         "curva_jgb_rendimentos_cenarios.png", "Rendimentos dos JGBs | quatro cenarios")
desenhar(c("spread5_2", "spread10_2", "spread30_10", "spread30_5", "spread30_2"),
         c(spread5_2 = "JGB5Y - JGB2Y", spread10_2 = "JGB10Y - JGB2Y",
           spread30_10 = "JGB30Y - JGB10Y", spread30_5 = "JGB30Y - JGB5Y",
           spread30_2 = "JGB30Y - JGB2Y"),
         "curva_jgb_spreads_cenarios.png", "Spreads da curva JGB | quatro cenarios", 100)
