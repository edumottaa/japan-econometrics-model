# Refazer somente os gráficos a partir das métricas já calculadas.
# Executar com o diretório do projeto como diretório de trabalho.
library(tidyverse)

metricas_cpi <- read_csv(
  "data/processed/cv_trimestral_3_bvars_20261020_modelos_1_2/metricas_por_periodo_h24.csv",
  show_col_types = FALSE
) |>
  filter(variavel == "cpi_core", .model != "BVAR 7 | petroleo/UST") |>
  mutate(
    .model = factor(.model, levels = c("var", "drift", "naive", "ar", "BVAR 9 | ARIMA externos")),
    periodo = factor(periodo, levels = c("2015-2019", "2020-2021", "2022 em diante"))
  ) |>
  pivot_longer(c(RMSE, CRPS), names_to = "metrica", values_to = "valor")

cores <- c(
  ar = "#355C7D", naive = "#668BA8", drift = "#92AEC4", var = "#B9CAD8",
  "BVAR 9 | ARIMA externos" = "#A44725"
)
saida_plot <- "src/Outputs/plot data"
dir.create(saida_plot, recursive = TRUE, showWarnings = FALSE)

for (metrica_atual in c("RMSE", "CRPS")) {
  grafico <- metricas_cpi |>
    filter(metrica == metrica_atual) |>
    ggplot(aes(x = valor, y = .model, fill = .model)) +
    geom_col(width = .62) +
    geom_text(
      aes(label = formatC(valor, format = "f", digits = 2, decimal.mark = ",")),
      hjust = -.15, size = 4
    ) +
    facet_wrap(vars(periodo), ncol = 1, scales = "fixed") +
    scale_fill_manual(values = cores) +
    scale_x_continuous(limits = c(0, 4), breaks = seq(0, 4, .5),
                       expand = expansion(mult = c(0, 0))) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 12),
      panel.spacing.y = grid::unit(1.2, "lines"),
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, size = 9),
      plot.margin = margin(12, 22, 12, 12)
    ) +
    labs(
      title = paste("CV trimestral por período - CPI core |", metrica_atual),
      subtitle = "Data prevista | horizontes de 1 a 24 meses reunidos | menor é melhor",
      caption = paste(
        "BVAR 9 condicional ex post; lags e ordens dos modelos externos selecionados retrospectivamente.",
        "Incerteza dos caminhos previstos de IP/VIX não propagada.",
        sep = "\n"
      ),
      x = metrica_atual, y = NULL
    )

  print(grafico)
  ggsave(file.path(saida_plot, paste0("cpi_core_periodos_h24_", tolower(metrica_atual), ".png")),
         grafico, width = 10, height = 11, dpi = 180, bg = "white")
}
