# Comparação das pontes e benchmarks: mesmos eixos entre períodos de cada métrica.

library(fpp3)
library(tidyverse)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
saida_cv_pib <- "src/Outputs/2- Evaluation/variaveis complementares/PIB/comparacao_pontes"
resultado <- readRDS(file.path(saida_cv_pib, "resultados_cv_pib.rds"))
cores <- c("Atividade + ARIMA" = "#355C7D",
  "ARIMA sem preditores" = "#777777", "Naive" = "#AAAAAA", "Drift" = "#CCCCCC")

# 1. Desempenho geral.
grafico_geral_pib <- resultado$geral |>
  pivot_longer(c(RMSE, MAE, CRPS, Winkler_80), names_to = "metrica", values_to = "erro") |>
  ggplot(aes(erro, factor(.model, levels = rev(names(cores))), fill = .model)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", erro)), hjust = -.15, size = 3) +
  scale_fill_manual(values = cores) +
  scale_x_continuous(expand = expansion(mult = c(0, .18))) +
  facet_wrap(~metrica, scales = "free_x", ncol = 2) +
  labs(title = "PIB | pontes e benchmarks no CV", x = "Erro em pontos percentuais | menor é melhor",
    y = NULL, caption = "Crescimento trimestral. Preditores previstos pelo BVAR; condicionamento externo ex post.") +
  theme_minimal(base_size = 11) + theme(panel.grid.major.y = element_blank())

# 2. Mesma escala horizontal nos três períodos de cada métrica.
grafico_periodos_pib <- resultado$periodos |>
  mutate(periodo = factor(periodo, levels = c("2015-2019", "2020-2021", "2022 em diante"))) |>
  pivot_longer(c(RMSE, CRPS), names_to = "metrica", values_to = "erro") |>
  ggplot(aes(erro, factor(.model, levels = rev(names(cores))), fill = .model)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", erro)), hjust = -.15, size = 3) +
  scale_fill_manual(values = cores) +
  scale_x_continuous(expand = expansion(mult = c(0, .18))) +
  facet_grid(periodo ~ metrica, scales = "free_x") +
  labs(title = "PIB | desempenho por período-alvo", x = "Erro em pontos percentuais | menor é melhor", y = NULL) +
  theme_minimal(base_size = 11) + theme(panel.grid.major.y = element_blank())

grafico_horizontes_pib <- resultado$horizontes |>
  ggplot(aes(h, RMSE, colour = .model)) + geom_line() + geom_point() +
  scale_colour_manual(values = cores) + scale_x_continuous(breaks = 1:8) +
  labs(title = "PIB | erro por horizonte", x = "Trimestres após a origem", y = "RMSE (p.p.)", colour = NULL) +
  theme_minimal() + theme(legend.position = "bottom")

# Armazenamento da saída em outputs.
ggsave(file.path(saida_cv_pib, "cv_pib_geral.png"), grafico_geral_pib,
  width = 12, height = 7, dpi = 160, bg = "white")
ggsave(file.path(saida_cv_pib, "cv_pib_por_periodo.png"), grafico_periodos_pib,
  width = 12, height = 10, dpi = 160, bg = "white")
ggsave(file.path(saida_cv_pib, "cv_pib_por_horizonte.png"), grafico_horizontes_pib,
  width = 11, height = 6, dpi = 160, bg = "white")
if (interactive()) print(grafico_periodos_pib)
