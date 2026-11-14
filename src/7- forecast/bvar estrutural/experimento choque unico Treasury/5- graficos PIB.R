# PIB: cenário base com histórico desde 2019 e comparação dos quatro cenários.

Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
library(fpp3)
library(tidyverse)

trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/choque unico Treasury'
tipo <- commandArgs(trailingOnly=TRUE)[1]

# Armazenamento da saída em outputs.
saida_pib <- file.path(trabalho,tipo,'PIB')
resultado_pib <- readRDS(file.path(saida_pib, "forecast_pib.rds"))
previsto <- resultado_pib$resumo |> filter(variavel == "crescimento_anual")
historico <- resultado_pib$historico |>
  mutate(valor = 100 * (pib / lag(pib, 4) - 1)) |>
  filter(date >= yearquarter("2019 Q1"), is.finite(valor)) |>
  as_tibble()
cores_pib <- c("Base" = "#24629A", "Benigno" = "#34844A", "Adverso" = "#D57825",
  "Adverso estrutural" = "#BD3B3B")
rotulos <- previsto |> filter(quarter(date) == 4L | date == max(date)) |>
  mutate(rotulo = paste0(format(round(media, 2), nsmall = 2, decimal.mark = ","), "%"))
transicao <- previsto |> filter(date == min(date)) |>
  mutate(inicio = resultado_pib$ultimo_pib_observado, valor_inicio = last(historico$valor))

# 1. Cenário base: PIB real contra o mesmo trimestre do ano anterior, 2019-2028.
grafico_base <- previsto |> filter(cenario == "Base") |>
  ggplot(aes(date, media)) +
  geom_hline(yintercept = 0, colour = "grey65", linewidth = .3) +
  geom_ribbon(aes(ymin = inferior_95, ymax = superior_95), fill = cores_pib[["Base"]], alpha = .12) +
  geom_ribbon(aes(ymin = inferior_80, ymax = superior_80), fill = cores_pib[["Base"]], alpha = .23) +
  geom_line(data = historico, aes(date, valor), colour = "#303030", linewidth = .7) +
  geom_line(colour = cores_pib[["Base"]], linewidth = .85) +
  geom_segment(data = transicao |> filter(cenario == "Base"),
    aes(x = inicio, y = valor_inicio, xend = date, yend = media),
    linetype = "dashed", colour = cores_pib[["Base"]]) +
  geom_point(data = rotulos |> filter(cenario == "Base"), colour = cores_pib[["Base"]], size = 1.8) +
  geom_text(data = rotulos |> filter(cenario == "Base"), aes(label = rotulo),
    colour = cores_pib[["Base"]], vjust = -1, size = 3.5) +
  scale_x_yearquarter(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = c(.02, .05))) +
  labs(title = "PIB real | variação em 12 meses (%)", subtitle = "Cenário base | 2019-2028",
    x = NULL, y = NULL,
    caption = paste("Preto: observado. Azul: previsão média. Faixas: intervalos de 80% e 95%.",
      "\nTrimestre contra o mesmo trimestre do ano anterior. Última previsão: 2028 T2.",
      "\nIntervalos condicionais à trajetória média da atividade do BVAR.")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"), plot.caption = element_text(hjust = 0, size = 9))

# 2. Comparação: somente a projeção e o último ponto observado.
# Espaçar os rótulos próximos e ligá-los aos pontos; o valor do ponto não muda.
rotulos_cenarios <- rotulos |> group_by(date) |> arrange(media, .by_group = TRUE) |>
  mutate(posicao_rotulo = media + c(-.24, -.08, .08, .24)) |> ungroup()
grafico_cenarios <- previsto |>
  ggplot(aes(date, media, colour = cenario)) +
  geom_hline(yintercept = 0, colour = "grey65", linewidth = .3) +
  geom_line(linewidth = .9) +
  geom_segment(data = transicao,
    aes(x = inicio, y = valor_inicio, xend = date, yend = media), linetype = "dashed") +
  geom_point(data = transicao |> slice_head(n = 1), aes(x = inicio, y = valor_inicio),
    inherit.aes = FALSE, colour = "#303030", size = 2) +
  geom_point(data = rotulos, size = 1.8) +
  geom_segment(data = rotulos_cenarios,
    aes(xend = date, yend = posicao_rotulo), linewidth = .35, show.legend = FALSE) +
  geom_label(data = rotulos_cenarios, aes(y = posicao_rotulo, label = rotulo),
    linewidth = 0, size = 3.3, show.legend = FALSE, fill = "white") +
  scale_colour_manual(values = cores_pib) +
  scale_x_yearquarter(date_breaks = "6 months", labels = function(x) paste0(year(x), " T", quarter(x)),
    expand = expansion(mult = c(.04, .08))) +
  scale_y_continuous(expand = expansion(mult = c(.12, .15))) +
  labs(title = "PIB real | variação em 12 meses (%)", subtitle = paste("Quatro cenários |", if(tipo=="original") "BVAR original" else "BVAR com produção mundial via ARIMA"),
    x = NULL, y = NULL, colour = NULL,
    caption = paste("Rótulos no fim de cada ano e em 2028 T2. Preto: último PIB observado.",
      "\nProjeções condicionais aos caminhos de preços; não representam choques de oferta isolados.")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"), plot.caption = element_text(hjust = 0, size = 9))

ggsave(file.path(saida_pib, "crescimento_anual_base.png"), grafico_base,
  width = 11, height = 6.5, dpi = 160, bg = "white")
ggsave(file.path(saida_pib, "crescimento_anual_cenarios.png"), grafico_cenarios,
  width = 12, height = 7, dpi = 160, bg = "white")
if (interactive()) { print(grafico_base); print(grafico_cenarios) }
