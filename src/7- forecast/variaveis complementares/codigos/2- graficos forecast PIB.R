# Gráficos do PIB: cenário base e comparação dos quatro cenários.

library(fpp3)
library(tidyverse)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")

# Armazenamento da saída em outputs.
saida_pib <- "src/Outputs/6- Projeções complementares/PIB/forecast"
dir.create(saida_pib, recursive = TRUE, showWarnings = FALSE)
resultado_pib <- readRDS(file.path(saida_pib, "forecast_pib.rds"))
resumo <- resultado_pib$resumo
historico <- resultado_pib$historico |>
  mutate(crescimento_trimestral = 100 * (pib / lag(pib) - 1),
    crescimento_anual = 100 * (pib / lag(pib, 4) - 1)) |>
  as_tibble() |>
  pivot_longer(c(crescimento_trimestral, crescimento_anual), names_to = "variavel", values_to = "valor") |>
  filter(date >= yearquarter("2019 Q1"))
titulos_pib <- c(crescimento_trimestral = "PIB real | trimestre contra trimestre (%)",
  crescimento_anual = "PIB real | quatro trimestres (%)")
cores_pib <- c("Base" = "blue", "Benigno" = "green4", "Adverso" = "#E66101", "Adverso estrutural" = "red")

for (alvo in names(titulos_pib)) {
  previsto <- resumo |> filter(variavel == alvo)
  passado <- historico |> filter(variavel == alvo)
  transicao <- previsto |> filter(date == min(date)) |>
    mutate(inicio = resultado_pib$ultimo_pib_observado,
      valor_inicio = last(passado$valor))
  grafico_base <- previsto |> filter(cenario == "Base") |>
    ggplot(aes(date, media)) +
    geom_ribbon(aes(ymin = inferior_95, ymax = superior_95), fill = "blue", alpha = .12) +
    geom_ribbon(aes(ymin = inferior_80, ymax = superior_80), fill = "blue", alpha = .22) +
    geom_line(colour = "blue") +
    geom_line(data = passado, aes(date, valor), colour = "black") +
    geom_segment(data = transicao |> filter(cenario == "Base"),
      aes(x = inicio, y = valor_inicio, xend = date, yend = media), linetype = "dashed", colour = "blue") +
    labs(title = paste(titulos_pib[alvo], "| cenário base"), x = NULL, y = NULL,
      caption = "Média e intervalos de 80% e 95%. Intervalos condicionais à trajetória média da atividade do BVAR.") +
    theme_minimal()
  grafico_cenarios <- previsto |>
    ggplot(aes(date, media, colour = cenario)) + geom_line() +
    geom_line(data = passado, aes(date, valor), inherit.aes = FALSE, colour = "black") +
    geom_segment(data = transicao,
      aes(x = inicio, y = valor_inicio, xend = date, yend = media), linetype = "dashed") +
    scale_colour_manual(values = cores_pib) +
    labs(title = paste(titulos_pib[alvo], "| quatro cenários"), x = NULL, y = NULL, colour = "Cenário") +
    theme_minimal() + theme(legend.position = "bottom")
  print(grafico_base)
  print(grafico_cenarios)
  ggsave(file.path(saida_pib, paste0(alvo, "_base.png")), grafico_base,
    width = 11, height = 6, dpi = 180, bg = "white")
  ggsave(file.path(saida_pib, paste0(alvo, "_cenarios.png")), grafico_cenarios,
    width = 11, height = 6, dpi = 180, bg = "white")
}
