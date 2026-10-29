.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
Sys.setlocale("LC_CTYPE", "English_United States.utf8")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
saida <- "src/Outputs/Forecast BVAR estrutural/pesos exponenciais"
resultado <- readRDS(file.path(saida, "forecast_bvar.rds"))
stopifnot(isTRUE(all.equal(resultado$pesos_exponenciais$rho, .995)))
origem <- as.Date(resultado$origem)
variaveis <- c("atividade", "cpi_core", "juro_curto", "inclinacao", "usdjpy")
titulos <- c(atividade = "Atividade | variação em 12 meses", cpi_core = "CPI core | variação em 12 meses",
  juro_curto = "Juro curto", inclinacao = "JGB 10 anos − 2 anos", usdjpy = "USD/JPY")
unidades <- c(atividade = "%", cpi_core = "%", juro_curto = "% a.a.", inclinacao = "Pontos percentuais", usdjpy = "JPY por USD")
cores <- c("Base" = "blue", "Benigno" = "green4", "Adverso" = "#E66101", "Adverso estrutural" = "red")
previsto <- read_csv(file.path(saida, "resumo_unidades_grafico.csv"), show_col_types = FALSE) |>
  mutate(date = as.Date(date), cenario = factor(cenario, levels = names(cores)))
historico <- resultado$dados_modelo |> as_tibble() |> select(date, all_of(variaveis)) |>
  mutate(date = as.Date(date), across(c(atividade, cpi_core, usdjpy), ~ exp(.x / 100)),
    across(c(atividade, cpi_core), ~ 100 * (.x / lag(.x, 12) - 1))) |>
  filter(date >= as.Date("2025-01-01")) |>
  pivot_longer(-date, names_to = "variavel", values_to = "valor")

# Separar somente a posição dos textos; os valores previstos e os pontos permanecem iguais.
separar_rotulos <- function(y, distancia) {
  ordem <- order(y)
  posicao <- y[ordem]
  for (i in 2:length(posicao)) posicao[i] <- max(posicao[i], posicao[i - 1] + distancia)
  posicao <- posicao + mean(y) - mean(posicao)
  posicao[order(ordem)]
}

for (alvo in variaveis) {
  dados <- previsto |> filter(variavel == alvo)
  passado <- historico |> filter(variavel == alvo)
  amplitude <- diff(range(c(dados$media, passado$valor), na.rm = TRUE))
  rotulos <- dados |> filter(month(date) == 12 | date == max(date)) |>
    mutate(rotulo = formatC(media, format = "f", digits = 2, decimal.mark = ","), data_rotulo = date + 30) |>
    mutate(posicao_rotulo = separar_rotulos(media, .065 * amplitude), .by = date)
  stopifnot(nrow(rotulos) == 12, all(is.finite(rotulos$posicao_rotulo)))
  transicao <- dados |> filter(date == min(date)) |>
    mutate(valor_inicio = passado$valor[passado$date == origem])
  grafico <- ggplot(dados, aes(x = date, y = media, colour = cenario)) +
    geom_line(linewidth = .85) +
    geom_line(data = passado, aes(x = date, y = valor), inherit.aes = FALSE, colour = "black", linewidth = .65) +
    geom_segment(data = transicao, aes(x = origem, y = valor_inicio, xend = date, yend = media),
      linetype = "dashed", show.legend = FALSE) +
    geom_vline(xintercept = origem, linetype = "dotted", colour = "grey55") +
    geom_point(data = rotulos, size = 1.7, show.legend = FALSE) +
    geom_segment(data = rotulos, aes(xend = data_rotulo, yend = posicao_rotulo),
      linewidth = .4, alpha = .8, show.legend = FALSE) +
    geom_label(data = rotulos, aes(x = data_rotulo, y = posicao_rotulo, label = rotulo),
      hjust = 0, size = 3.8, fontface = "bold", fill = "white", linewidth = 0,
      label.padding = grid::unit(.12, "lines"), show.legend = FALSE) +
    scale_colour_manual(values = cores, breaks = names(cores)) +
    scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y",
      limits = as.Date(c("2025-01-01", "2028-12-31")), expand = expansion(mult = c(.01, .02))) +
    scale_y_continuous(expand = expansion(mult = c(.12, .16))) +
    theme_minimal(base_size = 12) + theme(legend.position = "top", panel.grid.minor = element_blank()) +
    labs(title = paste("Japão |", titulos[alvo]), subtitle = "Quatro cenários | Peso exponencial mensal: 0,995",
      caption = "Médias condicionais. Rótulos: dezembro/2026, dezembro/2027 e agosto/2028 (fim da projeção). Preto: histórico observado.",
      colour = "Cenário", x = NULL, y = unidades[alvo])
  ggsave(file.path(saida, paste0(alvo, "_cenarios_rotulos_anuais.png")), grafico,
    width = 12, height = 6, dpi = 180, bg = "white")
  message("GRAFICO: ", alvo, " | ", nrow(rotulos), " rotulos")
}
