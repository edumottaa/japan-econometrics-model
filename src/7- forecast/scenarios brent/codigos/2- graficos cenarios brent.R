# Historico e quatro cenarios do Brent.

library(fpp3)
library(tidyverse)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
saida <- "src/Outputs/cenarios_brent"
dir.create(saida, recursive = TRUE, showWarnings = FALSE)

# 1. Construir os cenarios e ler o historico mensal em dolares.
source("src/7- forecast/scenarios brent/codigos/1- construir cenarios brent.R", local = TRUE, encoding = "UTF-8")
historico_brent <- readRDS("data/cache/cache_fred.rds") |>
  transmute(date = yearmonth(date), preco = DCOILBRENTEU, cenario = "Historico") |>
  filter(date >= yearmonth("2015 Jan"), date < min(cenarios_brent$date), !is.na(preco))

dados_grafico <- bind_rows(historico_brent, as_tibble(cenarios_brent)) |>
  mutate(cenario = factor(cenario, levels = c("Historico", "Base", "Benigno",
                                             "Adverso", "Adverso estrutural")))

# 2. Transicao entre o ultimo observado e o inicio dos cenarios.
transicao <- as_tibble(cenarios_brent) |>
  filter(date == min(date)) |>
  mutate(date_inicio = max(historico_brent$date),
         preco_inicio = historico_brent$preco[which.max(historico_brent$date)])

# 3. Comparar o historico e os cenarios.
grafico_brent <- dados_grafico |>
  ggplot(aes(x = date, y = preco, colour = cenario)) +
  geom_line() +
  geom_segment(data = transicao,
               aes(x = date_inicio, y = preco_inicio, xend = date, yend = preco),
               linetype = "dashed", show.legend = FALSE) +
  scale_colour_manual(values = c("Historico" = "black", "Base" = "blue",
                                "Benigno" = "green4", "Adverso" = "#E66101",
                                "Adverso estrutural" = "red")) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Brent | historico e cenarios", x = NULL, y = "US$/barril",
       subtitle = "Base: futuros | Benigno: P25 | Adverso: P75 | Adverso estrutural: P95",
       caption = "Historico: FRED (media mensal). Cenarios: aproximacao normal. Tracejado: transicao ilustrativa.",
       colour = "Serie")

if (interactive()) print(grafico_brent)
ggsave(file.path(saida, "cenarios.png"), grafico_brent,
       width = 11, height = 5, dpi = 180, bg = "white")
