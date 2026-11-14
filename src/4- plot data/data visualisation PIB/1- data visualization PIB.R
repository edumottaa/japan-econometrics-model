# Visualização do PIB e de sua relação histórica com a atividade.

library(fpp3)
library(tidyverse)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
dados_pib <- readRDS("data/processed/variaveis complementares/dados_pib.rds")

# Armazenamento da saída em outputs.
saida_pib <- "src/Outputs/1- plot data/variaveis complementares/PIB"
dir.create(saida_pib, recursive = TRUE, showWarnings = FALSE)

# 1. Crescimento trimestral das duas séries, sem anualização.
grafico_pib_atividade <- dados_pib |>
  select(date, PIB = pib_crescimento, Atividade = atividade_crescimento) |>
  pivot_longer(-date, names_to = "serie", values_to = "crescimento") |>
  ggplot(aes(date, crescimento, colour = serie)) +
  geom_line() + geom_hline(yintercept = 0, colour = "grey70") +
  facet_wrap(~serie, ncol = 1, scales = "free_y") +
  labs(title = "Japão | PIB real e índice coincidente", x = NULL,
    y = "Variação trimestral (%)", colour = NULL) +
  theme_minimal() + theme(legend.position = "none")
print(grafico_pib_atividade)
ggsave(file.path(saida_pib, "pib_e_atividade.png"), grafico_pib_atividade,
  width = 11, height = 7, dpi = 180, bg = "white")

# 2. A relação é preditiva; a reta não representa um efeito causal da atividade.
grafico_relacao_pib <- dados_pib |>
  ggplot(aes(atividade_crescimento, pib_crescimento)) +
  geom_point(alpha = .65) + geom_smooth(method = "lm", formula = y ~ x) +
  labs(title = "Japão | relação entre atividade e PIB",
    x = "Atividade: crescimento trimestral", y = "PIB real: crescimento trimestral") +
  theme_minimal()
print(grafico_relacao_pib)
ggsave(file.path(saida_pib, "relacao_pib_atividade.png"), grafico_relacao_pib,
  width = 9, height = 6, dpi = 180, bg = "white")
