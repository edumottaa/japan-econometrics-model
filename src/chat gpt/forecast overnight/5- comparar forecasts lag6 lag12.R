# Comparar apenas o numero de lags, sem CV.
library(fpp3)
library(tidyverse)
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
somente_funcoes_graficos <- TRUE
source("src/7- forecast/comparacao hiato e priors/2- graficos forecast bvars.R", encoding = "UTF-8")
somente_funcoes_graficos <- FALSE
raiz <- "src/Outputs/Forecast BVAR estrutural"
resultado6 <- readRDS(file.path(raiz, "overnight fim de mes/forecast_bvar.rds"))
resultado12 <- readRDS(file.path(raiz, "overnight fim de mes lag12/forecast_bvar.rds"))
stopifnot(identical(resultado6$dados_modelo, resultado12$dados_modelo),
          identical(resultado6$caminhos, resultado12$caminhos),
          identical(resultado6$origem, resultado12$origem))
ajuste12 <- readRDS("data/processed/forecast_overnight_fim_mes lag12/modelo.rds")
ajuste6 <- readRDS("data/processed/forecast_overnight_fim_mes/modelo.rds")
stopifnot(ajuste12$configuracao$p == 12L, length(ajuste12$variaveis) == 7L,
  ajuste12$post$last_draw$p == 12L,
  identical(select(ajuste6$configuracao, -p), select(ajuste12$configuracao, -p)),
  dim(ajuste12$post$posterior$A)[2] == 85L,
  all(resultado12$diagnosticos$finitos), all(resultado12$diagnosticos$erro_condicoes <= 1e-8))

grafico6 <- preparar_grafico(resultado6)
grafico12 <- preparar_grafico(resultado12)
resumo <- bind_rows(mutate(grafico6$resumo, .model = "6 lags"),
                     mutate(grafico12$resumo, .model = "12 lags")) |>
  filter(cenario == "Base", variavel %in% names(titulos)[3:7]) |>
  mutate(variavel = factor(variavel, levels = names(titulos)))
historico <- grafico6$historico |>
  filter(date >= yearmonth("2019 Jan"), variavel %in% names(titulos)[3:7]) |>
  mutate(variavel = factor(variavel, levels = names(titulos)))
transicao <- resumo |> filter(date == min(date)) |>
  left_join(historico |> filter(date == resultado6$origem) |>
    select(variavel, valor_inicio = valor), by = "variavel")
g <- resumo |> ggplot(aes(date, media, colour = .model)) + geom_line(linewidth = .8) +
  geom_line(data = historico, aes(date, valor), colour = "black", inherit.aes = FALSE) +
  geom_segment(data = transicao,
    aes(x = resultado6$origem, y = valor_inicio, xend = date, yend = media), linetype = "dashed") +
  facet_wrap(~variavel, ncol = 1, scales = "free_y", labeller = as_labeller(titulos)) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y",
    limits = as.Date(c("2019-01-01", "2028-12-01")), expand = expansion(mult = 0)) +
  scale_colour_manual(values = c("6 lags" = "blue", "12 lags" = "#E66101")) +
  labs(title = "BVAR estrutural | 6 versus 12 lags | base", x = NULL, y = NULL, colour = NULL,
    caption = "Mesmos dados, priors e condicoes. Overnight de fim de mes desde marco/2024.") +
  theme(legend.position = "bottom")
pasta <- file.path(raiz, "overnight fim de mes lag12")
ggsave(file.path(pasta, "comparacao_lag6_lag12.png"), g, width = 12, height = 14, dpi = 160, bg = "white")
write_csv(resumo |> mutate(date = as.Date(date)), file.path(pasta, "comparacao_lag6_lag12.csv"))
marcos <- resumo |> filter(date %in% yearmonth(c("2026 Dec", "2027 Dec", "2028 Aug"))) |>
  select(.model, variavel, date, media) |> pivot_wider(names_from = .model, values_from = media)
write_csv(marcos |> mutate(date = as.Date(date)), file.path(pasta, "comparacao_marcos.csv"))
print(marcos, n = Inf)
cat("VALIDADO: lag 12; sete variaveis; mesma base e mesmas condicoes do lag 6.\n")
