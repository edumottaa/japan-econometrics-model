# Separar o efeito dos lags do efeito de lambda; cenario base.
Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(TZ = "America/Sao_Paulo")
library(fpp3)
library(tidyverse)
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
somente_funcoes_graficos <- TRUE
source("src/7- forecast/comparacao hiato e priors/2- graficos forecast bvars.R", encoding = "UTF-8")
somente_funcoes_graficos <- FALSE
raiz <- "src/Outputs/Forecast BVAR estrutural"
pastas <- c("overnight fim de mes", "overnight fim de mes lag12",
            "overnight fim de mes lag12 lambda010")
nomes <- c("6 lags | lambda 0,20", "12 lags | lambda 0,20", "12 lags | lambda 0,10")
resultados <- lapply(pastas, function(p) readRDS(file.path(raiz, p, "forecast_bvar.rds")))
stopifnot(all(vapply(resultados, function(r) identical(r$dados_modelo, resultados[[1]]$dados_modelo) &&
  identical(r$caminhos, resultados[[1]]$caminhos), logical(1))))
graficos <- lapply(resultados, preparar_grafico)
variaveis_domesticas <- c("atividade", "cpi_core", "juro_curto", "inclinacao", "usdjpy")
resumo <- bind_rows(lapply(seq_along(graficos), function(i)
  graficos[[i]]$resumo |> mutate(modelo = nomes[i]))) |>
  filter(cenario == "Base", variavel %in% variaveis_domesticas) |>
  mutate(variavel = factor(variavel, levels = variaveis_domesticas),
         modelo = factor(modelo, levels = nomes))
historico <- graficos[[1]]$historico |>
  filter(date >= yearmonth("2019 Jan"), variavel %in% variaveis_domesticas) |>
  mutate(variavel = factor(variavel, levels = variaveis_domesticas))
transicao <- resumo |> filter(date == min(date)) |>
  left_join(historico |> filter(date == resultados[[1]]$origem) |>
    select(variavel, inicio = valor), by = "variavel")
g <- resumo |> ggplot(aes(date, media, colour = modelo)) +
  geom_line(linewidth = .8) +
  geom_line(data = historico, aes(date, valor), colour = "black", inherit.aes = FALSE) +
  geom_segment(data = transicao,
    aes(x = resultados[[1]]$origem, y = inicio, xend = date, yend = media), linetype = "dashed") +
  facet_wrap(~variavel, ncol = 1, scales = "free_y", labeller = as_labeller(titulos)) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y",
    limits = as.Date(c("2019-01-01", "2028-12-01")), expand = expansion(mult = 0)) +
  scale_colour_manual(values = setNames(c("blue", "#E66101", "#C51B7D"), nomes)) +
  labs(title = "BVAR estrutural | lags e lambda | cenario base", x = NULL, y = NULL,
    colour = NULL, caption = "Mesmos dados, identificacao, mu = delta = 1 e condicoes externas. Sem prior adicional de inflacao.") +
  theme(legend.position = "bottom")
saida <- file.path(raiz, pastas[3])
ggsave(file.path(saida, "comparacao_lags_lambda.png"), g, width = 12, height = 14, dpi = 160, bg = "white")
g_zoom <- g
g_zoom$layers[[2]] <- geom_point(data = historico |> filter(date >= resultados[[1]]$origem),
  aes(date, valor), colour = "black", size = 1.2, inherit.aes = FALSE)
g_zoom <- g_zoom + scale_x_yearmonth(date_breaks = "6 months", date_labels = "%b/%Y",
  limits = as.Date(c(resultados[[1]]$origem, max(resumo$date))), expand = expansion(mult = .01)) +
  labs(title = "BVAR estrutural | comparacao das trajetorias previstas")
ggsave(file.path(saida, "comparacao_forecast.png"), g_zoom, width = 12, height = 14, dpi = 160, bg = "white")
write_csv(resumo |> mutate(date = as.Date(date)), file.path(saida, "comparacao_lags_lambda.csv"))
marcos <- resumo |> filter(date %in% yearmonth(c("2026 Dec", "2027 Dec", "2028 Aug"))) |>
  select(variavel, date, modelo, media) |> pivot_wider(names_from = modelo, values_from = media)
write_csv(marcos |> mutate(date = as.Date(date)), file.path(saida, "comparacao_marcos.csv"))
print(marcos, n = Inf)
