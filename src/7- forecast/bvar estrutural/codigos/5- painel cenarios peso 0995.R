.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
Sys.setlocale("LC_CTYPE", "English_United States.utf8")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
saida <- "src/Outputs/Forecast BVAR estrutural/pesos exponenciais"
resultado <- readRDS(file.path(saida, "forecast_bvar.rds"))
stopifnot(isTRUE(all.equal(resultado$pesos_exponenciais$rho, .995)),
  all(resultado$diagnosticos$finitos), all(resultado$diagnosticos$erro_condicoes < 1e-8))
origem <- resultado$origem
variaveis <- c("atividade", "cpi_core", "juro_curto", "inclinacao", "usdjpy")
titulos <- c("Atividade | variação em 12 meses (%)", "CPI core | variação em 12 meses (%)",
  "Juro curto (% a.a.)", "JGB 10 anos − 2 anos (p.p.)", "USD/JPY (JPY por USD)")
cores <- c("Base" = "blue", "Benigno" = "green4", "Adverso" = "#E66101", "Adverso estrutural" = "red")
previsto <- read_csv(file.path(saida, "resumo_unidades_grafico.csv"), show_col_types = FALSE) |>
  mutate(date = yearmonth(date), cenario = factor(cenario, levels = names(cores)),
    painel = factor(variavel, levels = variaveis, labels = titulos))
stopifnot(nrow(previsto) == 480, all(is.finite(previsto$media)), !anyNA(previsto$cenario),
  all((previsto |> count(variavel, cenario))$n == 24))
historico <- resultado$dados_modelo |> as_tibble() |> select(date, all_of(variaveis)) |>
  mutate(across(c(atividade, cpi_core, usdjpy), ~ exp(.x / 100)),
    across(c(atividade, cpi_core), ~ 100 * (.x / lag(.x, 12) - 1))) |>
  pivot_longer(-date, names_to = "variavel", values_to = "valor") |>
  mutate(painel = factor(variavel, levels = variaveis, labels = titulos))
transicao <- previsto |> filter(date == min(date)) |>
  left_join(historico |> filter(date == origem) |> select(variavel, valor_inicio = valor), by = "variavel")
for (zoom in c(FALSE, TRUE)) {
  inicio <- yearmonth(if (zoom) "2025 Jan" else "2019 Jan")
  grafico <- ggplot(previsto, aes(x = date, y = media, colour = cenario)) +
    geom_line(linewidth = .8) +
    geom_line(data = filter(historico, date >= inicio), aes(x = date, y = valor),
      inherit.aes = FALSE, colour = "black", linewidth = .6) +
    geom_segment(data = transicao, aes(x = origem, y = valor_inicio, xend = date, yend = media),
      linetype = "dashed", show.legend = FALSE) +
    geom_vline(xintercept = as.Date(origem), linetype = "dotted", colour = "grey55") +
    facet_wrap(vars(painel), ncol = 1, scales = "free_y") +
    scale_colour_manual(values = cores, breaks = names(cores)) +
    scale_x_yearmonth(date_breaks = if (zoom) "6 months" else "1 year",
      date_labels = if (zoom) "%m/%Y" else "%Y", limits = as.Date(c(inicio, yearmonth("2028 Dec"))),
      expand = expansion(mult = c(.01, .02))) +
    scale_y_continuous(expand = expansion(mult = c(.10, .15))) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top", strip.text = element_text(face = "bold"),
      panel.spacing.y = grid::unit(1.2, "lines")) +
    labs(title = "Japão | BVAR estrutural | quatro cenários",
      subtitle = "Peso exponencial mensal: 0,995 | Projeções até agosto de 2028",
      caption = "Linhas coloridas: médias condicionais por cenário. Preto: histórico observado. Linha vertical: fim da amostra de estimação.",
      colour = "Cenário", x = NULL, y = NULL)
  ggsave(file.path(saida, if (zoom) "painel_cenarios_0995_zoom.png" else "painel_cenarios_0995_2019_2028.png"),
    grafico, width = 12, height = 14, dpi = 180, bg = "white")
}
print(previsto |> filter(date == max(date)) |> select(variavel, cenario, date, media), n = Inf)
