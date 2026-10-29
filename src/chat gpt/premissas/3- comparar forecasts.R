Sys.setlocale("LC_CTYPE", "English_United States.utf8")
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
source("src/chat gpt/premissas/0- funcoes forecasts.R", encoding = "UTF-8")
modelos <- c("Original", "BoJ dezembro 1,50", "Inflacao suave", "Tendencia sem prior", "Prior longo prazo")
cores_modelos <- setNames(c("blue", "#E66101", "#7B3294", "grey45", "#D73027"), modelos)
tabelas <- list()
args <- commandArgs(trailingOnly = TRUE)
lags <- if (length(args)) as.integer(args[1]) else 6L
stopifnot(all(lags %in% c(6L, 12L)))
for (p in lags) {
  ref <- referencia(p)
  pasta <- file.path(raiz_premissas, paste0("lag", p))
  arquivos <- c("juro dezembro 1.50", "inflacao suave", "tendencia sem prior adicional", "prior inflacao longo prazo")
  resultados <- c(list(ref$resultado), lapply(arquivos, function(x) readRDS(file.path(pasta, x, "forecast_bvar.rds"))))
  stopifnot(all(vapply(resultados, function(x) identical(x$dados_modelo, ref$resultado$dados_modelo), logical(1))))
  graficos <- lapply(resultados, preparar_grafico)
  resumo <- bind_rows(lapply(seq_along(graficos), function(i) graficos[[i]]$resumo |>
    mutate(modelo = modelos[i], lags = p))) |>
    filter(cenario == "Base", variavel %in% alvos_base) |>
    mutate(variavel = factor(variavel, levels = alvos_base), modelo = factor(modelo, levels = modelos))
  historia <- graficos[[1]]$historico |> filter(date >= yearmonth("2019 Jan"), variavel %in% alvos_base) |>
    mutate(variavel = factor(variavel, levels = alvos_base))
  transicao <- resumo |> filter(date == min(date)) |>
    left_join(historia |> filter(date == ref$resultado$origem) |> select(variavel, inicio = valor), by = "variavel")
  construir <- function(alvos) resumo |> filter(variavel %in% alvos) |>
    ggplot(aes(date, media, colour = modelo)) + geom_line(linewidth = .7) +
    geom_line(data = filter(historia, variavel %in% alvos), aes(date, valor), inherit.aes = FALSE, colour = "black") +
    geom_segment(data = filter(transicao, variavel %in% alvos),
      aes(x = ref$resultado$origem, y = inicio, xend = date, yend = media), linetype = "dashed") +
    facet_wrap(~variavel, scales = "free_y", ncol = 1, labeller = as_labeller(titulos)) +
    scale_colour_manual(values = cores_modelos, drop = FALSE) +
    scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y",
      limits = as.Date(c("2019-01-01", "2028-12-01")), expand = expansion(mult = 0)) +
    labs(title = paste0("BVAR estrutural | ", p, " lags | cenario base"), x = NULL, y = NULL, colour = NULL,
      caption = "Comparacao de forecasts; sem novo CV. Prior e condicionamento suave sao hipoteses distintas.") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  ggsave(file.path(pasta, "comparacao_cinco_variaveis.png"), construir(alvos_base), width = 13, height = 14, dpi = 160, bg = "white")
  ggsave(file.path(pasta, "comparacao_inflacao_juros.png"), construir(c("cpi_core", "juro_curto")), width = 13, height = 8, dpi = 160, bg = "white")
  write_csv(resumo |> mutate(date = as.Date(date)), file.path(pasta, "comparacao_base.csv"))
  tabelas[[as.character(p)]] <- resumo
}
marcos <- bind_rows(tabelas) |>
  filter(date %in% yearmonth(c("2026 Dec", "2027 Dec", "2028 Aug"))) |>
  select(lags, modelo, variavel, date, media)
write_csv(marcos |> mutate(date = as.Date(date)), file.path(raiz_premissas, "comparacao_marcos.csv"))
print(filter(marcos, variavel %in% c("cpi_core", "juro_curto")), n = Inf)
