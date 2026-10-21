# Gráficos das séries nas unidades usuais
# Executar após src/2- data_cleaning.R, com o objeto data em nível.
# CPI core e atividade: variação em 12 meses, e não soma das taxas YoY.

library(fpp3)
library(tidyverse)

# 1. Preparar as séries sem as transformações logarítmicas do BVAR.
# fill_gaps() garante que lag(..., 12) represente exatamente 12 meses.
data_plot <- data |>
  select(date, cpi_core, atividade, juro_curto, inclinacao_10_2,
         brent, ust2y, usdjpy, vix, ip_mundial, topix) |>
  arrange(date) |>
  fill_gaps() |>
  transmute(
    date,
    cpi_core_yoy = 100 * (cpi_core / lag(cpi_core, 12) - 1),
    atividade_yoy = 100 * (atividade / lag(atividade, 12) - 1),
    juro_curto,
    inclinacao_10_2,
    brent,
    ust2y,
    usdjpy,
    vix,
    ip_mundial,
    topix
  )

# Manter as definições das fontes: o juro curto é a série híbrida do modelo;
# TOPIX está em índice 2015 = 100, conforme a série OCDE/FRED utilizada.
rotulos <- c(
  cpi_core_yoy = "CPI core | variação em 12 meses (%)",
  atividade_yoy = "Atividade | variação em 12 meses (%)",
  juro_curto = "Juro curto | taxa-sombra até fev/2024; call rate após (%)",
  inclinacao_10_2 = "JGB 10 anos - 2 anos (p.p.)",
  brent = "Petróleo Brent (US$/barril)",
  ust2y = "Treasury de 2 anos (% a.a.)",
  usdjpy = "USD/JPY (ienes por dólar)",
  vix = "VIX (pontos)",
  ip_mundial = "Produção industrial mundial (índice CPB)",
  topix = "TOPIX (índice 2015 = 100)"
)

saida_plot <- "src/Outputs/plot data"
dir.create(saida_plot, recursive = TRUE, showWarnings = FALSE)

# 2. Um gráfico por série, usando autoplot() no padrão do fpp3.
graficos_series <- list()
for (variavel in names(rotulos)) {
  grafico <- data_plot |>
    autoplot(!!sym(variavel), na.rm = TRUE) +
    labs(title = unname(rotulos[variavel]), x = NULL, y = NULL)

  graficos_series[[variavel]] <- grafico
  print(grafico)
  ggsave(file.path(saida_plot, paste0(variavel, ".png")), grafico,
         width = 11, height = 5, dpi = 180, bg = "white")
}

# 3. Painéis por grupo, como no script de visualização existente.
grupos <- list(
  inflacao_e_atividade = c("cpi_core_yoy", "atividade_yoy"),
  juros_petroleo_cambio = c("juro_curto", "inclinacao_10_2", "brent", "ust2y", "usdjpy"),
  vix_producao_mundial_topix = c("vix", "ip_mundial", "topix")
)
titulos <- c(
  inflacao_e_atividade = "Japão | CPI core e atividade em 12 meses",
  juros_petroleo_cambio = "Juros, petróleo e câmbio | séries em nível",
  vix_producao_mundial_topix = "VIX, produção mundial e TOPIX | séries em nível"
)

for (grupo in names(grupos)) {
  variaveis <- grupos[[grupo]]
  grafico <- data_plot |>
    as_tibble() |>
    mutate(date = as.Date(date)) |>
    select(date, all_of(variaveis)) |>
    pivot_longer(-date, names_to = "variavel", values_to = "valor") |>
    mutate(variavel = factor(variavel, levels = variaveis,
                            labels = unname(rotulos[variaveis]))) |>
    ggplot(aes(x = date, y = valor, colour = variavel)) +
    geom_line(na.rm = TRUE) +
    facet_wrap(~variavel, scales = "free_y", ncol = 1) +
    theme(legend.position = "none") +
    labs(title = unname(titulos[grupo]), x = NULL, y = NULL)

  print(grafico)
  ggsave(file.path(saida_plot, paste0(grupo, ".png")), grafico,
         width = 12, height = 3 * length(variaveis), dpi = 180, bg = "white")
}
