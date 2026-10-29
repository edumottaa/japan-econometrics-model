# Historico e previsoes nas mesmas unidades de data visualization.

library(fpp3)
library(tidyverse)
library(distributional)

# Reiniciar o dispositivo gráfico antes de desenhar (erro depth(NULL) do grid).
graphics.off()

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
# Armazenamento da saída em outputs.
saida <- "src/Outputs/Forecast BVAR estrutural/pesos exponenciais"
dir.create(saida, recursive = TRUE, showWarnings = FALSE)

# 1. Ler o forecast e recuperar o historico em nivel.
resultado_forecast <- readRDS(file.path(saida, "forecast_bvar.rds"))
origem <- resultado_forecast$origem
variaveis_domesticas <- c("atividade", "cpi_core", "juro_curto", "inclinacao", "usdjpy")
historico_nivel <- resultado_forecast$dados_modelo |>
  as_tibble() |>
  select(date, all_of(variaveis_domesticas)) |>
  mutate(across(c(atividade, cpi_core, usdjpy), ~ exp(.x / 100))) |>
  arrange(date)

historico_forecast <- historico_nivel |>
  mutate(across(c(atividade, cpi_core), ~ 100 * (.x / lag(.x, 12) - 1))) |>
  filter(date >= yearmonth("2015 Jan")) |>
  pivot_longer(-date, names_to = "variavel", values_to = "valor")

# 2. CPI e atividade: variacao em 12 meses, calculada em cada trajetoria.
resumos_yoy <- list()
for (alvo in c("atividade", "cpi_core")) {
  for (cenario_atual in unique(resultado_forecast$previsoes$cenario)) {
    previsto <- resultado_forecast$previsoes |>
      as_tibble() |>
      filter(variavel == alvo, cenario == cenario_atual) |>
      arrange(date)
    sorteios <- do.call(rbind, parameters(previsto$valor)$x)
    passado <- matrix(tail(historico_nivel[[alvo]], 12), 12, ncol(sorteios))
    trajetorias <- rbind(passado, sorteios)
    variacao <- 100 * (trajetorias[12 + seq_len(nrow(sorteios)), , drop = FALSE] /
                        trajetorias[seq_len(nrow(sorteios)), , drop = FALSE] - 1)
    intervalos <- apply(variacao, 1, quantile, probs = c(.025, .10, .50, .90, .975))
    resumos_yoy[[length(resumos_yoy) + 1L]] <- tibble(
      cenario = cenario_atual, variavel = alvo, date = previsto$date,
      media = rowMeans(variacao), mediana = intervalos[3, ],
      inferior_95 = intervalos[1, ], inferior_80 = intervalos[2, ],
      superior_80 = intervalos[4, ], superior_95 = intervalos[5, ])
  }
}
resumo_forecast <- bind_rows(
  resultado_forecast$resumo |> filter(!variavel %in% c("atividade", "cpi_core")),
  bind_rows(resumos_yoy)
)

# Rótulos anuais: dezembro e o último mês da projeção, para cada cenário.
rotulos_anuais <- resumo_forecast |>
  group_by(variavel, cenario) |>
  filter(month(as.Date(date)) == 12 | date == max(date)) |>
  arrange(date, .by_group = TRUE) |>
  mutate(rotulo = formatC(media, format = "f", digits = 2, decimal.mark = ","),
         posicao = if_else(row_number() %% 2L == 1L, -0.8, 1.8)) |>
  ungroup()

# Afastar apenas os textos que se sobrepõem; manter os pontos nos valores previstos.
separar_rotulos <- function(y, distancia) {
  ordem <- order(y)
  posicao <- y[ordem]
  for (i in seq_along(posicao)[-1]) posicao[i] <- max(posicao[i], posicao[i - 1] + distancia)
  posicao <- posicao + mean(y) - mean(posicao)
  posicao[order(ordem)]
}

titulos <- c(atividade = "Atividade | variacao em 12 meses (%)",
             cpi_core = "CPI core | variacao em 12 meses (%)",
             juro_curto = "Juro curto", inclinacao = "JGB 10 anos - 2 anos", usdjpy = "USD/JPY")
unidades <- c(atividade = "%", cpi_core = "%", juro_curto = "% a.a.",
              inclinacao = "Pontos percentuais", usdjpy = "JPY por USD")
cores <- c("Base" = "blue", "Benigno" = "green4", "Adverso" = "#E66101",
           "Adverso estrutural" = "red")

# 3. Um grafico base e um com os quatro cenarios para cada variavel.
for (alvo in variaveis_domesticas) {
  historico <- historico_forecast |> filter(variavel == alvo)
  previsto <- resumo_forecast |> filter(variavel == alvo)
  rotulos <- rotulos_anuais |> filter(variavel == alvo, cenario == "Base")
  amplitude <- diff(range(c(previsto$media, historico$valor), na.rm = TRUE))
  rotulos_cenarios <- rotulos_anuais |> filter(variavel == alvo) |>
    mutate(data_rotulo = date + 1L,
      posicao_rotulo = separar_rotulos(media, .065 * amplitude), .by = date)
  transicao <- previsto |>
    filter(date == min(date)) |>
    mutate(date_inicio = origem, valor_inicio = tail(historico$valor, 1))

  grafico_base <- previsto |>
    filter(cenario == "Base") |>
    ggplot(aes(x = date, y = media)) +
    geom_ribbon(aes(ymin = inferior_95, ymax = superior_95), fill = "blue", alpha = 0.10) +
    geom_ribbon(aes(ymin = inferior_80, ymax = superior_80), fill = "blue", alpha = 0.18) +
    geom_line(colour = "blue") +
    geom_line(data = historico, aes(x = date, y = valor), colour = "black", inherit.aes = FALSE) +
    geom_segment(data = filter(transicao, cenario == "Base"),
                 aes(x = date_inicio, y = valor_inicio, xend = date, yend = media),
                 colour = "blue", linetype = "dashed") +
    geom_point(data = rotulos, colour = "blue", size = 1.3) +
    geom_text(data = rotulos, aes(label = rotulo, vjust = posicao), colour = "black", fontface = "bold", size = 3) +
    scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
    scale_y_continuous(expand = expansion(mult = c(.10, .15))) +
    labs(title = paste(titulos[alvo], "| cenario base"), x = NULL, y = unidades[alvo],
         caption = "Media e intervalos de 80% e 95%. Rotulos: dezembro e fim da projeção.")

  grafico_cenarios <- previsto |>
    ggplot(aes(x = date, y = media, colour = cenario)) +
    geom_line() +
    geom_line(data = historico, aes(x = date, y = valor), colour = "black", inherit.aes = FALSE) +
    geom_segment(data = transicao,
                 aes(x = date_inicio, y = valor_inicio, xend = date, yend = media),
                 linetype = "dashed", show.legend = FALSE) +
    geom_point(data = rotulos_cenarios, size = 1.5, show.legend = FALSE) +
    geom_segment(data = rotulos_cenarios, aes(xend = data_rotulo, yend = posicao_rotulo),
                 linewidth = .35, show.legend = FALSE) +
    geom_label(data = rotulos_cenarios,
               aes(x = data_rotulo, y = posicao_rotulo, label = rotulo),
               hjust = 0, size = 3, fontface = "bold", fill = "white", linewidth = 0,
               label.padding = grid::unit(.1, "lines"), show.legend = FALSE) +
    scale_colour_manual(values = cores, breaks = names(cores)) +
    scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
    labs(title = paste(titulos[alvo], "| quatro cenarios"), x = NULL, y = unidades[alvo],
         colour = "Cenario", caption = "Medias previstas. Rotulos: dezembro e fim da projeção. Preto: historico observado.") +
    theme(legend.position = "bottom")

  ggsave(file.path(saida, paste0(alvo, "_base.png")), grafico_base,
         width = 11, height = 5, dpi = 180, bg = "white")
  ggsave(file.path(saida, paste0(alvo, "_cenarios.png")), grafico_cenarios,
         width = 11, height = 5, dpi = 180, bg = "white")
  ggsave(file.path(saida, paste0(alvo, "_cenarios_rotulos_anuais.png")), grafico_cenarios,
         width = 11, height = 5, dpi = 180, bg = "white")
  if (interactive()) {
    print(grafico_base)
    print(grafico_cenarios)
  }
}

# 4. Painel do cenario base: cinco variaveis, de 2019 a 2028.
rotulos_painel <- paste(unname(titulos[variaveis_domesticas]),
                        paste0("(", unname(unidades[variaveis_domesticas]), ")"))
rotulos_painel[1:2] <- unname(titulos[variaveis_domesticas[1:2]])
base_painel <- resumo_forecast |>
  filter(cenario == "Base") |>
  mutate(painel = factor(variavel, levels = variaveis_domesticas, labels = rotulos_painel))
historico_painel <- historico_forecast |>
  filter(date >= yearmonth("2019 Jan")) |>
  mutate(painel = factor(variavel, levels = variaveis_domesticas, labels = rotulos_painel))
rotulos_painel_base <- rotulos_anuais |>
  filter(cenario == "Base") |>
  mutate(painel = factor(variavel, levels = variaveis_domesticas, labels = rotulos_painel))
transicao_painel <- base_painel |>
  filter(date == min(date)) |>
  left_join(historico_painel |> filter(date == origem) |> select(variavel, valor_inicio = valor),
            by = "variavel") |>
  mutate(date_inicio = origem)

grafico_painel_base <- base_painel |>
  ggplot(aes(x = date, y = media)) +
  geom_ribbon(aes(ymin = inferior_95, ymax = superior_95), fill = "blue", alpha = 0.10) +
  geom_ribbon(aes(ymin = inferior_80, ymax = superior_80), fill = "blue", alpha = 0.18) +
  geom_line(colour = "blue") +
  geom_line(data = historico_painel, aes(x = date, y = valor), colour = "black", inherit.aes = FALSE) +
  geom_segment(data = transicao_painel,
               aes(x = date_inicio, y = valor_inicio, xend = date, yend = media),
               colour = "blue", linetype = "dashed") +
  geom_point(data = rotulos_painel_base, colour = "blue", size = 1.1) +
  geom_text(data = rotulos_painel_base, aes(label = rotulo, vjust = posicao), colour = "black", fontface = "bold", size = 2.8) +
  facet_wrap(~painel, scales = "free_y", ncol = 1) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y",
                   limits = as.Date(c("2019-01-01", "2028-12-31")), expand = expansion(mult = 0)) +
  scale_y_continuous(expand = expansion(mult = c(.10, .18))) +
  labs(title = "Japao | cenario base", x = NULL, y = NULL,
       caption = "Media e intervalos de 80% e 95%. Rotulos: dezembro e fim da projeção.")

ggsave(file.path(saida, "painel_base_2019_2028.png"), grafico_painel_base,
       width = 12, height = 14, dpi = 180, bg = "white")

if (interactive()) print(grafico_painel_base)


# Atualizar o resumo utilizado pelos demais gráficos de cenários.
write_csv(resumo_forecast |> mutate(date = as.Date(date)),
          file.path(saida, "resumo_unidades_grafico.csv"))
