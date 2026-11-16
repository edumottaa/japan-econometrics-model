# Sensibilidade da referência VAR a trajetórias hipotéticas do UST30Y.
# O JGB30Y do cenário vem do script 13; a referência não o substitui.
# Os caminhos do UST30Y abaixo não são curvas a termo da Bloomberg.
source("src/JGB30Y/projecoes/00_dados.R", encoding = "UTF-8")
source(file.path("src/7- forecast/bvar estrutural",
                 "experimento oferta estrutural central/funcoes gaussianas.R"),
       encoding = "UTF-8")

y <- mensal |> as_tibble() |> select(jgb2, ust30, jgb30) |> as.matrix()
var <- ajustar_var(diff(y), max_lag = 6L)
trajetoria <- read_csv(file.path(saida, "jgb30_projecao_spread_arima.csv"),
                       show_col_types = FALSE) |>
  transmute(mes = yearmonth(mes), jgb2_cenario = jgb2_mercado_pct,
            jgb30_cenario = jgb30_mercado_pct)
condicoes <- futuro |>
  left_join(trajetoria, by = "mes", relationship = "one-to-one") |>
  filter(is.finite(jgb2_cenario), is.finite(jgb30_cenario))
stopifnot(nrow(condicoes) == 24L)

# Cada caminho chega a +/-50 pb em 12 meses e fica nesse nível até 24 meses.
cenarios <- c("UST30 -50 pb" = -0.5, "UST30 constante" = 0,
              "UST30 +50 pb" = 0.5)
sensibilidade <- imap_dfr(cenarios, function(choque, nome) {
  ust30 <- base$ust30 + choque * pmin(condicoes$horizonte, 12L) / 12
  imposicao <- cbind(jgb2 = diff(c(base$jgb2, condicoes$jgb2_cenario)),
                     ust30 = diff(c(base$ust30, ust30)),
                     jgb30 = rep(NA_real_, nrow(condicoes)))
  jgb30_var <- base$jgb30 + cumsum(prever_var_condicionado(var, imposicao)$media[, "jgb30"])
  tibble(cenario_ust30 = nome, date = condicoes$date,
         horizonte = condicoes$horizonte, ust30_cenario_pct = ust30,
         jgb30_var_livre_pct = jgb30_var,
         jgb30_spread_imposto_pct = condicoes$jgb30_cenario,
         ajuste_ao_cenario_bp = 100 * (condicoes$jgb30_cenario - jgb30_var))
})

write_csv(sensibilidade, file.path(saida, "var_sensibilidade_ust30.csv"))

linhas <- bind_rows(
  mercado |>
    filter(date >= ymd("2022-01-01")) |>
    transmute(date, serie = "Observado", taxa = jgb30),
  sensibilidade |>
    transmute(date, serie = cenario_ust30, taxa = jgb30_var_livre_pct),
  sensibilidade |>
    filter(cenario_ust30 == "UST30 constante") |>
    transmute(date, serie = "JGB2Y + spread projetado", taxa = jgb30_spread_imposto_pct)
)
cores <- c("Observado" = "#24282D", "UST30 -50 pb" = "#6DA8CB",
           "UST30 constante" = "#286C9B", "UST30 +50 pb" = "#153F63",
           "JGB2Y + spread projetado" = "#B96344")
grafico <- ggplot(linhas, aes(date, taxa, colour = serie)) +
  geom_line(linewidth = 0.85) +
  geom_vline(xintercept = base$date, linetype = "dotted", colour = "grey55") +
  scale_colour_manual(values = cores, breaks = names(cores)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1,
                                                  decimal.mark = ",", suffix = "%")) +
  labs(title = "JGB30Y: cenário externo e referências do VAR",
       subtitle = "O VAR serve à decomposição; caminhos do UST30Y são hipóteses",
       x = NULL, y = "% ao ano", colour = NULL,
       caption = "JGB30Y = JGB2Y + spread projetados. A referência VAR varia com ±50 pb no UST30Y até 12 meses.") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")
ggsave(file.path(saida, "jgb30_var_sensibilidade_ust30.png"), grafico,
       width = 11, height = 6, dpi = 180, bg = "white")

print(sensibilidade |>
        filter(horizonte %in% c(12L, 24L)) |>
        select(cenario_ust30, date, ust30_cenario_pct,
               jgb30_var_livre_pct, jgb30_spread_imposto_pct,
               ajuste_ao_cenario_bp), width = Inf)
