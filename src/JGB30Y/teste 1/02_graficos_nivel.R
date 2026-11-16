# JGB30Y | 3. Nível observado e parcelas acumuladas desde 2020
library(fpp3)
library(readr)

pasta <- "src/JGB30Y/teste 1/resultados"
diario <- read_csv("src/JGB30Y/resultados/mercado_diario.csv",
                   show_col_types = FALSE) |>
  mutate(date = ymd(date)) |>
  filter(complete.cases(d_jgb30_bp, d_jgb2_bp, d_ust30_bp))

# Mesma regressao explicativa do Teste 1, estimada na amostra inteira.
ajuste <- lm(d_jgb30_bp ~ d_jgb2_bp + d_ust30_bp, data = diario)
b <- coef(ajuste)
base <- diario |> filter(date < ymd("2021-01-01")) |> slice_tail(n = 1)
stopifnot(base$date == ymd("2020-12-30"))

nivel <- diario |>
  filter(date >= ymd("2021-01-01")) |>
  transmute(date, jgb30, nivel_inicial = base$jgb30,
            parcela_jgb2 = b["d_jgb2_bp"] * (jgb2 - base$jgb2),
            parcela_ust30 = b["d_ust30_bp"] * (ust30 - base$ust30)) |>
  mutate(nivel_sem_residuo = nivel_inicial + parcela_jgb2 + parcela_ust30,
         residuo_acumulado = jgb30 - nivel_sem_residuo,
         residuo_bp = 100 * residuo_acumulado)
stopifnot(max(abs(nivel$jgb30 - nivel$nivel_inicial - nivel$parcela_jgb2 -
                  nivel$parcela_ust30 - nivel$residuo_acumulado)) < 1e-10)
write_csv(nivel, file.path(pasta, "teste1_nivel_desde2021.csv"))

# Cada faixa liga o nível antes e depois de uma parcela, inclusive quando negativa.
faixas <- bind_rows(
  nivel |> transmute(date, componente = "Nível inicial", de = 0,
                     ate = nivel_inicial),
  nivel |> transmute(date, componente = "JGB2", de = nivel_inicial,
                     ate = nivel_inicial + parcela_jgb2),
  nivel |> transmute(date, componente = "UST30",
                     de = nivel_inicial + parcela_jgb2,
                     ate = nivel_sem_residuo),
  nivel |> transmute(date, componente = "Não explicado",
                     de = nivel_sem_residuo, ate = jgb30)) |>
  mutate(componente = factor(componente,
                            levels = c("Nível inicial", "JGB2", "UST30",
                                       "Não explicado")))

marcos <- bind_rows(
  nivel |> filter(year(date) == 2021) |> slice_tail(n = 1),
  nivel |> filter(year(date) == 2024) |> slice_tail(n = 1),
  nivel |> slice_tail(n = 1)) |>
  mutate(rotulo = paste0(format(date, "%m/%Y"), ": ",
                         gsub("\\.", ",", sprintf("%.1f%%", residuo_acumulado))),
         rotulo = gsub("-0,0%", "0,0%", rotulo, fixed = TRUE),
         label_y = (jgb30 + nivel_sem_residuo) / 2,
         hjust = if_else(date == max(date), 1, 0.5))
write_csv(marcos |> select(date, jgb30, nivel_sem_residuo,
                          residuo_acumulado, residuo_bp, rotulo),
          file.path(pasta, "teste1_residuo_marcos.csv"))

grafico_nivel <- ggplot(faixas, aes(date, fill = componente,
                                   group = componente)) +
  geom_ribbon(aes(ymin = pmin(de, ate), ymax = pmax(de, ate)),
              alpha = 0.65) +
  geom_line(data = nivel, aes(date, jgb30), inherit.aes = FALSE,
            colour = "black", linewidth = 0.8) +
  geom_line(data = nivel, aes(date, nivel_sem_residuo),
            inherit.aes = FALSE, colour = "grey40", linetype = "dashed",
            linewidth = 0.4) +
  geom_label(data = marcos,
             aes(date, label_y, label = rotulo, hjust = hjust),
             inherit.aes = FALSE, fill = "#EAF6EE", colour = "#164B35",
             size = 3.5, linewidth = 0.15) +
  scale_fill_manual(values = c("Nível inicial" = "#D9DEE3", "JGB2" = "#E99F9B",
                               "UST30" = "#8DB7E8", "Não explicado" = "#55B889")) +
  scale_x_date(limits = range(nivel$date), expand = c(0, 0),
               date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1,
                                                  decimal.mark = ",", suffix = "%")) +
  labs(title = "JGB30: rendimento e parcelas acumuladas",
       subtitle = "Referência: 30/12/2020  |  Linha preta: JGB30 observado  |  Último dado: 30/10/2026",
       x = NULL, y = "% ao ano", fill = "Parcela",
       caption = "Rótulos: parcela verde acumulada; não identifica isoladamente o prêmio fiscal") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")
ggsave(file.path(pasta, "teste1_nivel_desde2021.png"), grafico_nivel,
       width = 12, height = 6.5, dpi = 160, bg = "white")

print(marcos)
