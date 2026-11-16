# JGB2Y | 
#Forecast: OIS a termo da Bloomberg e gap mensal BVAR menos mercado.
source("src/JGB30Y/projecoes/00_dados.R", encoding = "UTF-8")
library(readxl)

ois <- read_xlsx(file.path(dados, "OIS jpy fwrd.xlsx"), skip = 1,
                 col_types = c("text", "numeric", "numeric")) |>
  transmute(inicio_ois = mdy(Date), mes = yearmonth(inicio_ois),
            curto_mercado_pct = `Forward Rate`, zero_pct = `Zero Rate`) |>
  arrange(mes)
stopifnot(all(is.finite(ois$curto_mercado_pct)),
          all(ois$mes[-1] == head(ois$mes, -1) + 1L), first(ois$mes) == mes_final + 1L)

# Forward de 1M em ACT/365. OIS2Y par aproximado com dois pagamentos anuais
montar_ois2 <- function(x) {
  x |> mutate(
    dias = as.numeric(lead(inicio_ois) - inicio_ois),
    desconto = cumprod(lag(1 / (1 + curto_mercado_pct / 100 * dias / 365), default = 1)),
    ano1 = as.numeric(lead(inicio_ois, 12) - inicio_ois) / 365,
    ano2 = as.numeric(lead(inicio_ois, 24) - lead(inicio_ois, 12)) / 365,
    ois2_termo_pct = 100 * (desconto - lead(desconto, 24)) /
      (ano1 * lead(desconto, 12) + ano2 * lead(desconto, 24)))
}
curva <- montar_ois2(ois)

insumos <- futuro |> select(mes, date, horizonte, juro_curto, extensao_bvar) |>
  left_join(curva |> select(mes, inicio_ois, curto_mercado_pct, ois2_termo_pct),
            by = "mes", relationship = "one-to-one")
stopifnot(all(is.finite(insumos$ois2_termo_pct)), all(is.finite(insumos$curto_mercado_pct)))

ois_historico <- read_csv(file.path(saida, "jgb2_cobertura_ois.csv"), show_col_types = FALSE) |>
  transmute(mes = yearmonth(date), ois2_pct)
historico <- mensal |> as_tibble() |> select(mes, date, jgb2, juro_curto) |>
  left_join(ois_historico, by = "mes", relationship = "one-to-one")
stopifnot(all(is.finite(historico$ois2_pct)))

# Ordem e defasagens do modelo base de 01_jgb2.R. O gap de mercado e uma sensibilidade.
ajustar_jgb2 <- function(x, inclui_curto = TRUE) {
  formula <- if (inclui_curto) {
    jgb2 ~ 1 + juro_curto + ois_lag2 + pdq(0, 0, 1) + PDQ(0, 0, 0)
  } else {
    jgb2 ~ 1 + ois_lag2 + pdq(0, 0, 1) + PDQ(0, 0, 0)
  }
  x |> mutate(ois_lag2 = lag(ois2_pct, 2L)) |>
    filter(!is.na(ois_lag2)) |> as_tsibble(index = mes) |>
    model(arima = ARIMA(formula))
}
ajuste <- ajustar_jgb2(historico)

projetar <- function(curto, ois_futuro = insumos$ois2_termo_pct, modelo = ajuste) {
  novos <- bind_rows(historico |> select(mes, ois2_pct, juro_curto),
                     insumos |> transmute(mes, ois2_pct = ois_futuro, juro_curto = curto)) |>
    arrange(mes) |> mutate(ois_lag2 = lag(ois2_pct, 2L)) |>
    filter(mes > mes_final) |> as_tsibble(index = mes)
  forecast(modelo, new_data = novos) |> as_tibble() |> pull(.mean)
}

projecao <- insumos |> mutate(
  mes = as.Date(mes), data_extracao = first(ois$inicio_ois), jgb2_origem = base$jgb2,
  jgb2_bvar = projetar(juro_curto), jgb2_curto_mercado = projetar(curto_mercado_pct),
  gap_curto_bp = 100 * (juro_curto - curto_mercado_pct),
  efeito_gap_jgb2_bp = 100 * (jgb2_bvar - jgb2_curto_mercado),
  delta_jgb2_bp = 100 * (jgb2_bvar - jgb2_origem))
write_csv(projecao, file.path(saida, "jgb2_projecao_curva_mercado.csv"))

taxa <- scales::label_number(accuracy = 0.01, decimal.mark = ",", suffix = "%")

# Historico desde 2016 e a mesma projecao em dois horizontes.
grafico_jgb2 <- function(meses) {
  linha <- projecao |> filter(horizonte <= meses) |> transmute(date, jgb2 = jgb2_bvar, extensao_bvar) |>
    bind_rows(tibble(date = base$date, jgb2 = base$jgb2, extensao_bvar = FALSE)) |> arrange(date)
  fim <- linha |> slice_tail(n = 1) |> mutate(rotulo = taxa(jgb2))
  extensao <- linha |> filter(extensao_bvar | lead(extensao_bvar, default = FALSE))
  nota <- paste0("OIS a termo reconstruído da extração Bloomberg de ",
                 format(first(ois$inicio_ois), "%d/%m/%Y"), ". Último JGB2Y observado em ",
                 format(base$date, "%d/%m/%Y"), ".")
  if (nrow(extensao) > 0) {
    nota <- paste0(nota, "\nTracejado: juro curto do BVAR mantido no nível de ",
                   format(first(extensao$date), "%m/%Y"), ".")
  }
  ggplot(historico, aes(date, jgb2)) +
    geom_vline(xintercept = base$date, colour = "grey65", linetype = "dotted") +
    geom_line(colour = "#242A30", linewidth = 0.9) +
    geom_line(data = filter(linha, !extensao_bvar), colour = "#337961", linewidth = 1.1) +
    geom_line(data = extensao, colour = "#337961", linewidth = 1.1, linetype = "21") +
    geom_point(data = fim, colour = "#337961", size = 2.4) +
    geom_text(data = fim, aes(label = rotulo), hjust = -0.2, colour = "#337961", fontface = "bold") +
    scale_x_date(breaks = seq(floor_date(min(historico$date), "year"), fim$date, by = "2 years"),
                 date_labels = "%Y", expand = expansion(mult = c(0.01, 0.08))) +
    scale_y_continuous(labels = scales::label_number(decimal.mark = ",", suffix = "%")) +
    labs(title = "JGB2Y com a curva OIS a termo",
         subtitle = paste("Projeção condicional até", format(fim$date, "%m/%Y")),
         x = NULL, y = "% ao ano", caption = nota) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))
}
ggsave(file.path(saida, "jgb2_projecao_curva_mercado.png"), grafico_jgb2(12L),
       width = 10, height = 5, dpi = 190, bg = "white")
ggsave(file.path(saida, "jgb2_projecao_curva_mercado_24m.png"), grafico_jgb2(24L),
       width = 10, height = 5, dpi = 190, bg = "white")

# Comparar o juro curto do BVAR com o forward mensal de TONA.
serie_bvar <- "BVAR Base"
serie_mercado <- "TONA implícito (forward de 1 mês)"
curvas_curto <- projecao |>
  transmute(mes, extensao_bvar, `BVAR Base` = juro_curto,
            `TONA implícito (forward de 1 mês)` = curto_mercado_pct) |>
  pivot_longer(-c(mes, extensao_bvar), names_to = "serie", values_to = "taxa")
ultimo_bvar <- max(projecao$mes[!projecao$extensao_bvar])
grafico_curto <- ggplot(curvas_curto, aes(mes, taxa, colour = serie)) +
  geom_line(data = filter(curvas_curto, serie == serie_mercado | !extensao_bvar), linewidth = 1) +
  geom_line(data = filter(curvas_curto, serie == serie_bvar, mes >= ultimo_bvar),
            linetype = "dashed", linewidth = 1, show.legend = FALSE) +
  scale_colour_manual(values = c("#315B7A", "#B45B42"), limits = c(serie_bvar, serie_mercado)) +
  scale_x_date(breaks = seq(min(projecao$mes), max(projecao$mes), by = "3 months"),
               date_labels = "%m/%Y") +
  scale_y_continuous(labels = taxa, breaks = seq(1.2, 2.2, by = 0.2)) +
  labs(title = "Juro curto japonês projetado pelo BVAR e implícito no mercado",
       subtitle = paste("Curva Bloomberg de", format(first(ois$inicio_ois), "%d/%m/%Y")),
       x = NULL, y = "% ao ano", colour = NULL,
       caption = paste0("Tracejado: taxa do BVAR de ", format(ultimo_bvar, "%m/%Y"),
                        " mantida até ", format(max(projecao$mes), "%m/%Y"), ".")) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        legend.justification = "left", plot.title = element_text(face = "bold"))
ggsave(file.path(saida, "juro_curto_bvar_vs_mercado.png"), grafico_curto,
       width = 10, height = 5, dpi = 190, bg = "white")

print(projecao |> filter(horizonte %in% c(12L, 24L)), width = Inf)
print(projecao |> summarise(curto_bvar = mean(juro_curto), curto_mercado = mean(curto_mercado_pct),
                            gap_24m_bp = mean(gap_curto_bp)))
print(tidy(ajuste))
