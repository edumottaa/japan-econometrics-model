# Slide 1 | Decomposição do JGB30Y no cenário futuro

# 1. Estimar o VAR mensal
# O VAR usa variações mensais de JGB2Y, UST30Y e JGB30Y
# Testamos até seis defasagens e guardamos o diagnóstico do modelo escolhido
source("src/JGB30Y/projecoes/00_dados.R", encoding = "UTF-8")
source(file.path("src/7- forecast/bvar estrutural",
                 "experimento oferta estrutural central/funcoes gaussianas.R"),
       encoding = "UTF-8")

y <- mensal |> as_tibble() |> select(jgb2, ust30, jgb30) |> as.matrix()
var <- ajustar_var(diff(y), max_lag = 6L)
write_csv(var$teste_lags, file.path(saida, "var_lags.csv"))
write_csv(diagnosticar_var(var), file.path(saida, "var_diagnostico.csv"))
stopifnot(var$raiz_maxima < 1)

# 2. Montar as trajetórias futuras
# do UST30Y, foi se usado a curva a termo  

arquivo_japao <- file.path(saida, "jgb30_projecao_spread_arima.csv")
if (!file.exists(arquivo_japao)) stop("Rode 11, 12 e 13 para gerar as trajetórias japonesas.")
trajetoria_japao <- read_csv(arquivo_japao, show_col_types = FALSE) |>
  transmute(mes = yearmonth(mes),
            jgb2_cenario = jgb2_mercado_pct,
            spread_bp,
            jgb30_cenario = jgb30_mercado_pct)
curva_ust30_disponivel <- any(is.finite(futuro$ust30_mercado_pct))
fonte_ust30 <- if (curva_ust30_disponivel) "Curva a termo" else "Constante (premissa provisória)"
condicoes <- futuro |>
  left_join(trajetoria_japao, by = "mes", relationship = "one-to-one") |>
  mutate(ust30_cenario = if (curva_ust30_disponivel) ust30_mercado_pct else base$ust30) |>
  filter(cumall(is.finite(jgb2_cenario) & is.finite(ust30_cenario) & is.finite(jgb30_cenario)))

h <- nrow(condicoes)
write_csv(tibble(p = var$p, raiz_maxima = var$raiz_maxima, meses_condicionados = h,
                 fonte_ust30, data_snapshot_ust30 = first(condicoes$data_snapshot),
                 data_origem = base$date),
          file.path(saida, "var_status.csv"))

if (h < 12) {
  message("VAR estimado. O gráfico depende de pelo menos 12 meses seguidos de insumos.")
} else {

  # 3. Calcular referências para a decomposição
  # O caminho do JGB30Y já vem do JGB2Y + spread 30–2 projetados
  # O VAR só calcula referências com JGB2Y e UST30Y condicionados
  jgb2_fixo <- rep(base$jgb2, h)
  ust30_fixo <- rep(base$ust30, h)
  condicionar <- function(jgb2, ust30) {
    cond <- cbind(jgb2 = diff(c(base$jgb2, jgb2)),
                  ust30 = diff(c(base$ust30, ust30)),
                  jgb30 = rep(NA_real_, h))
    base$jgb30 + cumsum(prever_var_condicionado(var, cond)$media[, "jgb30"])
  }

  ref_fixa <- condicionar(jgb2_fixo, ust30_fixo)
  ref_jgb2 <- condicionar(condicoes$jgb2_cenario, ust30_fixo)
  ref_ust30 <- condicionar(jgb2_fixo, condicoes$ust30_cenario)
  ref_condicionada <- condicionar(condicoes$jgb2_cenario, condicoes$ust30_cenario)
  cenario <- condicoes$jgb30_cenario
  stopifnot(max(abs(ref_jgb2 + ref_ust30 - ref_fixa - ref_condicionada)) < 1e-8,
            all(is.finite(cenario)))

  # 4. Calcular drivers do rendimento
  
  hist <- read_csv("src/JGB30Y/teste 1/resultados/teste1_nivel_desde2021.csv",
                   show_col_types = FALSE) |>
    filter(date <= base$date) |>
    transmute(date, jgb30, referencia = nivel_inicial, parcela_jgb2, parcela_ust30,
              parcela_restante = residuo_acumulado)
  ancora <- hist |> slice_tail(n = 1)
  stopifnot(ancora$date == base$date, near(ancora$jgb30, base$jgb30))
  projecao <- condicoes |>
    transmute(date, horizonte, extensao_bvar, fonte_ust30, data_snapshot_ust30 = data_snapshot,
              jgb2_cenario, ust30_cenario, spread_bp, jgb30 = cenario,
              jgb30_var_taxas_fixas = ref_fixa, jgb30_var_condicionado = ref_condicionada,
              dinamica_var_bp = 100 * (ref_fixa - base$jgb30),
              contribuicao_jgb2_bp = 100 * (ref_jgb2 - ref_fixa),
              contribuicao_ust30_bp = 100 * (ref_ust30 - ref_fixa),
              ajuste_cenario_bp = 100 * (cenario - ref_condicionada),
              referencia = ancora$referencia,
              parcela_jgb2 = ancora$parcela_jgb2 + contribuicao_jgb2_bp / 100,
              parcela_ust30 = ancora$parcela_ust30 + contribuicao_ust30_bp / 100,
              parcela_restante = ancora$parcela_restante + (dinamica_var_bp + ajuste_cenario_bp) / 100)
  stopifnot(all(with(projecao, near(jgb30, referencia + parcela_jgb2 +
                                     parcela_ust30 + parcela_restante))))
  write_csv(projecao, file.path(saida, "var_decomposicao_condicionada.csv"))

  # 5. Desenhar e salvar o gráfico
  # Áreas: parcelas acumuladas. Linha cheia: observado. Linha tracejada:
  # cenário condicionado. Os rótulos mostram o valor da parte verde.
  nivel <- bind_rows(hist, projecao)
  faixas <- nivel |>
    transmute(date, referencia, fim_jgb2 = referencia + parcela_jgb2,
              fim_ust30 = fim_jgb2 + parcela_ust30, jgb30)
  areas <- bind_rows(
    faixas |> transmute(date, componente = "Referência", de = 0, ate = referencia),
    faixas |> transmute(date, componente = "JGB2Y", de = referencia, ate = fim_jgb2),
    faixas |> transmute(date, componente = "UST30Y", de = fim_jgb2, ate = fim_ust30),
    faixas |> transmute(date, componente = "Parcela restante", de = fim_ust30, ate = jgb30)
  )
  rotulos <- bind_rows(hist |> filter(year(date) == 2021) |> slice_tail(n = 1),
                       hist |> filter(year(date) == 2024) |> slice_tail(n = 1),
                       ancora, nivel |> slice_tail(n = 1)) |>
    mutate(y = jgb30 - parcela_restante / 2,
           rotulo = scales::number(parcela_restante, accuracy = 0.1,
                                   decimal.mark = ",", suffix = "%"))
  cores <- c("Referência" = "#D9DEE3", "JGB2Y" = "#E99F9B",
             "UST30Y" = "#8DB7E8", "Parcela restante" = "#55B889")
  subtitulo_ust <- if (curva_ust30_disponivel) "UST30Y condicionado à curva a termo" else
    paste0("Cenário provisório com UST30Y constante em ",
           scales::number(base$ust30, accuracy = 0.01, decimal.mark = ",", suffix = "%"))
  grafico <- ggplot(areas, aes(date, fill = componente)) +
    geom_ribbon(aes(ymin = pmin(de, ate), ymax = pmax(de, ate)), alpha = 0.67) +
    geom_line(data = hist, aes(date, jgb30), inherit.aes = FALSE, linewidth = 0.75) +
    geom_line(data = nivel |> filter(date >= base$date), aes(date, jgb30),
              inherit.aes = FALSE, linewidth = 0.75, linetype = "dashed") +
    geom_vline(xintercept = base$date, colour = "grey50", linetype = "dotted") +
    geom_label(data = rotulos, aes(date, y, label = rotulo), inherit.aes = FALSE,
               fill = "#EAF6EE", colour = "#164B35", size = 3.5, linewidth = 0.15) +
    scale_fill_manual(values = cores, breaks = names(cores)) +
    scale_x_date(breaks = seq(floor_date(min(hist$date), "year"), max(nivel$date), by = "year"),
                 date_labels = "%Y",
                 expand = expansion(mult = c(0.015, 0.035))) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.1,
                                                     decimal.mark = ",", suffix = "%")) +
    labs(title = "JGB30Y | Rendimento e parcelas acumuladas",
          subtitle = paste0("Histórico: regressão diária. Futuro: VAR(", var$p,
                            ") mensal. ", subtitulo_ust, "."),
         x = NULL, y = "% ao ano", fill = NULL,
          caption = "Linha tracejada: JGB2Y + spread projetados. Verde: parcela restante, sem identificação fiscal.") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom")
  ggsave(file.path(saida, "slide1_decomposicao_condicionada.png"), grafico,
         width = 12, height = 6.5, dpi = 180, bg = "white")
  print(projecao |> filter(horizonte %in% c(12L, 24L)) |>
          select(date, jgb30, jgb30_var_condicionado, dinamica_var_bp,
                 starts_with("contribuicao_"), ajuste_cenario_bp),
        width = Inf)
}
