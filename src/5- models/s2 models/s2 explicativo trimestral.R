if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")
library(tidyverse)
library(fpp3)
library(readxl)
library(jsonlite)

# S2 explicativo: somente trimestres completos; taxas em 12 meses, não anualizadas.
# Dois modelos: (1) CPI core e seus fatores; (2) serviços e salários.
# Bens e serviços são uma decomposição contábil, não uma terceira regressão.
# FPP3: https://otexts.com/fpp3/dynamic-regression.html
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
if (file.exists(".Renviron")) readRenviron(".Renviron")
atualizar_dados_s2 <- FALSE
cache_s2 <- "data/cache/s2_explicativo_trimestral.rds"

# 1. Dados: reaproveitar os arquivos locais e guardar as consultas em cache.
if (atualizar_dados_s2 || !file.exists(cache_s2)) {
  # Usar a credencial já configurada na coleta, sem executar toda a coleta antiga.
  coleta <- readLines("src/1- data_collection/2- data_collection_ sInflation.R", encoding = "UTF-8")
  eval(parse(text = coleta[grepl("^app_id_estat <-", coleta)]))
  tabelas <- c("2015" = "0003143513", "2020" = "0003427113", "2025" = "0004052037")
  cpi <- imap_dfr(tabelas, function(tabela, base) {
    consulta <- fromJSON(paste0(
      "https://api.e-stat.go.jp/rest/3.0/app/json/getStatsData?appId=", app_id_estat,
      "&statsDataId=", tabela, "&cdTab=1,3&cdArea=00000",
      "&cdCat01=0161,0220,0241&cdTimeFrom=2010000101"))
    stopifnot(consulta$GET_STATS_DATA$RESULT$STATUS == 0)
    consulta$GET_STATS_DATA$STATISTICAL_DATA$DATA_INF$VALUE |>
      as_tibble() |>
      filter(substr(.data[["@time"]], 7, 8) != "00") |>
      transmute(date = yearmonth(ym(paste(substr(.data[["@time"]], 1, 4),
                                         substr(.data[["@time"]], 7, 8)))),
                base, serie = paste0("c", .data[["@cat01"]], "_", .data[["@tab"]]),
                valor = suppressWarnings(as.numeric(.data[["$"]])) ) |>
      filter(!is.na(date)) |>
      pivot_wider(names_from = serie, values_from = valor)
  })
  tankan <- fromJSON(paste0("https://www.stat-search.boj.or.jp/api/v1/getDataCode?",
    "format=json&lang=en&db=CO&code=TK99F0000204HCQ00000&startDate=201401"))
  periodos <- tankan$RESULTSET$VALUES$SURVEY_DATES[[1]]
  expectativas <- tibble(
    date = yearquarter(make_date(periodos %/% 100, (periodos %% 100) * 3)),
    expectativa = as.numeric(tankan$RESULTSET$VALUES$VALUES[[1]]))
  boj_importacao <- fromJSON(paste0("https://www.stat-search.boj.or.jp/api/v1/getDataCode?",
    "format=json&lang=en&db=PR01&code=PRCG20_2600000000&startDate=201001"))
  importacoes <- tibble(
    date = yearmonth(ym(boj_importacao$RESULTSET$VALUES$SURVEY_DATES[[1]])),
    indice_importacao = as.numeric(boj_importacao$RESULTSET$VALUES$VALUES[[1]]))
  stopifnot(!anyDuplicated(expectativas$date), !anyDuplicated(importacoes$date))
  saveRDS(list(cpi = cpi, expectativas = expectativas, importacoes = importacoes,
               extraido_em = Sys.time()), cache_s2)
}
fontes_s2 <- readRDS(cache_s2)

# BoJ: ajuste institucional e decomposição do hiato em trabalho e capital.
# Arquivos já mantidos pela coleta do projeto; TRUE também os atualiza.
if (atualizar_dados_s2 || !file.exists("data/cpirev.xlsx"))
  download.file("https://www.boj.or.jp/en/research/research_data/cpi/cpirev.xlsx",
                "data/cpirev.xlsx", mode = "wb")
if (atualizar_dados_s2 || !file.exists("data/gap.xlsx"))
  download.file("https://www.boj.or.jp/en/research/research_data/gap/gap.xlsx",
                "data/gap.xlsx", mode = "wb")
cpi_ajustado <- read_excel("data/cpirev.xlsx", skip = 4, col_names = FALSE,
                           col_types = "text", .name_repair = "minimal")
names(cpi_ajustado) <- paste0("v", seq_len(ncol(cpi_ajustado)))
cpi_ajustado <- cpi_ajustado |>
  transmute(date = yearmonth(as.Date(as.numeric(v1), origin = "1899-12-30")),
            base = case_when(!is.na(v2) ~ "2025", !is.na(v3) ~ "2020",
                             !is.na(v4) ~ "2015"),
            inflacao_sem_institucionais = coalesce(as.numeric(v2), as.numeric(v3), as.numeric(v4))) |>
  filter(!is.na(base), !is.na(date))
hiato_boj <- read_excel("data/gap.xlsx", sheet = "data1", skip = 5,
                        col_names = FALSE, col_types = "text") |>
  transmute(date = yearquarter(...1), hiato = as.numeric(...2),
            hiato_capital = as.numeric(...3), hiato_trabalho = as.numeric(...4)) |>
  filter(!is.na(date), !is.na(hiato))
# A coluna de salário do projeto é a variação em 12 meses dos salários regulares nominais.
salarios_s2 <- readRDS("data/cache/cache_salarios.rds") |>
  transmute(date = yearmonth(date), salario = as.numeric(salario))

# 2. Preparação mensal apenas para construir corretamente as observações trimestrais.
# Pesos oficiais por 10.000, na mesma base dos índices de cada trecho.
# 2015: https://www.stat.go.jp/data/cpi/2015/kaisetsu/pdf/5-2.pdf
# 2020: https://www.stat.go.jp/data/cpi/sokuhou/nen/pdf/zen-n.pdf (dez/2025, tabela 4)
# 2025: https://www.stat.go.jp/data/cpi/sokuhou/tsuki/pdf/zenkoku.pdf (set/2026, tabela 4)
pesos_cpi <- tribble(
  ~base, ~peso_core, ~peso_bens, ~peso_servicos,
  "2015", 9586, 4555, 5031,
  "2020", 9604, 4650, 4954,
  "2025", 9621, 4657, 4964)
stopifnot(all(pesos_cpi$peso_bens + pesos_cpi$peso_servicos == pesos_cpi$peso_core))

cpi_componentes <- fontes_s2$cpi |>
  group_by(base) |>
  arrange(date, .by_group = TRUE) |>
  mutate(core_anterior = lag(c0161_1, 12), servicos_anterior = lag(c0220_1, 12),
         bens_anterior = lag(c0241_1, 12)) |>
  ungroup() |>
  inner_join(cpi_ajustado, by = c("date", "base")) |>
  left_join(pesos_cpi, by = "base") |>
  transmute(date, inflacao = c0161_3, inflacao_servicos = c0220_3,
            inflacao_bens = c0241_3, inflacao_sem_institucionais,
            governo_institucional = inflacao - inflacao_sem_institucionais,
            peso_servicos_anterior = peso_servicos * servicos_anterior / (peso_core * core_anterior),
            contribuicao_servicos = 100 * peso_servicos * (c0220_1 - servicos_anterior) /
              (peso_core * core_anterior),
            contribuicao_bens = 100 * peso_bens * (c0241_1 - bens_anterior) /
              (peso_core * core_anterior))
importacao_s2 <- fontes_s2$importacoes |>
  arrange(date) |>
  mutate(importacao = 100 * (indice_importacao / lag(indice_importacao, 12) - 1)) |>
  select(date, importacao)

# Médias das taxas e contribuições mensais; nenhuma repetição de dado trimestral em meses.
# 2016 em diante: usar os pesos de 2015 a partir de um ano após a base.
dados_s2_trimestral <- cpi_componentes |>
  left_join(importacao_s2, by = "date") |>
  left_join(salarios_s2, by = "date") |>
  filter(date >= yearmonth("2016 Jan"), date < yearmonth(Sys.Date())) |>
  mutate(date = yearquarter(date)) |>
  group_by(date) |>
  summarise(meses = n(), across(where(is.numeric),
    ~ if (n() == 3L && all(is.finite(.x))) mean(.x) else NA_real_), .groups = "drop") |>
  filter(meses == 3L) |>
  left_join(fontes_s2$expectativas, by = "date") |>
  left_join(hiato_boj, by = "date") |>
  arrange(date) |>
  as_tsibble(index = date)
stopifnot(!anyDuplicated(dados_s2_trimestral$date))

# 3. Regressão 1: expectativas, importação e hiato de trabalho/capital.
# Governo/instituições já é uma contribuição medida pelo BoJ: entra com coeficiente 1.
# Estimar o CPI ajustado e somar a contribuição evita regredir o CPI numa diferença dele mesmo.
# A medida inclui impostos, subsídios e outros ajustes institucionais; não é fiscal puro.
amostra_fatores <- dados_s2_trimestral |>
  select(date, inflacao, inflacao_sem_institucionais, governo_institucional,
         expectativa, importacao, hiato_trabalho, hiato_capital) |>
  drop_na()
modelo_fatores_s2 <- amostra_fatores |>
  model(fatores = ARIMA(inflacao_sem_institucionais ~ 1 + expectativa + importacao +
    hiato_trabalho + hiato_capital + pdq(0:2, 0, 0:2) + PDQ(0, 0, 0), stepwise = FALSE))
report(modelo_fatores_s2)

# 4. Regressão 2: parcela dos salários associada à inflação de serviços.
# Controles contemporâneos; é uma primeira especificação explicativa, sem seleção de lags.
# Ajuste institucional do core é um controle agregado, não um ajuste específico de serviços.
amostra_servicos <- dados_s2_trimestral |>
  select(date, inflacao_servicos, salario, expectativa, importacao,
         hiato_trabalho, hiato_capital, governo_institucional, peso_servicos_anterior,
         inflacao, contribuicao_servicos, contribuicao_bens) |>
  drop_na()
modelo_servicos_s2 <- amostra_servicos |>
  model(servicos = ARIMA(inflacao_servicos ~ 1 + salario + expectativa + importacao +
    hiato_trabalho + hiato_capital + governo_institucional +
    pdq(0:2, 0, 0:2) + PDQ(0, 0, 0), stepwise = FALSE))
report(modelo_servicos_s2)

# 5. Diagnósticos: d = 0 é a especificação inicial para as taxas, não uma prova de estacionariedade.
coeficientes_s2 <- bind_rows(tidy(modelo_fatores_s2), tidy(modelo_servicos_s2))
residuos_s2 <- bind_rows(as_tibble(augment(modelo_fatores_s2)),
                         as_tibble(augment(modelo_servicos_s2)))
diagnosticos_s2 <- residuos_s2 |>
  group_by(.model) |>
  group_modify(~ {
    ordem <- coeficientes_s2 |>
      filter(.model == .y$.model, str_detect(term, "^(ar|ma)[0-9]+$")) |>
      nrow()
    as_tibble_row(as.list(ljung_box(na.omit(.x$.innov), lag = 8, dof = ordem)))
  }) |>
  ungroup()
estacionariedade_s2 <- amostra_servicos |>
  select(date, inflacao_servicos, salario, expectativa, importacao,
         hiato_trabalho, hiato_capital) |>
  pivot_longer(-date, names_to = "variavel", values_to = "valor") |>
  as_tsibble(key = variavel, index = date) |>
  features(valor, unitroot_kpss)
print(diagnosticos_s2)
print(estacionariedade_s2)
if (any(diagnosticos_s2$lb_pvalue < .05))
  warning("Há autocorrelação residual: a especificação precisa ser revista antes de usar inferência.")
if (any(estacionariedade_s2$kpss_pvalue < .05))
  warning("KPSS rejeita estacionariedade de algum regressor. Tratar as parcelas como descritivas; investigar tendência/cointegração antes de inferência.")

# 6. Contribuições: coeficiente vezes variável, com intercepto e resto explícitos.
b <- tidy(modelo_fatores_s2) |> select(term, estimate) |> deframe()
decomposicao_fatores_s2 <- amostra_fatores |>
  as_tibble() |>
  transmute(date, inflacao, Expectativas = b[["expectativa"]] * expectativa,
            `Importações` = b[["importacao"]] * importacao,
            `Hiato de trabalho` = b[["hiato_trabalho"]] * hiato_trabalho,
            `Hiato de capital` = b[["hiato_capital"]] * hiato_capital,
            `Governo e instituições` = governo_institucional,
            Constante = b[["intercept"]]) |>
  mutate(`Dinâmica e não explicado` = inflacao - rowSums(pick(-date, -inflacao)))

beta_salario <- tidy(modelo_servicos_s2) |> filter(term == "salario") |> pull(estimate)
stopifnot(length(beta_salario) == 1L, is.finite(beta_salario))
decomposicao_setorial_s2 <- amostra_servicos |>
  as_tibble() |>
  transmute(date, inflacao, `Bens sem alimentos frescos` = contribuicao_bens,
            `Serviços: demais componentes` = contribuicao_servicos -
              peso_servicos_anterior * beta_salario * salario,
            `Serviços: salários estimados` = peso_servicos_anterior * beta_salario * salario,
            `Arredondamento e encadeamento` = inflacao - contribuicao_servicos - contribuicao_bens)
# Salários são uma parcela de serviços: não somar novamente à contribuição total de serviços.
# Pode ser negativa ou superar a contribuição líquida: não truncar o resultado para embelezar.
stopifnot(max(abs(rowSums(select(decomposicao_setorial_s2, -date, -inflacao)) -
                     decomposicao_setorial_s2$inflacao)) < 1e-8)

# 7. Gráficos: contribuições em p.p. para a inflação core em 12 meses (%).
grafico_fatores_s2 <- decomposicao_fatores_s2 |>
  mutate(date = as.Date(date)) |>
  pivot_longer(-c(date, inflacao), names_to = "componente", values_to = "contribuicao") |>
  ggplot(aes(date, contribuicao, fill = componente)) +
  geom_col(width = 65) + geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(aes(y = inflacao), colour = "black", linewidth = .7) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Japão | fatores associados à inflação core",
       subtitle = "Dados trimestrais | linha preta: inflação observada em 12 meses",
       x = NULL, y = "Contribuição (p.p.)", fill = NULL,
       caption = "Regressão explicativa com erros ARIMA; associações, não efeitos causais.\nGoverno e instituições: diferença entre CPI oficial e CPI ajustado pelo BoJ, com coeficiente fixado em 1.") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.margin = margin(15, 20, 15, 25)) +
  guides(fill = guide_legend(nrow = 3))

grafico_setorial_s2 <- decomposicao_setorial_s2 |>
  mutate(date = as.Date(date)) |>
  pivot_longer(-c(date, inflacao), names_to = "componente", values_to = "contribuicao") |>
  ggplot(aes(date, contribuicao, fill = componente)) +
  geom_col(width = 65, colour = "white", linewidth = .15) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(aes(y = inflacao), colour = "black", linewidth = .7) +
  scale_fill_manual(values = c("Bens sem alimentos frescos" = "#bc6c25",
    "Serviços: demais componentes" = "#a9cce3", "Serviços: salários estimados" = "#24567a",
    "Arredondamento e encadeamento" = "#c8c8c8")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Japão | inflação de bens e serviços",
       subtitle = "Salários: parcela estimada dos serviços em azul escuro | linha preta: CPI core",
       x = NULL, y = "Contribuição (p.p.)", fill = NULL,
       caption = "Bens excluem alimentos frescos. Contribuições reconstruídas com os pesos de cada base do MIC.\nSalários: coeficiente × crescimento salarial × peso dos serviços; não é causal. Especificação de serviços ainda com autocorrelação residual.") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

# Armazenamento da saída em outputs.
saida_s2 <- "src/Outputs/S2/explicativo trimestral"
dir.create(saida_s2, recursive = TRUE, showWarnings = FALSE)
saveRDS(list(fatores = modelo_fatores_s2, servicos = modelo_servicos_s2),
        file.path(saida_s2, "modelos.rds"))
saveRDS(dados_s2_trimestral, file.path(saida_s2, "dados_trimestrais.rds"))
write_csv(as_tibble(dados_s2_trimestral), file.path(saida_s2, "dados_trimestrais.csv"))
write_csv(coeficientes_s2, file.path(saida_s2, "coeficientes.csv"))
write_csv(diagnosticos_s2, file.path(saida_s2, "ljung_box.csv"))
write_csv(estacionariedade_s2, file.path(saida_s2, "kpss.csv"))
write_csv(decomposicao_fatores_s2, file.path(saida_s2, "contribuicoes_fatores.csv"))
write_csv(decomposicao_setorial_s2, file.path(saida_s2, "contribuicoes_bens_servicos.csv"))
write_csv(bind_rows(glance(modelo_fatores_s2), glance(modelo_servicos_s2)),
          file.path(saida_s2, "ajuste.csv"))
writeLines(c(
  "S2 explicativo trimestral: primeira especificação, sem forecast ou interpretação causal.",
  paste("Amostra dos fatores:", min(amostra_fatores$date), "a", max(amostra_fatores$date)),
  paste("Amostra de serviços:", min(amostra_servicos$date), "a", max(amostra_servicos$date)),
  "CPI core exclui alimentos frescos; não é o core-core.",
  "Taxas em 12 meses, agregadas pela média dos três meses completos.",
  "Governo/instituições: contribuição medida pela diferença MIC menos BoJ; coeficiente 1.",
  "Hiato de trabalho e de capital: decomposição publicada pelo BoJ.",
  "Salários são os regulares nominais da MHLW, não exclusivos do setor de serviços.",
  "Pesos de cada base do CPI; a parcela de salários não é somada novamente aos serviços.",
  "Consultar ljung_box.csv, kpss.csv e coeficientes.csv antes de interpretar as parcelas.",
  "Rejeição no KPSS exige investigar a especificação; Ljung-Box não valida causalidade.",
  paste("Consultas em cache extraídas em:", fontes_s2$extraido_em),
  "cpirev.xlsx, gap.xlsx e cache_salarios.rds são os arquivos locais do projeto."
), file.path(saida_s2, "LEIA-ME.txt"), useBytes = TRUE)
ggsave(file.path(saida_s2, "inflacao_fatores.png"), grafico_fatores_s2,
       width = 13, height = 7, dpi = 160, bg = "white")
ggsave(file.path(saida_s2, "inflacao_bens_servicos_salarios.png"), grafico_setorial_s2,
       width = 13, height = 7, dpi = 160, bg = "white")
ggsave(file.path(saida_s2, "residuos_fatores.png"), gg_tsresiduals(modelo_fatores_s2),
       width = 10, height = 7, dpi = 130, bg = "white")
ggsave(file.path(saida_s2, "residuos_servicos.png"), gg_tsresiduals(modelo_servicos_s2),
       width = 10, height = 7, dpi = 130, bg = "white")
if (interactive()) { print(grafico_fatores_s2); print(grafico_setorial_s2) }
