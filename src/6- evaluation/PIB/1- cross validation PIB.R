# CV da ponte com atividade do PIB e benchmarks; mesmas origens trimestrais do BVAR.
# Reutiliza previsões do BVAR reduzido com rho = 0,995: não reestima o BVAR.
# Pseudo fora da amostra com dado revisado e caminhos externos ex post do BVAR.

library(fpp3)
library(tidyverse)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
source("src/5- models/variaveis complementares/0- funcoes ponte PIB.R", encoding = "UTF-8")

# 1. Base congelada igual à do CV original; agregar em nível ANTES de crescer.
dados_mensais_cv_pib <- readRDS("data/processed/cv_estrutural_mensal_20261019/data_frozen.rds") |>
  as_tibble() |> transmute(date, atividade = exp(atividade / 100)) |>
  as_tsibble(index = date)
atividade_cv_pib <- dados_mensais_cv_pib |>
  index_by(trimestre = yearquarter(date)) |>
  summarise(meses = n(), atividade = mean(atividade)) |>
  rename(date = trimestre) |> filter(meses == 3L) |>
  mutate(atividade_crescimento = 100 * (atividade / lag(atividade) - 1))
dados_pib_cv <- readRDS("data/cache/cache_pib.rds") |>
  mutate(pib_crescimento = 100 * (pib / lag(pib) - 1)) |>
  left_join(atividade_cv_pib |> select(-meses), by = "date") |>
  drop_na(pib_crescimento, atividade_crescimento)

# Armazenamento da saída em outputs.
saida_cv_pib <- "src/Outputs/2- Evaluation/variaveis complementares/PIB/comparacao_pontes"
dir.create(file.path(saida_cv_pib, "checkpoints"), recursive = TRUE, showWarnings = FALSE)

# 2. Treino até o trimestre ANTERIOR à origem, preservando a convenção original.
# Selecionar diferenciação, lags e ARIMA dentro de CADA treino, sem informação futura.
arquivos_cv_pib <- sort(list.files("data/processed/cv_reduzido_pesos_expost_20261027/checkpoints",
  pattern = "^janela_[0-9]+[.]rds$", full.names = TRUE))
stopifnot(length(arquivos_cv_pib) == 47L)
# Piloto opcional, por variável de ambiente; não altera a configuração padrão.
limite_janelas <- as.integer(Sys.getenv("PIB_CV_JANELAS", "47"))
resultados_cv_pib <- escolhas_cv_pib <- diagnosticos_cv_pib <- grades_cv_pib <- list()
for (i in seq_len(min(limite_janelas, length(arquivos_cv_pib)))) {
  inicio <- proc.time()[[3]]
  janela <- readRDS(arquivos_cv_pib[i])
  origem <- yearquarter(janela$origem)
  treino <- dados_pib_cv |> filter(date <= origem - 1L)
  futuro <- preparar_futuro_pontes(
    dados_mensais_cv_pib |> filter(date <= janela$origem), janela$previsoes,
    ultimo_pib = max(treino$date)) |>
    filter(date <= max(dados_pib_cv$date))
  # Últimas janelas com apenas 1-2 meses previstos não fecham um trimestre avaliado.
  if (nrow(futuro) == 0L || !any(futuro$date > origem)) next
  stopifnot(min(futuro$date) == max(treino$date) + 1L, !any(has_gaps(futuro)$.gaps))
  selecao <- ajustar_pontes_pib(treino)
  futuro <- preparar_futuro_pontes(
    dados_mensais_cv_pib |> filter(date <= janela$origem), janela$previsoes,
    ultimo_pib = max(treino$date)) |>
    filter(date <= max(dados_pib_cv$date))
  stopifnot(all(is.finite(as.matrix(as_tibble(futuro) |> select(-date)))))

  pontes <- forecast(selecao$modelos$atividade, new_data = futuro) |>
    as_tibble() |> mutate(.model = "Atividade + ARIMA")
  # Benchmarks usam exatamente os mesmos trimestres de treino e previsão.
  # Naive/Drift são aplicados ao CRESCIMENTO trimestral, não ao nível do PIB.
  treino_bench <- selecao$preparo$dados
  benchmarks <- treino_bench |> model(
    arima = ARIMA(pib_crescimento ~ pdq(p = 0:2, d = 0:1, q = 0:2) + PDQ(0, 0, 0),
      stepwise = FALSE, approximation = FALSE),
    naive = NAIVE(pib_crescimento),
    drift = RW(pib_crescimento ~ drift())) |>
    forecast(new_data = futuro |> select(date)) |> as_tibble() |>
    mutate(.model = recode(.model, arima = "ARIMA sem preditores", naive = "Naive", drift = "Drift"))
  resultados_cv_pib[[i]] <- bind_rows(pontes, benchmarks) |>
    filter(date > origem) |>
    mutate(.id = janela$indice, origem = origem, h = as.integer(date - origem),
      periodo = case_when(year(date) <= 2019 ~ "2015-2019", year(date) <= 2021 ~ "2020-2021",
        TRUE ~ "2022 em diante"))
  escolhas_cv_pib[[i]] <- selecao$escolhidos |> mutate(.id = janela$indice, origem = as.character(origem))
  grades_cv_pib[[i]] <- selecao$tabela |> mutate(.id = janela$indice, origem = as.character(origem))
  diagnosticos_cv_pib[[i]] <- tibble(.id = janela$indice, origem = as.character(janela$origem),
    ultimo_pib_treino = as.character(max(treino$date)),
    horizontes_avaliados = n_distinct(resultados_cv_pib[[i]]$date),
    segundos = proc.time()[[3]] - inicio)
  saveRDS(list(previsoes = resultados_cv_pib[[i]], escolhas = escolhas_cv_pib[[i]],
    grade = grades_cv_pib[[i]], diagnostico = diagnosticos_cv_pib[[i]]),
    file.path(saida_cv_pib, "checkpoints", sprintf("janela_%03d.rds", janela$indice)))
  message("CV PIB: ", i, "/", length(arquivos_cv_pib), " | ", janela$origem,
    " | ", round(proc.time()[[3]] - inicio, 1), " segundos")
}

# 3. Métricas gerais, por período-alvo e por horizonte, até oito trimestres.
previsoes_cv_pib <- bind_rows(resultados_cv_pib) |>
  as_tsibble(key = c(.id, .model), index = date) |>
  as_fable(response = "pib_crescimento", distribution = "pib_crescimento")
observados_pib <- dados_pib_cv |> select(date, pib_crescimento)
stopifnot(all(is.finite(mean(previsoes_cv_pib$pib_crescimento))),
  all(is.finite(distributional::variance(previsoes_cv_pib$pib_crescimento))))
casos_cv <- previsoes_cv_pib |> as_tibble() |> count(.id, date)
stopifnot(all(casos_cv$n == 4L), !anyDuplicated(as_tibble(previsoes_cv_pib)[c(".id", ".model", "date")]))
medidas_pib <- list(RMSE = RMSE, MAE = MAE, CRPS = CRPS, Winkler_80 = winkler_score)
cv_pib_geral <- previsoes_cv_pib |>
  accuracy(observados_pib, by = ".model", measures = medidas_pib, level = 80)
cv_pib_periodos <- previsoes_cv_pib |>
  accuracy(observados_pib, by = c("periodo", ".model"), measures = medidas_pib, level = 80)
cv_pib_horizontes <- previsoes_cv_pib |>
  accuracy(observados_pib, by = c("h", ".model"), measures = medidas_pib, level = 80)
print(cv_pib_geral, width = Inf)
print(cv_pib_periodos, n = Inf, width = Inf)

# 4. Gráficos e tabelas ficam em outro script, na mesma classificação evaluation.
write_csv(cv_pib_geral, file.path(saida_cv_pib, "metricas_gerais.csv"))
write_csv(cv_pib_periodos, file.path(saida_cv_pib, "metricas_por_periodo.csv"))
write_csv(cv_pib_horizontes, file.path(saida_cv_pib, "metricas_por_horizonte.csv"))
write_csv(bind_rows(escolhas_cv_pib), file.path(saida_cv_pib, "escolhas_por_janela.csv"))
write_csv(bind_rows(grades_cv_pib), file.path(saida_cv_pib, "teste_lags_por_janela.csv"))
write_csv(bind_rows(diagnosticos_cv_pib), file.path(saida_cv_pib, "diagnosticos.csv"))
write_csv(previsoes_cv_pib |> as_tibble() |> count(periodo, .model, name = "casos"),
  file.path(saida_cv_pib, "casos_por_periodo.csv"))
write_csv(previsoes_cv_pib |> as_tibble() |> select(-pib_crescimento) |>
  left_join(as_tibble(observados_pib), by = "date"), file.path(saida_cv_pib, "previsoes_pontuais.csv"))
saveRDS(list(previsoes = previsoes_cv_pib, geral = cv_pib_geral, periodos = cv_pib_periodos,
  horizontes = cv_pib_horizontes, escolhas = bind_rows(escolhas_cv_pib),
  diagnosticos = bind_rows(diagnosticos_cv_pib)), file.path(saida_cv_pib, "resultados_cv_pib.rds"))
