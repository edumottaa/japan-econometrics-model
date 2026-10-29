# CV do PIB ~ atividade com erros ARIMA, usando as origens do CV reduzido com peso 0,995.
# Reutiliza as previsões salvas; não reestima o BVAR.
# Teste condicional ex post aos caminhos externos, com a versão atual do PIB

library(fpp3)
library(tidyverse)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
source("src/7- forecast/variaveis complementares/codigos/0- funcoes projecao PIB.R", encoding = "UTF-8")

# 1. Manter a mesma base histórica e as mesmas origens trimestrais do CV do BVAR
dados_mensais_cv_pib <- readRDS("data/processed/cv_estrutural_mensal_20261019/data_frozen.rds") |>
  as_tibble() |> transmute(date, atividade = exp(atividade / 100)) |> as_tsibble(index = date)
atividade_cv_pib <- dados_mensais_cv_pib |>
  index_by(trimestre = yearquarter(date)) |>
  summarise(meses = n(), atividade = mean(atividade)) |>
  rename(date = trimestre) |> filter(meses == 3L) |>
  mutate(atividade_crescimento = 100 * (atividade / lag(atividade) - 1))
dados_pib_cv <- readRDS("data/cache/cache_pib.rds") |>
  mutate(pib_crescimento = 100 * (pib / lag(pib) - 1)) |>
  left_join(atividade_cv_pib |> select(date, atividade_crescimento), by = "date") |>
  drop_na(pib_crescimento, atividade_crescimento)

# Armazenamento da saída em outputs
saida_cv_pib <- "src/Outputs/2- Evaluation/variaveis complementares/PIB"
dir.create(saida_cv_pib, recursive = TRUE, showWarnings = FALSE)

# 2. Treinar até o trimestre anterior à origem, pois o PIB tem atraso de divulgação.
# Os meses são tratados como disponíveis na origem, como no CV original do BVAR
arquivos_cv_pib <- sort(list.files("data/processed/cv_reduzido_pesos_expost_20261027/checkpoints",
  pattern = "^janela_[0-9]+[.]rds$", full.names = TRUE))
stopifnot(length(arquivos_cv_pib) > 0L)
resultados_cv_pib <- diagnosticos_cv_pib <- list()
for (i in seq_along(arquivos_cv_pib)) {
  janela <- readRDS(arquivos_cv_pib[i])
  origem <- yearquarter(janela$origem)
  if (origem >= max(dados_pib_cv$date)) next
  treino <- dados_pib_cv |> filter(date <= origem - 1L)
  ajuste <- treino |>
    model(arima = ARIMA(pib_crescimento ~ atividade_crescimento + pdq(d = 0) + PDQ(0, 0, 0)))
  futuro <- preparar_atividade_pib(dados_mensais_cv_pib |> filter(date <= janela$origem),
    janela$previsoes, ultimo_pib = max(treino$date), log_bvar = TRUE) |>
    filter(date <= max(dados_pib_cv$date))
  stopifnot(min(futuro$date) == max(treino$date) + 1L,
    !any(has_gaps(futuro)$.gaps), all(is.finite(futuro$atividade_crescimento)))
  resultados_cv_pib[[i]] <- ajuste |> forecast(new_data = futuro) |>
    as_tibble() |> filter(date > origem) |>
    mutate(.id = janela$indice, origem = origem, h = as.integer(date - origem),
      periodo = case_when(year(date) <= 2019 ~ "2015-2019", year(date) <= 2021 ~ "2020-2021",
        TRUE ~ "2022 em diante"))
  diagnosticos_cv_pib[[i]] <- tibble(.id = janela$indice, origem = as.character(janela$origem),
    ultimo_pib_treino = as.character(max(treino$date)),
    horizontes_avaliados = n_distinct(resultados_cv_pib[[i]]$date))
  message("CV PIB: janela ", janela$indice, " | origem ", janela$origem)
}

# 3. Métricas gerais, por período e por horizonte (até oito trimestres).
previsoes_cv_pib <- bind_rows(resultados_cv_pib) |>
  as_tsibble(key = c(.id, .model), index = date) |>
  as_fable(response = "pib_crescimento", distribution = "pib_crescimento")
observados_pib <- dados_pib_cv |> select(date, pib_crescimento)
medidas_pib <- list(RMSE = RMSE, MAE = MAE, CRPS = CRPS)
cv_pib_geral <- previsoes_cv_pib |>
  accuracy(observados_pib, by = ".model", measures = medidas_pib)
cv_pib_periodos <- previsoes_cv_pib |>
  accuracy(observados_pib, by = c("periodo", ".model"), measures = medidas_pib)
cv_pib_horizontes <- previsoes_cv_pib |>
  accuracy(observados_pib, by = c("h", ".model"), measures = medidas_pib)
print(cv_pib_geral)
print(cv_pib_periodos)

# 4. Mesmo eixo x para comparar os períodos.
grafico_cv_pib <- cv_pib_periodos |>
  pivot_longer(c(RMSE, CRPS), names_to = "metrica", values_to = "erro") |>
  mutate(periodo = factor(periodo, levels = c("2015-2019", "2020-2021", "2022 em diante"))) |>
  ggplot(aes(erro, periodo)) + geom_col(fill = "#355C7D") +
  facet_wrap(~metrica, scales = "free_x") +
  labs(title = "PIB ~ atividade com erros ARIMA | validação temporal",
    x = "Erro em pontos percentuais", y = NULL) + theme_minimal()
print(grafico_cv_pib)
ggsave(file.path(saida_cv_pib, "cv_pib_por_periodo.png"), grafico_cv_pib,
  width = 11, height = 6, dpi = 180, bg = "white")
write_csv(cv_pib_geral, file.path(saida_cv_pib, "metricas_gerais.csv"))
write_csv(cv_pib_periodos, file.path(saida_cv_pib, "metricas_por_periodo.csv"))
write_csv(cv_pib_horizontes, file.path(saida_cv_pib, "metricas_por_horizonte.csv"))
write_csv(bind_rows(diagnosticos_cv_pib), file.path(saida_cv_pib, "diagnosticos.csv"))
write_csv(previsoes_cv_pib |> as_tibble() |> count(periodo, .model, name = "casos"),
  file.path(saida_cv_pib, "casos_por_periodo.csv"))
saveRDS(list(previsoes = previsoes_cv_pib, geral = cv_pib_geral, periodos = cv_pib_periodos,
  horizontes = cv_pib_horizontes, diagnosticos = bind_rows(diagnosticos_cv_pib)),
  file.path(saida_cv_pib, "resultados_cv_pib.rds"))
