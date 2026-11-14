Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
library(fpp3)
library(tidyverse)
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/cenario oferta estrutural central'
tipo <- commandArgs(trailingOnly=TRUE)[1]
pasta <- file.path(trabalho,tipo)
dados_pib <- readRDS(file.path(raiz,'data/processed/variaveis complementares/dados_pib.rds'))
modelo_pib <- readRDS(file.path(raiz,'data/processed/variaveis complementares/selecao_pontes_pib.rds'))$modelos$atividade
base <- readRDS(file.path(raiz,'data/processed/forecast_pesos_exponenciais_20261026/base.rds'))
historico <- base |> as_tibble() |> transmute(date,atividade=exp(atividade/100))
atividade_prevista <- readRDS(file.path(pasta,'atividade niveis.rds'))
# 2. Prever o crescimento trimestral condicionado à trajetória média da atividade.
# generate() simula os erros ARIMA para construir níveis e taxas de quatro trimestres.
# Estes intervalos não incluem a incerteza da atividade prevista pelo BVAR.
previsoes_pib <- simulacoes_pib <- list()
for (cenario_atual in unique(atividade_prevista$cenario)) {
  futuro <- bind_rows(historico,atividade_prevista |> filter(cenario==cenario_atual) |>
    select(date,atividade)) |> arrange(date) |> as_tsibble(index=date) |>
    index_by(trimestre=yearquarter(date)) |> summarise(meses=n(),atividade=mean(atividade)) |>
    rename(date=trimestre) |> filter(meses==3L) |>
    mutate(atividade_crescimento=100*(atividade/lag(atividade)-1)) |>
    filter(date>max(dados_pib$date)) |> select(date,atividade_crescimento)
  stopifnot(min(futuro$date) == max(dados_pib$date) + 1L,
    !any(has_gaps(futuro)$.gaps), all(is.finite(futuro$atividade_crescimento)))
  previsoes_pib[[cenario_atual]] <- modelo_pib |> forecast(new_data = futuro) |>
    as_tibble() |> mutate(cenario = cenario_atual)
  set.seed(123)
  simulacoes_pib[[cenario_atual]] <- modelo_pib |>
    generate(new_data = futuro, times = 1000, bootstrap = FALSE) |>
    as_tibble() |>
    # Manter as simulações centradas na previsão analítica de forecast().
    # Preserva os desvios conjuntos dos sorteios e evita divergência de médias.
    left_join(previsoes_pib[[cenario_atual]] |> select(date, media_prevista = .mean), by = "date") |>
    group_by(.model, date) |> mutate(.sim = .sim - mean(.sim) + media_prevista) |> ungroup() |>
    group_by(.model, .rep) |> arrange(date, .by_group = TRUE) |>
    mutate(pib = last(dados_pib$pib) * cumprod(1 + .sim / 100),
      crescimento_trimestral = .sim, cenario = cenario_atual) |> ungroup()
}
previsoes_pib <- bind_rows(previsoes_pib)
simulacoes_pib <- bind_rows(simulacoes_pib)

# 3. Calcular a taxa de quatro trimestres e a média de cada ano completo.
historia_pib <- readRDS(file.path(raiz,'data/cache/cache_pib.rds'))
trajetorias_pib <- simulacoes_pib |> select(.model, cenario, .rep, date, pib) |>
  bind_rows(crossing(distinct(simulacoes_pib, .model, cenario, .rep), as_tibble(historia_pib))) |>
  group_by(.model, cenario, .rep) |> arrange(date, .by_group = TRUE) |>
  mutate(crescimento_anual = 100 * (pib / lag(pib, 4) - 1)) |> ungroup()
simulacoes_pib <- simulacoes_pib |>
  left_join(trajetorias_pib |> select(.model, cenario, .rep, date, crescimento_anual),
    by = c(".model", "cenario", ".rep", "date"))
resumo_pib <- simulacoes_pib |>
  select(.model, cenario, date, pib, crescimento_trimestral, crescimento_anual) |>
  pivot_longer(c(pib, crescimento_trimestral, crescimento_anual), names_to = "variavel", values_to = "valor") |>
  summarise(media = mean(valor), mediana = median(valor), inferior_95 = quantile(valor, .025),
    inferior_80 = quantile(valor, .10), superior_80 = quantile(valor, .90), superior_95 = quantile(valor, .975),
    .by = c(.model, cenario, date, variavel))
resumo_anual_pib <- trajetorias_pib |> mutate(ano = year(date)) |>
  summarise(pib = mean(pib), trimestres = n(), .by = c(.model, cenario, .rep, ano)) |>
  filter(trimestres == 4L) |>
  group_by(.model, cenario, .rep) |> arrange(ano, .by_group = TRUE) |>
  mutate(crescimento = 100 * (pib / lag(pib) - 1)) |> ungroup() |>
  filter(ano >= year(max(dados_pib$date))) |>
  summarise(media = mean(crescimento), inferior_80 = quantile(crescimento, .10),
    superior_80 = quantile(crescimento, .90), .by = c(.model, cenario, ano))
stopifnot(all(is.finite(simulacoes_pib$pib)), all(simulacoes_pib$pib > 0))

# Armazenamento da saída em outputs.
saida_pib <- file.path(pasta,'PIB')
dir.create(saida_pib, recursive = TRUE, showWarnings = FALSE)
write_csv(resumo_pib |> mutate(date = as.character(date)), file.path(saida_pib, "resumo_forecast_pib.csv"))
write_csv(resumo_anual_pib, file.path(saida_pib, "crescimento_por_ano.csv"))
saveRDS(list(previsoes = previsoes_pib, simulacoes = simulacoes_pib, resumo = resumo_pib,
  anual = resumo_anual_pib, historico = historia_pib, modelo = "PIB ~ atividade com erros ARIMA",
  origem_bvar = max(base$date), ultimo_pib_observado = max(dados_pib$date),
  fonte_bvar = file.path(pasta,'atividade niveis.rds')), file.path(saida_pib, "forecast_pib.rds"))
print(resumo_anual_pib, n = Inf)
