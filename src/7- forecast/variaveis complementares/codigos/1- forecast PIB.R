# PIB nos quatro cenários: atividade do BVAR como preditora, com erros ARIMA.

library(fpp3)
library(tidyverse)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
source("src/5- models/variaveis complementares/1- modelos PIB.R", encoding = "UTF-8")
source("src/7- forecast/variaveis complementares/codigos/0- funcoes projecao PIB.R", encoding = "UTF-8")

# 1. Reutilizar a atividade prevista pelo BVAR, sem reestimar o BVAR.
arquivo_forecast_bvar <- "src/Outputs/Forecast BVAR estrutural/pesos exponenciais/forecast_bvar.rds"
resultado_bvar <- readRDS(arquivo_forecast_bvar)
stopifnot(isTRUE(all.equal(resultado_bvar$pesos_exponenciais$rho, .995)))
historico <- resultado_bvar$dados_modelo |> as_tibble() |>
  transmute(date, atividade = exp(atividade / 100))

# 2. Prever o crescimento trimestral condicionado à trajetória média da atividade.
# generate() simula os erros ARIMA para construir níveis e taxas de quatro trimestres.
# Estes intervalos não incluem a incerteza da atividade prevista pelo BVAR.
previsoes_pib <- simulacoes_pib <- list()
for (cenario_atual in unique(resultado_bvar$previsoes$cenario)) {
  futuro <- preparar_atividade_pib(historico,
    resultado_bvar$previsoes |> as_tibble() |> filter(cenario == cenario_atual),
    ultimo_pib = max(dados_pib$date))
  stopifnot(min(futuro$date) == max(dados_pib$date) + 1L,
    !any(has_gaps(futuro)$.gaps), all(is.finite(futuro$atividade_crescimento)))
  previsoes_pib[[cenario_atual]] <- modelo_pib |> forecast(new_data = futuro) |>
    as_tibble() |> mutate(cenario = cenario_atual)
  set.seed(123)
  simulacoes_pib[[cenario_atual]] <- modelo_pib |>
    generate(new_data = futuro, times = 1000, bootstrap = FALSE) |>
    as_tibble() |> group_by(.model, .rep) |> arrange(date, .by_group = TRUE) |>
    mutate(pib = last(dados_pib$pib) * cumprod(1 + .sim / 100),
      crescimento_trimestral = .sim, cenario = cenario_atual) |> ungroup()
}
previsoes_pib <- bind_rows(previsoes_pib)
simulacoes_pib <- bind_rows(simulacoes_pib)

# 3. Calcular a taxa de quatro trimestres e a média de cada ano completo.
historia_pib <- readRDS("data/cache/cache_pib.rds")
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
saida_pib <- "src/Outputs/6- Projeções complementares/PIB/forecast"
dir.create(saida_pib, recursive = TRUE, showWarnings = FALSE)
write_csv(resumo_pib |> mutate(date = as.character(date)), file.path(saida_pib, "resumo_forecast_pib.csv"))
write_csv(resumo_anual_pib, file.path(saida_pib, "crescimento_por_ano.csv"))
saveRDS(list(previsoes = previsoes_pib, simulacoes = simulacoes_pib, resumo = resumo_pib,
  anual = resumo_anual_pib, historico = historia_pib, modelo = "PIB ~ atividade com erros ARIMA",
  origem_bvar = resultado_bvar$origem, ultimo_pib_observado = max(dados_pib$date),
  fonte_bvar = arquivo_forecast_bvar), file.path(saida_pib, "forecast_pib.rds"))
print(resumo_anual_pib, n = Inf)
