args <- commandArgs(trailingOnly = TRUE)
source("C:/Users/dudup/OneDrive/japan-econometrics-model/src/evaluation/cv_covid_nativo/comum.R")
out <- args[1]; etapa <- as.integer(args[2])
atuais <- modelos |> filter(.data$etapa == .env$etapa)
comparar <- atuais$modelo
if (etapa == 2) comparar <- c(comparar, "foreign_p3_covid", "foreign_p6_covid")
if (etapa == 4) comparar <- c(comparar, "foreign_p3_covid", "foreign_p6_covid")
benchmarks <- readRDS(file.path(out, "benchmarks.rds"))
resultados <- detalhes <- list()
for (nome in comparar) {
  arquivos <- file.path(out, "windows", sprintf("%s_w%03d.rds", nome, 1:47))
  stopifnot(all(file.exists(arquivos)))
  for (arquivo in arquivos) {
    r <- readRDS(arquivo)
    if (is.null(r$erro)) resultados[[length(resultados) + 1L]] <- r$pontuacoes
    detalhes[[length(detalhes) + 1L]] <- tibble(.model = nome, .id = r$janela,
      erro = if (is.null(r$erro)) NA_character_ else r$erro,
      valido = if (is.null(r$erro)) isTRUE(r$diagnostico$valido) && (is.na(r$diagnostico$ess_hyper) || r$diagnostico$ess_hyper >= 50) else FALSE,
      ess_pesos = if (is.null(r$erro)) r$diagnostico$ess_pesos else NA_real_,
      ess_hyper = if (is.null(r$erro)) r$diagnostico$ess_hyper else NA_real_, segundos = r$segundos)
  }
}
diagnosticos <- bind_rows(detalhes)
sc <- bind_rows(benchmarks, bind_rows(resultados))
stopifnot(!anyDuplicated(sc[c(".model", ".id", "variavel", "h")]))
esperados <- benchmarks |> distinct(variavel, .model) |> count(variavel, name = "n_bench")
esperados$n_esperado <- esperados$n_bench + length(comparar)
# Nunca comparar um candidato parcial com benchmarks de amostra completa.
comuns <- sc |> left_join(esperados, by = "variavel") |>
  group_by(variavel, .id, h) |> filter(n_distinct(.model) == first(n_esperado)) |> ungroup()
stopifnot(nrow(comuns) > 0)
resumir <- function(x, horizonte) x |> group_by(variavel, .model) |>
  summarise(origens = n_distinct(.id), casos = n(),
    RMSE = fabletools::RMSE(observado - previsao), CRPS = mean(CRPS), .groups = "drop") |>
  mutate(horizonte = horizonte)
tabela <- bind_rows(
  resumir(filter(comuns, h <= 12), "ate_12"),
  resumir(comuns, "ate_24"),
  resumir(filter(comuns, h >= 12), "12_a_24"),
  resumir(filter(comuns, h == 12), "h12"),
  resumir(filter(comuns, h == 24), "h24"))
por_h <- comuns |> filter(h >= 12) |> group_by(variavel, .model, h) |>
  summarise(origens = n_distinct(.id), casos = n(), RMSE = fabletools::RMSE(observado - previsao),
    CRPS = mean(CRPS), .groups = "drop")
periodos <- comuns |> mutate(periodo_origem = if_else(origem >= tsibble::yearmonth("2020 Mar"), "desde_mar2020", "antes_mar2020")) |>
  filter(h >= 12) |> group_by(periodo_origem, variavel, .model) |>
  summarise(origens = n_distinct(.id), casos = n(), RMSE = fabletools::RMSE(observado - previsao),
    CRPS = mean(CRPS), .groups = "drop")
write_csv(periodos, file.path(out, "tabelas", paste0("etapa", etapa, "_periodos.csv")))
qualidade <- diagnosticos |> group_by(.model) |>
  summarise(janelas = n(), erros = sum(!is.na(erro)), alertas = sum(!valido),
    .groups = "drop")
tabela <- tabela |> left_join(qualidade, by = ".model") |> arrange(variavel, horizonte, RMSE)
write_csv(tabela, file.path(out, "tabelas", paste0("etapa", etapa, "_resumo.csv")))
write_csv(por_h, file.path(out, "tabelas", paste0("etapa", etapa, "_h12_a_h24.csv")))
write_csv(diagnosticos, file.path(out, "tabelas", paste0("etapa", etapa, "_diagnosticos.csv")))
write_csv(comuns, file.path(out, "tabelas", paste0("etapa", etapa, "_pontuacoes.csv")))
principal <- tabela |> filter(horizonte == "12_a_24", variavel %in% c("cpi_core", "atividade")) |>
  select(.model, variavel, RMSE, CRPS) |> pivot_wider(names_from = variavel, values_from = c(RMSE, CRPS))
texto <- c(paste0("# CV - etapa ", etapa), "",
  "Nove variaveis endogenas, sem bolsa. Previsoes mensais; treino inicial de 180 meses e reestimacao a cada tres meses.",
  "Mesma base congelada do CV anterior (dados finais, nao vintages em tempo real). Funcoes dos pacotes; bsvarSIGNs::forecast usa a versao local 3.0.0.1 com correcao de estado inicial e deslocamento de lags (software/bsvarSIGNs-3.0-forecast-fix/README.md). CRPS por fabletools::CRPS.", "",
  "## RMSE e CRPS: horizontes de 12 a 24 meses reunidos", "",
  "A tabela abaixo usa todos os pares origem/horizonte observados e comuns aos modelos apresentados. Isso nao e apenas h12 ou h24; os resultados de cada horizonte estao no CSV separado.", "",
  knitr::kable(principal, format = "pipe", digits = 3), "",
  "## Amostra comum utilizada", "",
  knitr::kable(tabela |> distinct(variavel, horizonte, origens, casos), format = "pipe"), "",
  "## Qualidade do calculo", "",
  knitr::kable(qualidade, format = "pipe"), "",
  "Alertas indicam baixa diversidade das simulacoes ou ESS abaixo de 50. Boas metricas com alertas nao validam a estimacao. Os detalhes identificam a janela e a causa.", "",
  "## Especificacoes", "",
  "bsvarSIGNs: foreign, mu=delta=1 e lambda=0.2 fixos; psi calibrado no treino e fixo. COVID nativa estimada quando ha pelo menos quatro meses observados desde marco/2020; nesta grade, desde a origem junho/2020. Na origem marco/2020 o perfil ainda nao e estimado. Versoes sem COVID permitem medir seu efeito.",
  "Na etapa 2, acrescentam-se pulsos exogenos em marco, abril e maio/2020, somente quando observados no treino; no futuro sao zero. Eles nao sao novas variaveis endogenas.",
  "BVAR condicionado: lambda/SOC/SUR estimados, psi fixo; 5000 iteracoes, burn-in 2500. Sem tratamento direto de COVID, conforme a ultima decisao do usuario. Caminhos externos previstos por VAR/AICc so no treino e tratados como fixos.",
  "Todos usam 2500 sorteios preditivos. CPI e atividade sao as series transformadas do projeto; CPI nao e aqui uma taxa de inflacao anual e atividade nao e PIB.", "",
  "Os CSVs de resumo e por horizonte tambem incluem juros, inclinacao e USDJPY. Benchmarks: AR, naive, drift e VAR domestico de tres variaveis (atividade, CPI e juros).")
writeLines(texto, file.path(out, paste0("etapa", etapa, ".md")), useBytes = TRUE)
status(out, paste0("etapa", etapa), estado = "calculada_revisar", modelos = atuais$modelo,
  arquivos = 47 * nrow(atuais), erros = sum(!is.na(diagnosticos$erro)), alertas = sum(!diagnosticos$valido))
print(principal, n = Inf); print(qualidade, n = Inf)