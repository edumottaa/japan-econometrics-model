# PIB em função de atividade, com erros ARIMA e diagnóstico dos resíduos.

library(fpp3)
library(tidyverse)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")

# 1. Usar a especificação do teste de diferenciação e lags, sem fixar p e q à mão.
dados_pib <- readRDS("data/processed/variaveis complementares/dados_pib.rds")
selecao_pontes_pib <- readRDS("data/processed/variaveis complementares/selecao_pontes_pib.rds")
modelo_pib <- selecao_pontes_pib$modelos$atividade
modelo_pib |> report()

# 2. Diagnóstico das INOVAÇÕES: série, ACF, histograma e gráfico quantil-quantil.
residuos_pib <- augment(modelo_pib) |> filter(is.finite(.innov))
grafico_residuos_pib <- modelo_pib |> gg_tsresiduals()
grafico_qq_pib <- residuos_pib |>
  ggplot(aes(sample = .innov)) + stat_qq() + stat_qq_line() +
  labs(title = "PIB | atividade | normalidade das inovações", x = "Quantis normais", y = "Inovações") +
  theme_minimal()

# 3. Ljung-Box com graus de liberdade dos termos AR e MA, em 8 e 12 trimestres.
# Não rejeitar H0 não prova independência, normalidade ou variância constante.
parametros_pib <- tidy(modelo_pib)
dof_pib <- sum(str_detect(parametros_pib$term, "^(ar|ma|sar|sma)[0-9]+$"))
diagnosticos_pib <- map_dfr(c(8L, 12L), function(k) {
  residuos_pib |> features(.innov, ljung_box, lag = k, dof = dof_pib) |>
    mutate(lag_teste = k, dof = dof_pib)
})
resumo_residuos_pib <- residuos_pib |> as_tibble() |>
  summarise(n = n(), media = mean(.innov), desvio_padrao = sd(.innov),
    shapiro_p = shapiro.test(.innov)$p.value)
variancia_residuos_pib <- residuos_pib |> mutate(quadrado = .innov^2) |>
  features(quadrado, ljung_box, lag = 8) |>
  mutate(interpretacao = "Autocorrelação dos quadrados: diagnóstico de heterocedasticidade, não prova de variância constante.")
print(diagnosticos_pib)
print(resumo_residuos_pib)
print(variancia_residuos_pib)
if (interactive()) print(grafico_residuos_pib)

# Armazenamento da saída em outputs.
saida_modelos_pib <- "src/Outputs/6- Projeções complementares/PIB/modelos/atividade"
dir.create(saida_modelos_pib, recursive = TRUE, showWarnings = FALSE)
saveRDS(modelo_pib, file.path(saida_modelos_pib, "modelos_pib.rds"))
write_csv(parametros_pib, file.path(saida_modelos_pib, "coeficientes.csv"))
write_csv(glance(modelo_pib), file.path(saida_modelos_pib, "ajuste.csv"))
write_csv(diagnosticos_pib, file.path(saida_modelos_pib, "ljung_box.csv"))
write_csv(resumo_residuos_pib, file.path(saida_modelos_pib, "resumo_residuos.csv"))
write_csv(variancia_residuos_pib, file.path(saida_modelos_pib, "variancia_residuos.csv"))
write_csv(as_tibble(residuos_pib), file.path(saida_modelos_pib, "residuos.csv"))
ggsave(file.path(saida_modelos_pib, "diagnostico_residuos.png"), grafico_residuos_pib,
  width = 11, height = 7, dpi = 160, bg = "white")
ggsave(file.path(saida_modelos_pib, "normalidade_residuos.png"), grafico_qq_pib,
  width = 7, height = 5, dpi = 160, bg = "white")

# Manter o arquivo usado pelo forecast existente da ponte simples.
saveRDS(modelo_pib, "src/Outputs/6- Projeções complementares/PIB/modelos/modelos_pib.rds")
