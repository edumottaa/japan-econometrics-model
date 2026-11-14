# Diferenciação, ordem ARIMA e defasagens da ponte com atividade do PIB.
# Referência de organização: código EUA 1.0; amostra comum para comparar AICc.

library(fpp3)
library(tidyverse)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
source("src/5- models/variaveis complementares/0- funcoes ponte PIB.R", encoding = "UTF-8")
dados_pib <- readRDS("data/processed/variaveis complementares/dados_pib.rds")

# 1. Níveis e crescimentos: KPSS, ADF e necessidade de diferenças.
testes_series_pib <- testar_series_pib(dados_pib)
print(testes_series_pib, width = Inf)

# 2. Defasagens dos preditores e ordens dos erros ARIMA selecionadas juntas.
selecao_pontes_pib <- ajustar_pontes_pib(dados_pib)
print(selecao_pontes_pib$tabela, n = Inf)
print(selecao_pontes_pib$escolhidos, width = Inf)

# 3. Diferenças sazonais não são impostas a séries já ajustadas sazonalmente.
# Os testes são diagnósticos: rejeição/não rejeição não prova estacionariedade.
grafico_lags_pib <- selecao_pontes_pib$tabela |>
  ggplot(aes(delta_AICc, reorder(especificacao, -delta_AICc))) +
  geom_col(fill = "#355C7D") + geom_vline(xintercept = 2, linetype = "dashed") +
  facet_wrap(~familia, scales = "free_y") +
  labs(title = "Ponte para o PIB | teste de defasagens", x = "AICc acima do mínimo da família",
    y = NULL, caption = "Mesma amostra em todos os candidatos. Menor é melhor.") + theme_minimal()

# Armazenamento da saída em outputs.
saida_testes_pib <- "src/Outputs/2- Evaluation/variaveis complementares/PIB/testes"
dir.create(saida_testes_pib, recursive = TRUE, showWarnings = FALSE)
write_csv(testes_series_pib, file.path(saida_testes_pib, "estacionariedade.csv"))
write_csv(selecao_pontes_pib$tabela, file.path(saida_testes_pib, "teste_lags.csv"))
write_csv(selecao_pontes_pib$escolhidos, file.path(saida_testes_pib, "especificacoes_escolhidas.csv"))
ggsave(file.path(saida_testes_pib, "teste_lags.png"), grafico_lags_pib,
  width = 12, height = 7, dpi = 160, bg = "white")
saveRDS(selecao_pontes_pib, "data/processed/variaveis complementares/selecao_pontes_pib.rds")
