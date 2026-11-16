# Condicionamento rigido do plano inicial (Waggoner e Zha, 1999)
# O spread 30-5 e fixado na trajetoria do satelite; as outras variaveis se ajustam
# pela covariancia dos sorteios do BVAR: Y* = Y + K (alvo - spread), K = Cov(Y, spread) Cov(spread)^-1
.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))

library(fpp3)
library(tidyverse)

raiz <- "C:/Users/dudup/OneDrive/japan-econometrics-model"
saida <- file.path(raiz, "src/chat gpt/condicionamento do plano inicial/resultados")
variaveis <- c("ust2y", "brent", "ip_mundial", "atividade", "cpi_core", "juro_curto", "inclinacao", "usdjpy")
em_log <- c("brent", "ip_mundial", "atividade", "cpi_core", "usdjpy")



# 1. Sorteios do BVAR de producao (8 variaveis x 25 meses x 2500) e alvo do satelite
trajetorias <- readRDS(file.path(raiz, "src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao/trajetorias modelos.rds"))
satelite <- read_csv(file.path(raiz, "src/Outputs/Satelite fluxo suave/resultados/opcao_3/forecast/premissas.csv"),
                     show_col_types = FALSE) |>
  transmute(date = yearmonth(date), cenario, alvo = alvo_spread_pb / 100) |>
  arrange(cenario, date)



# 2. Condicionar cada sorteio na trajetoria do satelite
resultados <- list()
banda <- list()
for (cen in names(trajetorias)) {
  F <- trajetorias[[cen]]
  dimnames(F) <- list(variaveis, NULL, NULL)
  h <- dim(F)[2]
  S <- dim(F)[3]
  alvo <- satelite |> filter(cenario == cen)
  Y <- t(matrix(F, ncol = S))                      # sorteios x (8 variaveis empilhadas por mes)
  spread <- t(F["inclinacao", , ])
  livres <- which(apply(spread, 2, sd) > 0)        # o primeiro mes ja e observado
  K <- cov(Y, spread[, livres]) %*% solve(cov(spread[, livres]))
  distancia <- matrix(alvo$alvo[livres], S, length(livres), byrow = TRUE) - spread[, livres]
  F <- array(t(Y + distancia %*% t(K)), dim(F), dimnames(F))
  banda[[cen]] <- tibble(cenario = cen, juro_terminal_min = min(F["juro_curto", h, ]),
                         juro_terminal_max = max(F["juro_curto", h, ]))
  F[em_log, , ] <- exp(F[em_log, , ] / 100)
  resultados[[cen]] <- as_tibble(t(apply(F, c(1, 2), mean))) |>
    mutate(date = alvo$date, cenario = cen, modelo = "Rigido", .before = 1)
}



# 3. Salvar
forecast_rigido <- bind_rows(resultados) |>
  pivot_longer(all_of(variaveis), names_to = "variavel", values_to = "media")
write_csv(forecast_rigido |> mutate(date = as.Date(date)), file.path(saida, "forecast rigido.csv"))
print(bind_rows(banda))
