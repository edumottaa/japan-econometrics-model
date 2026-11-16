# Condicionamento suave do plano inicial: entropic tilting
# Repondera os sorteios do BVAR para a media do spread 30-5 bater com o satelite.
# Robertson, Tallman e Whiteman (2005); Kruger, Clark e Ravazzolo (2017)
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



# 2. Tilting: peso de cada sorteio proporcional a exp(lambda * spread)
# lambda e o menor "empurrao" que faz a media ponderada do spread igualar o alvo
tilting <- function(x, alvo) {
  x <- sweep(x, 2, alvo)                 # distancia de cada sorteio ao alvo
  escala <- apply(x, 2, sd)
  pesos <- function(l) {
    z <- drop(x %*% (l / escala))
    w <- exp(z - max(z))
    w / sum(w)
  }
  entropia <- function(l) {
    z <- drop(x %*% (l / escala))
    max(z) + log(mean(exp(z - max(z))))
  }
  lambda <- optim(rep(0, ncol(x)), entropia, function(l) drop(pesos(l) %*% x) / escala,
                  method = "BFGS", control = list(maxit = 1000, reltol = 1e-14))$par
  pesos(lambda)
}



# 3. Medias ponderadas por cenario, em niveis
resultados <- list()
diagnosticos <- list()
for (cen in names(trajetorias)) {
  F <- trajetorias[[cen]]
  dimnames(F) <- list(variaveis, NULL, NULL)
  alvo <- satelite |> filter(cenario == cen)
  spread <- t(F["inclinacao", , ])                 # sorteios x meses
  livres <- which(apply(spread, 2, sd) > 0)        # o primeiro mes ja e observado
  pesos <- list(`BVAR original` = rep(1 / ncol(F[1, , ]), ncol(F[1, , ])),
                `Tilting: todos os meses` = tilting(spread[, livres], alvo$alvo[livres]),
                `Tilting: so set/2028` = tilting(spread[, ncol(spread), drop = FALSE], tail(alvo$alvo, 1)))
  F[em_log, , ] <- exp(F[em_log, , ] / 100)
  for (m in names(pesos)) {
    medias <- apply(F, c(1, 2), function(x) sum(x * pesos[[m]]))
    resultados[[paste(cen, m)]] <- as_tibble(t(medias)) |>
      mutate(date = alvo$date, cenario = cen, modelo = m, .before = 1)
    diagnosticos[[paste(cen, m)]] <- tibble(cenario = cen, modelo = m,
      sorteios_efetivos = 1 / sum(pesos[[m]]^2),
      erro_alvo_pb = 100 * max(abs(medias["inclinacao", livres] - alvo$alvo[livres])))
  }
}



# 4. Salvar
forecast_tilting <- bind_rows(resultados) |>
  pivot_longer(all_of(variaveis), names_to = "variavel", values_to = "media")
diagnosticos <- bind_rows(diagnosticos)
write_csv(forecast_tilting |> mutate(date = as.Date(date)), file.path(saida, "forecast tilting.csv"))
write_csv(diagnosticos, file.path(saida, "diagnosticos tilting.csv"))
print(diagnosticos)
