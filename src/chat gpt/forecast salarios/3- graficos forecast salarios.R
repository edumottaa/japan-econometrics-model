# Mesmos graficos do forecast original; salarios em variacao de 12 meses.
library(fpp3)
library(tidyverse)
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
somente_funcoes_graficos <- TRUE
source("src/7- forecast/comparacao hiato e priors/2- graficos forecast bvars.R", encoding = "UTF-8")
somente_funcoes_graficos <- FALSE
saida <- "src/Outputs/Forecast BVAR estrutural/salarios"
resultado <- readRDS(file.path(saida, "forecast_bvar.rds"))
titulos <- c(titulos, salario = "Salario nominal regular | 12 meses (%)")

# Calcular as variacoes em cada sorteio antes de resumir a distribuicao.
historia <- resultado$dados_modelo |> as_tibble() |>
  mutate(across(any_of(c("brent", "atividade", "cpi_core", "usdjpy")), ~ exp(.x / 100)))
resumos <- list()
for (alvo in unique(resultado$previsoes$variavel)) for (c in unique(resultado$previsoes$cenario)) {
  prev <- resultado$previsoes |> as_tibble() |> filter(variavel == alvo, cenario == c) |> arrange(date)
  x <- do.call(rbind, parameters(prev$valor)$x)
  if (alvo %in% c("atividade", "cpi_core")) {
    trajetorias <- rbind(matrix(tail(historia[[alvo]], 12), 12, ncol(x)), x)
    x <- 100 * (trajetorias[12 + seq_len(nrow(x)), , drop = FALSE] /
                  trajetorias[seq_len(nrow(x)), , drop = FALSE] - 1)
  }
  q <- apply(x, 1, quantile, probs = c(.025, .10, .50, .90, .975))
  resumos[[length(resumos) + 1L]] <- tibble(.model = prev$.model, cenario = c, variavel = alvo,
    date = prev$date, media = rowMeans(x), mediana = q[3, ],
    inferior_95 = q[1, ], inferior_80 = q[2, ], superior_80 = q[4, ], superior_95 = q[5, ])
}
historia <- historia |> mutate(across(c(atividade, cpi_core), ~ 100 * (.x / lag(.x, 12) - 1))) |>
  pivot_longer(-date, names_to = "variavel", values_to = "valor") |> filter(date >= yearmonth("2015 Jan"))
grafico <- list(historico = historia, resumo = bind_rows(resumos), origem = resultado$origem)
plotar_formato(grafico, saida)
print(grafico$resumo |> filter(cenario == "Base", variavel %in% c("cpi_core", "juro_curto", "salario"),
  date %in% yearmonth(c("2026 Dec", "2027 Dec", "2028 Aug"))) |>
  select(variavel, date, media), n = Inf)
