# Modelo satelite do spread JGB 30Y-5Y (pb/mes)
# Delta spread = c + beta*Delta OIS2Y pos-YCC + gamma*fluxo estrangeiro(t-2) + erro branco
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- file.path(saida, 'modelo satelite')
dir.create(trabalho, recursive = TRUE, showWarnings = FALSE)
POS_YCC <- '2024 Apr'                      # primeiro mes inteiro depois do YCC
origem <- max(historico_satelite$date)

# 1. Regressao com erro branco, ate a origem do BVAR
treino <- preparar_variaveis_satelite(historico_satelite, aux_satelite, origem) |> as_tsibble(index = date)
stopifnot(!has_gaps(treino)$.gaps)
modelo_satelite <- treino |> model(satelite = ARIMA(d_spread ~ 1 + ois_pos + fluxo_l2 + pdq(0,0,0) + PDQ(0,0,0)))
stopifnot(!any(vapply(modelo_satelite$satelite, inherits, logical(1), 'null_mdl')))
report(modelo_satelite)

# 2. Diagnostico dos resíduos
residuos <- augment(modelo_satelite) |> filter(is.finite(.innov))
diagnostico <- map_dfr(c(12L, 24L), function(k) residuos |>
  features(.innov, ljung_box, lag = k) |> mutate(lag_teste = k))
print(diagnostico)

# Armazenamento da saida em outputs.
write_csv(tidy(modelo_satelite), file.path(trabalho, 'coeficientes satelite.csv'))
write_csv(glance(modelo_satelite), file.path(trabalho, 'ajuste satelite.csv'))
write_csv(diagnostico, file.path(trabalho, 'diagnostico satelite.csv'))
ggsave(file.path(trabalho, 'residuos satelite.png'), gg_tsresiduals(modelo_satelite),
  width = 11, height = 7, dpi = 160, bg = 'white')
