library(tidyverse)

# 2. Transformações do S2: mesmo critério do núcleo A.
# Índices e preços em 100 * log(x); inflação já calculada, expectativas, hiato
# e contribuições institucionais permanecem em nível (% ou pontos percentuais).

data_modelo_s2_mensal <- data_s2_mensal |>
  mutate(
    cpi_core = 100 * log(cpi_core),
    cpi_servicos = 100 * log(cpi_servicos),
    preco_importacao_iene = 100 * log(preco_importacao_iene)
  )

data_modelo_s2_trimestral <- data_s2_trimestral |>
  mutate(
    cpi_core = 100 * log(cpi_core),
    cpi_servicos = 100 * log(cpi_servicos),
    preco_importacao_iene = 100 * log(preco_importacao_iene)
  )

