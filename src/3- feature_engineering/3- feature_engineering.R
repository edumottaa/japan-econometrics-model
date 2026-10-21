# Construção das variáveis e transformações

library(tidyverse)

data_modelo <- data |>
  transmute(
    # Taxas e diferenciais: manter em nível (% e pontos percentuais)
    ust2y      = ust2y,
    juro_curto = juro_curto,
    inclinacao = inclinacao_10_2,
    
    # Índices e preços: aplicar 100 * log(x)
    ip_mundial = 100 * log(ip_mundial),
    brent      = 100 * log(brent),
    vix        = 100 * log(vix),
    atividade  = 100 * log(atividade),
    cpi_core   = 100 * log(cpi_core),
    usdjpy     = 100 * log(usdjpy),
    topix      = 100 * log(topix)
  )

# 2.3. Definir a amostra comum dos modelos
# Identificar e mostrar os meses com NA em pelo menos uma das séries.
meses_removidos <- data_modelo |>
  filter(if_any(-date, is.na)) |>
  pull(date)
meses_removidos

# drop_na() exclui o mês inteiro se qualquer série tiver NA, reduzindo a amostra.
# Os meses excluídos são os listados em meses_removidos acima.
# Se houver exclusões no meio da série, surgem lacunas no calendário:
# o BVAR tratará as linhas restantes como meses consecutivos, e as 12
# defasagens passarão a representar 12 observações, não necessariamente 12 meses.
# Manter a coluna date para gráficos, alinhamento das previsões e avaliação.
data_bal <- data_modelo |>
  arrange(date) |>
  tidyr::drop_na()


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

