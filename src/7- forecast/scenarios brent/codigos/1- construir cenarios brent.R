# Construir os cenarios do Brent em US$/barril.

library(fpp3)
library(tidyverse)
library(readxl)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")

# 1. Base e periodo.
arquivo_base <- "data/Contratos futuros Brent.xlsx"
data_cotacao <- as.Date("2026-10-22")

# 2. Importar a base. Volatilidade em decimal e prazos conforme a planilha.
dados_brent <- read_excel(
  arquivo_base, sheet = "Worksheet", range = cell_cols("A:F"),
  col_types = c("text", "text", "text", "numeric", "numeric", "date")
) |>
  filter(str_detect(Period, "^[0-9]{2}/[0-9]{4}$")) |>
  transmute(date = yearmonth(my(Period)), futuro = `Fair Value`,
            volatilidade_50d = `Volatilidade 50D (% a.a.)`,
            vencimento_informado = as.Date(`Data de vencimento`)) |>
  filter(date >= yearmonth("2026 Dec"), date <= yearmonth("2028 Nov")) |>
  arrange(date) |>
  mutate(prazo_anos = as.numeric(vencimento_informado - data_cotacao) / 365,
         desvio_preco = futuro * volatilidade_50d * sqrt(prazo_anos)) |>
  as_tsibble(index = date)

stopifnot(nrow(dados_brent) == 24L, all(is.finite(dados_brent$desvio_preco)),
          all(dados_brent$futuro > 0), all(dados_brent$volatilidade_50d > 0),
          all(dados_brent$prazo_anos > 0))

# 3. Construção de cenários
#Cenario base: contratos futuros do Brent
#Cenário benigno P25 contratos futuros
#Cenário adverso: P75 contratos futuros
#Cenário estrutural: P95 contratos futuros
#Distribuição: Assumiu-se uma distribuição normal
nomes_cenarios <- c("Base", "Benigno", "Adverso", "Adverso estrutural")

cenarios_brent <- dados_brent |>
  mutate(Base = futuro,
         Benigno = qnorm(0.25, mean = futuro, sd = desvio_preco),
         Adverso = qnorm(0.75, mean = futuro, sd = desvio_preco),
         `Adverso estrutural` = qnorm(0.95, mean = futuro, sd = desvio_preco)) |>
  select(date, all_of(nomes_cenarios)) |>
  pivot_longer(-date, names_to = "cenario", values_to = "preco") |>
  mutate(cenario = factor(cenario, levels = nomes_cenarios)) |>
  as_tsibble(key = cenario, index = date)

# 4. Novembro/2026: US$ 111 nos quatro cenarios, conforme hipotese do usuario.
cenarios_brent <- bind_rows(
  as_tibble(cenarios_brent),
  tibble(date = yearmonth("2026 Nov"), cenario = nomes_cenarios, preco = 111)
) |>
  mutate(cenario = factor(cenario, levels = nomes_cenarios)) |>
  arrange(cenario, date) |>
  as_tsibble(key = cenario, index = date)
