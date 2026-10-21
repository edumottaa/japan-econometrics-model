# Selecao de lags do petroleo para IP mundial e VIX, como no codigo dos EUA.
# Executar com data_bal preparado; nenhuma estimacao do BVAR e feita aqui.

library(tidyverse)
library(fpp3)

h <- 24
K <- 4

# 1. Reservar o mesmo holdout; todas as escolhas abaixo usam apenas o treino.
dados_lags <- data_bal |>
  select(date, brent, ip_mundial, vix) |>
  arrange(date)
stopifnot(!any(tsibble::has_gaps(dados_lags)$.gaps),
          nrow(dados_lags) > h + K + 24,
          all(is.finite(as.matrix(select(as_tibble(dados_lags), -date)))))

treino_lags <- dados_lags |> slice_head(n = nrow(dados_lags) - h)
amostra_lags <- tibble(inicio = as.character(min(treino_lags$date)),
                       fim_treino = as.character(max(treino_lags$date)),
                       fim_base = as.character(max(dados_lags$date)),
                       meses_treino = nrow(treino_lags),
                       meses_comuns = nrow(treino_lags) - K, meses_teste = h)
print(amostra_lags)

# Mesma amostra em todos os candidatos: retirar os quatro primeiros meses.
treino_lags <- treino_lags |>
  mutate(brent_1 = lag(brent, 1), brent_2 = lag(brent, 2),
         brent_3 = lag(brent, 3), brent_4 = lag(brent, 4)) |>
  drop_na() |>
  pivot_longer(c(ip_mundial, vix), names_to = "variavel", values_to = "valor") |>
  as_tsibble(key = variavel, index = date)

# 2. ARIMA escolhe d e D no treino; fixar ambos para comparar os AICc.
# p, q, P e Q continuam sendo selecionados automaticamente em cada candidato.
ajustes_lags <- ajustes_escolhidos <- tabelas_lags <- list()
for (alvo in c("ip_mundial", "vix")) {
  dados <- treino_lags |> filter(variavel == alvo)
  fit_d <- dados |>
    model(base = ARIMA(valor ~ brent + brent_1 + brent_2 + brent_3 + brent_4 +
                        pdq(p = 0, q = 0) + PDQ(P = 0, Q = 0), ic = "aicc"))
  d_externo <- fit_d$base[[1]]$fit$spec$d
  D_externo <- fit_d$base[[1]]$fit$spec$D
  stopifnot(length(d_externo) == 1, length(D_externo) == 1)

  ajustes <- dados |>
    model(
      # Familia A: contemporaneo e lags acumulados.
      A_lag0 = ARIMA(valor ~ brent + pdq(d = d_externo) + PDQ(D = D_externo), ic = "aicc", approximation = FALSE),
      A_lag1 = ARIMA(valor ~ brent + brent_1 + pdq(d = d_externo) + PDQ(D = D_externo), ic = "aicc", approximation = FALSE),
      A_lag2 = ARIMA(valor ~ brent + brent_1 + brent_2 + pdq(d = d_externo) + PDQ(D = D_externo), ic = "aicc", approximation = FALSE),
      A_lag3 = ARIMA(valor ~ brent + brent_1 + brent_2 + brent_3 + pdq(d = d_externo) + PDQ(D = D_externo), ic = "aicc", approximation = FALSE),
      A_lag4 = ARIMA(valor ~ brent + brent_1 + brent_2 + brent_3 + brent_4 + pdq(d = d_externo) + PDQ(D = D_externo), ic = "aicc", approximation = FALSE),
      # Familia B: somente lags a partir de dois meses, como no trabalho dos EUA.
      B_2 = ARIMA(valor ~ brent_2 + pdq(d = d_externo) + PDQ(D = D_externo), ic = "aicc", approximation = FALSE),
      B_23 = ARIMA(valor ~ brent_2 + brent_3 + pdq(d = d_externo) + PDQ(D = D_externo), ic = "aicc", approximation = FALSE),
      B_234 = ARIMA(valor ~ brent_2 + brent_3 + brent_4 + pdq(d = d_externo) + PDQ(D = D_externo), ic = "aicc", approximation = FALSE),
      B_3 = ARIMA(valor ~ brent_3 + pdq(d = d_externo) + PDQ(D = D_externo), ic = "aicc", approximation = FALSE),
      B_34 = ARIMA(valor ~ brent_3 + brent_4 + pdq(d = d_externo) + PDQ(D = D_externo), ic = "aicc", approximation = FALSE)
    )
  tabelas_lags[[alvo]] <- glance(ajustes) |>
    select(variavel, .model, AICc, BIC) |>
    mutate(d = d_externo, D = D_externo) |>
    arrange(AICc) |>
    mutate(delta_AICc = AICc - min(AICc, na.rm = TRUE))
  escolhido <- tabelas_lags[[alvo]] |>
    filter(is.finite(AICc)) |>
    slice_head(n = 1)
  stopifnot(nrow(escolhido) == 1)
  ajustes_lags[[alvo]] <- ajustes
  ajustes_escolhidos[[alvo]] <- ajustes |>
    select(variavel, all_of(escolhido$.model)) |>
    rename(reg_arima = all_of(escolhido$.model))
  cat("\n", alvo, "- especificacao escolhida no treino:\n")
  report(ajustes_escolhidos[[alvo]])
}

# 3. Tabela completa e menor AICc por variavel; sem avaliar ou alterar o holdout.
comparacao_lags <- bind_rows(tabelas_lags)
escolhas_lags <- comparacao_lags |>
  group_by(variavel) |>
  filter(is.finite(AICc)) |>
  slice_min(AICc, n = 1, with_ties = FALSE) |>
  ungroup()
print(comparacao_lags, n = Inf, width = Inf)
print(escolhas_lags, width = Inf)

saida_lags <- "data/processed/teste_lags_arima_externos"
dir.create(saida_lags, recursive = TRUE, showWarnings = FALSE)
write_csv(amostra_lags, file.path(saida_lags, "amostra.csv"))
write_csv(comparacao_lags, file.path(saida_lags, "comparacao_lags.csv"))
write_csv(escolhas_lags, file.path(saida_lags, "escolhas_lags.csv"))
saveRDS(list(ajustes = ajustes_lags, escolhidos = ajustes_escolhidos,
             comparacao = comparacao_lags, amostra = amostra_lags),
        file.path(saida_lags, "ajustes_lags.rds"))
