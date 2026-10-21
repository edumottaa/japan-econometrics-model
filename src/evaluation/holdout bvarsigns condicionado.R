# Holdout condicional: petroleo e Treasury realizados; IP mundial e VIX projetados.
# Executar apos preparar data_bal. Os indices ja estao em 100 * log(x).

library(tidyverse)
library(fpp3)
library(distributional)
library(bsvars)
library(bsvarSIGNs)

h <- 24
p <- 6
medidas <- list(RMSE = RMSE, MAE = MAE, CRPS = CRPS)

dados_holdout <- data_bal |>
  select(date, ust2y, ip_mundial, brent, vix,
         atividade, cpi_core, juro_curto, inclinacao, usdjpy) |>
  arrange(date)

stopifnot(nrow(dados_holdout) > h + p + 12,
          !any(tsibble::has_gaps(dados_holdout)$.gaps),
          all(is.finite(as.matrix(select(as_tibble(dados_holdout), -date)))),
          as.character(packageVersion("bsvarSIGNs")) == "3.0")

# 1. Reservar os ultimos 24 meses para teste.
treino <- dados_holdout |> slice_head(n = nrow(dados_holdout) - h)
teste <- dados_holdout |> slice_tail(n = h)

treino_alvos <- treino |>
  select(date, atividade, cpi_core) |>
  pivot_longer(-date, names_to = "variavel", values_to = "valor") |>
  as_tsibble(key = variavel, index = date)

teste_alvos <- teste |>
  select(date, atividade, cpi_core) |>
  pivot_longer(-date, names_to = "variavel", values_to = "valor") |>
  as_tsibble(key = variavel, index = date)

# 2. Benchmarks univariados, separados automaticamente por variavel.
prev_fable <- treino_alvos |>
  model(ar = ARIMA(valor ~ pdq(d = 1, q = 0) + PDQ(D = 0, Q = 0)),
        naive = NAIVE(valor), drift = RW(valor ~ drift())) |>
  forecast(h = h)

# Mesmo VAR domestico do holdout original.
prev_var <- treino |>
  model(var = VAR(vars(atividade, cpi_core, juro_curto))) |>
  forecast(h = h)

param_var <- parameters(prev_var$.distribution)
prev_var_alvos <- bind_rows(
  tibble(variavel = "atividade", .model = "var", date = prev_var$date,
         valor = dist_normal(map_dbl(param_var$mu, 1),
                             sqrt(map_dbl(param_var$sigma, ~ .x[1, 1])))),
  tibble(variavel = "cpi_core", .model = "var", date = prev_var$date,
         valor = dist_normal(map_dbl(param_var$mu, 2),
                             sqrt(map_dbl(param_var$sigma, ~ .x[2, 2]))))
) |>
  mutate(.mean = mean(valor)) |>
  as_tsibble(key = c(variavel, .model), index = date) |>
  as_fable(response = "valor", distribution = "valor")

# 3. Regressoes com erros ARIMA, estimadas somente no treino.
treino_externo <- treino |>
  select(date, brent, ip_mundial, vix) |>
  pivot_longer(c(ip_mundial, vix), names_to = "variavel", values_to = "valor") |>
  as_tsibble(key = variavel, index = date)

fit_externo <- treino_externo |>
  model(reg_arima = ARIMA(valor ~ brent))

# Apenas o petroleo realizado entra nas regressoes; IP e VIX do teste ficam fora.
futuro_externo <- new_data(treino_externo, h) |>
  left_join(as_tibble(select(teste, date, brent)), by = "date")

prev_externo <- fit_externo |>
  forecast(new_data = futuro_externo)

# Medias previstas fixas: as bandas posteriores nao incluem sua incerteza.
caminhos_externos <- prev_externo |>
  as_tibble() |>
  select(date, variavel, .mean) |>
  pivot_wider(names_from = variavel, values_from = .mean) |>
  left_join(as_tibble(select(teste, date, ust2y, brent)), by = "date") |>
  arrange(date) |>
  select(ust2y, ip_mundial, brent, vix) |>
  as.matrix()

stopifnot(nrow(caminhos_externos) == h, all(is.finite(caminhos_externos)))

# 4. Estimar uma unica vez o BVAR reduzido, mantendo a estrutura foreign atual.
dados_treino <- treino |> as_tibble() |> select(-date) |> as.matrix()
inicio_covid <- match(as.Date("2020-03-01"), as.Date(treino$date)) - p
if (is.na(inicio_covid) || inicio_covid <= 0) inicio_covid <- NULL
if (!is.null(inicio_covid) && nrow(treino) - p - inicio_covid + 1L < 4L) inicio_covid <- NULL

set.seed(123)
spec_holdout <- specify_bsvarSIGN$new(
  data = dados_treino[, 5:9], foreign = dados_treino[, 1:4], p = p,
  hyper_mu = FALSE, hyper_delta = FALSE, hyper_lambda = FALSE, hyper_psi = FALSE,
  hyper_covid = inicio_covid, sign_structural = matrix(NA_real_, 9, 9)
)
if (!is.null(inicio_covid)) spec_holdout$estimate_hyper(S = 5000, burn_in = 2500)
post_holdout <- bsvars::estimate(spec_holdout, S = 2500, show_progress = FALSE)

coeficientes <- matrix(post_holdout$posterior$A, ncol = 2500)
lags_domesticos <- unlist(lapply(0:(p - 1), function(j) 5:9 + 9 * j))
diagnostico_holdout <- tibble(
  finitos = all(is.finite(coeficientes)) && all(is.finite(post_holdout$posterior$Sigma)),
  distintos = ncol(unique(coeficientes, MARGIN = 2)),
  ess = post_holdout$posterior$ess,
  zero_lags = max(abs(post_holdout$posterior$A[1:4, lags_domesticos, ])),
  zero_impacto = max(abs(post_holdout$posterior$Theta0[1:4, 5:9, ]))
)
print(diagnostico_holdout)
stopifnot(diagnostico_holdout$finitos, is.finite(diagnostico_holdout$ess),
          diagnostico_holdout$ess >= 50, diagnostico_holdout$distintos >= 50,
          diagnostico_holdout$zero_lags < 1e-8, diagnostico_holdout$zero_impacto < 1e-8)

# 5. Prever com o mesmo posterior: livre e condicionado as quatro trajetorias.
condicoes <- cbind(caminhos_externos, matrix(NA_real_, h, 5))
set.seed(123)
previsoes_bvar <- list("BVAR reduzido" = bsvarSIGNs::forecast(post_holdout, horizon = h))
set.seed(123)
previsoes_bvar[["BVAR condicionado"]] <- bsvarSIGNs::forecast(
  post_holdout, horizon = h, conditional_forecast = condicoes
)

# Correcao em R do estado inicial e dos lags do forecast 3.0, como no CV atual.
# O ganho ajusta a media condicional; as inovacoes sorteadas pelo pacote sao mantidas.
estado_inicial <- c(t(dados_treino[nrow(dados_treino) - (0:(p - 1)), , drop = FALSE]), 1)
for (m in seq_along(previsoes_bvar)) {
  for (s in seq_len(dim(previsoes_bvar[[m]]$forecasts)[3])) {
    estado <- estado_inicial
    ganho <- matrix(0, 9, 4)
    if (m == 2) {
      sigma <- post_holdout$posterior$Sigma[, , s]
      ganho <- sigma[, 1:4] %*% solve(sigma[1:4, 1:4])
    }
    for (j in seq_len(h)) {
      media <- drop(post_holdout$posterior$A[, , s] %*% estado)
      ajuste <- media - previsoes_bvar[[m]]$forecast_mean[, j, s]
      previsoes_bvar[[m]]$forecasts[, j, s] <- previsoes_bvar[[m]]$forecasts[, j, s] +
        ajuste - drop(ganho %*% ajuste[1:4])
      if (m == 2) previsoes_bvar[[m]]$forecasts[1:4, j, s] <- caminhos_externos[j, ]
      previsoes_bvar[[m]]$forecast_mean[, j, s] <- media
      estado <- c(previsoes_bvar[[m]]$forecasts[, j, s], head(estado, 9 * (p - 1)), 1)
    }
  }
}

stopifnot(all(is.finite(previsoes_bvar[[1]]$forecasts)),
          all(is.finite(previsoes_bvar[[2]]$forecasts)),
          max(abs(sweep(previsoes_bvar[[2]]$forecasts[1:4, , ],
                        c(1, 2), t(caminhos_externos), "-"))) < 1e-8)

# 6. Adaptar somente as marginais domesticas para o fluxo fable.
prev_bvar_alvos <- list()
for (m in names(previsoes_bvar)) for (alvo in c("atividade", "cpi_core")) {
  posicao <- match(alvo, colnames(dados_treino))
  sorteios <- matrix(previsoes_bvar[[m]]$forecasts[posicao, , ], nrow = h)
  prev_bvar_alvos[[paste(m, alvo)]] <- tibble(
    variavel = alvo, .model = m, date = teste$date,
    valor = dist_sample(split(sorteios, row(sorteios)))
  )
}
prev_bvar_alvos <- bind_rows(prev_bvar_alvos) |>
  mutate(.mean = mean(valor)) |>
  as_tsibble(key = c(variavel, .model), index = date) |>
  as_fable(response = "valor", distribution = "valor")

# Benchmarks livres versus BVAR condicional ex post, nos mesmos meses de teste.
acc_holdout <- bind_rows(prev_fable, prev_var_alvos, prev_bvar_alvos) |>
  accuracy(teste_alvos, measures = medidas)

holdout_results <- acc_holdout |>
  filter(variavel == "cpi_core") |>
  select(.model, .type, RMSE, MAE, CRPS) |>
  arrange(RMSE)

holdout_results_atividade <- acc_holdout |>
  filter(variavel == "atividade") |>
  select(.model, .type, RMSE, MAE, CRPS) |>
  arrange(RMSE)

holdout_results
holdout_results_atividade
