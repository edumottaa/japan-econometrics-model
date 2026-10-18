# Executar na raiz do projeto, depois da preparação de data_bal.
# Teste exploratório: não substitui o CV completo de 47 janelas.
source("src/models/bvar exogeno experimental.R")
# Previsao pela funcao nativa do bsvarSIGNs.

# 1. Avaliar todos os modelos na mesma janela e nas mesmas unidades (100 * log).
avaliar_bvar_exogeno <- function(treino, teste, S = 2000, ajuste = NULL) {
  if (is.null(ajuste)) ajuste <- estimar_bvar_exogeno(treino, S = S)
  set.seed(123)
  sorteios <- bsvarSIGNs::forecast(ajuste$posterior, horizon = nrow(teste))$forecasts
  previsao_var <- treino |>
    model(var = VAR(vars(atividade, cpi_core, juro_curto))) |>
    forecast(h = nrow(teste))
  parametros_var <- distributional::parameters(previsao_var$.distribution)
  metricas <- previsoes <- vector("list", 2)
  alvos <- c("cpi_core", "atividade")

  for (j in seq_along(alvos)) {
    alvo <- alvos[j]
    treino_alvo <- treino |> select(date, valor = all_of(alvo))
    teste_alvo <- teste |> select(date, valor = all_of(alvo))
    previsao_ar <- treino_alvo |>
      model(ar = ARIMA(valor ~ pdq(d = 1, q = 0) + PDQ(D = 0, Q = 0)),
            naive = NAIVE(valor), drift = RW(valor ~ drift())) |>
      forecast(h = nrow(teste))

    # Distribuição marginal do VAR, como no holdout existente.
    pos_var <- match(alvo, c("atividade", "cpi_core", "juro_curto"))
    var_alvo <- tibble(
      .model = "var", date = teste$date,
      valor = distributional::dist_normal(
        mu = map_dbl(parametros_var$mu, ~ .x[pos_var]),
        sigma = sqrt(map_dbl(parametros_var$sigma, ~ .x[pos_var, pos_var]))
      )
    ) |> as_tsibble(key = .model, index = date) |>
      as_fable(response = "valor", distribution = "valor")

    # BVAR: sorteios da variável-alvo, sem refazer a estimação para cada variável.
    pos_bvar <- match(alvo, colnames(ajuste$dados_treino))
    amostras <- sorteios[pos_bvar, , ]  # linhas = meses; colunas = sorteios
    bvar_alvo <- tibble(
      .model = "BVAR exogeno", date = teste$date,
      valor = distributional::dist_sample(split(amostras, row(amostras)))
    ) |> as_tsibble(key = .model, index = date) |>
      as_fable(response = "valor", distribution = "valor")

    previsoes[[j]] <- bind_rows(previsao_ar, var_alvo, bvar_alvo)
    metricas[[j]] <- previsoes[[j]] |>
      accuracy(teste_alvo, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS)) |>
      mutate(variavel = alvo)
  }
  list(metricas = bind_rows(metricas), diagnostico = ajuste$diagnostico,
       previsoes = previsoes)
}

# 2. Holdout: últimos 24 meses, como na avaliação existente.
if (any(has_gaps(data_bal)$.gaps)) stop("A base tem meses faltando.")
h_exogeno <- 24
train_exogeno <- data_bal |> slice_head(n = nrow(data_bal) - h_exogeno)
test_exogeno <- data_bal |> slice_tail(n = h_exogeno)
resultado_holdout_exogeno <- avaliar_bvar_exogeno(train_exogeno, test_exogeno)
holdout_exogeno <- resultado_holdout_exogeno$metricas |>
  select(variavel, .model, RMSE, MAE, CRPS) |> arrange(variavel, RMSE)
print(holdout_exogeno)
print(resultado_holdout_exogeno$diagnostico)
saveRDS(resultado_holdout_exogeno, "data/processed/holdout_exogeno_experimental.rds")

# 3. CV curto: quatro origens predefinidas, horizonte fixo de 12 meses completos.
# 500 sorteios por janela são uma triagem; ampliar depois, se houver ganho consistente.
origens_exogeno <- max(data_bal$date) - c(120, 84, 48, 24)
resultados_cv_exogeno <- vector("list", length(origens_exogeno))
for (i in seq_along(origens_exogeno)) {
  origem_exogeno <- origens_exogeno[i]
  message("CV experimental: janela ", i, "/", length(origens_exogeno), " - ", origem_exogeno)
  treino_exogeno <- data_bal |> filter(date <= origem_exogeno)
  teste_exogeno <- data_bal |> filter(date > origem_exogeno, date <= origem_exogeno + 12)
  stopifnot(nrow(teste_exogeno) == 12)
  resultados_cv_exogeno[[i]] <- avaliar_bvar_exogeno(treino_exogeno, teste_exogeno, S = 500)
  resultados_cv_exogeno[[i]]$origem <- origem_exogeno
  saveRDS(resultados_cv_exogeno, "data/processed/cv_exogeno_experimental_janelas.rds")
}

metricas_cv_exogeno <- map_dfr(resultados_cv_exogeno,
  ~ mutate(.x$metricas, origem = .x$origem))
# Mesma quantidade de meses em cada janela: agregar RMSE pela raiz da média dos MSEs.
cv_exogeno <- metricas_cv_exogeno |>
  group_by(variavel, .model) |>
  summarise(RMSE = sqrt(mean(RMSE^2)), MAE = mean(MAE), CRPS = mean(CRPS), .groups = "drop") |>
  arrange(variavel, RMSE)
print(cv_exogeno)
write_csv(holdout_exogeno, "data/processed/holdout_exogeno_experimental.csv")
write_csv(cv_exogeno, "data/processed/cv_exogeno_experimental.csv")
