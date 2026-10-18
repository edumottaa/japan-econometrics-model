args <- commandArgs(trailingOnly = TRUE)
source("C:/Users/dudup/OneDrive/japan-econometrics-model/src/evaluation/cv_covid_nativo/comum.R")
out <- args[1]
for (pasta in c("windows", "logs", "status", "tabelas")) dir.create(file.path(out, pasta), recursive = TRUE, showWarnings = FALSE)
if (file.exists(file.path(out, "benchmarks.rds"))) quit(status = 0)

dados <- readRDS(file.path(root, "data/processed/cv_comparacao_20261017_193604/data_frozen.rds")) |>
  select(date, all_of(variaveis))
stopifnot(ncol(dados) == 10L, all(is.finite(as.matrix(select(as_tibble(dados), -date)))),
  !any(tsibble::has_gaps(dados)$.gaps))
cv_data <- dados |> stretch_tsibble(.init = 180, .step = 3)
origens <- cv_data |> as_tibble() |> group_by(.id) |> summarise(n = n(), origem = max(date), .groups = "drop") |>
  filter(origem < max(dados$date))
stopifnot(nrow(origens) == 47L)
saveRDS(dados, file.path(out, "dados.rds")); saveRDS(origens, file.path(out, "origens.rds"))
write_csv(modelos, file.path(out, "modelos.csv"))
resultados <- caminhos <- vector("list", nrow(origens))
for (i in seq_len(nrow(origens))) {
  treino <- dados |> slice_head(n = origens$n[i])
  teste <- dados |> filter(date > origens$origem[i], date <= origens$origem[i] + H)
  univariados <- map_dfr(domesticas, function(v) {
    fc <- treino |> select(date, valor = all_of(v)) |>
      model(ar = ARIMA(valor ~ pdq(d = 1, q = 0) + PDQ(D = 0, Q = 0)),
        naive = NAIVE(valor), drift = RW(valor ~ drift())) |> forecast(h = H)
    map_dfr(unique(fc$.model), function(nome) {
      f <- fc |> filter(.model == nome)
      avaliar_distribuicao(f$valor, treino, teste, i, nome, v)
    })
  })
  fc_var <- treino |> model(var = VAR(vars(atividade, cpi_core, juro_curto))) |> forecast(h = H)
  parametros <- distributional::parameters(fc_var$.distribution)
  var <- map_dfr(c("atividade", "cpi_core", "juro_curto"), function(v) {
    k <- match(v, c("atividade", "cpi_core", "juro_curto"))
    dist <- distributional::dist_normal(map_dbl(parametros$mu, function(m) m[k]),
      sqrt(map_dbl(parametros$sigma, function(m) m[k, k])))
    avaliar_distribuicao(dist, treino, teste, i, "var", v)
  })
  caminhos[[i]] <- treino |> model(externo = VAR(vars(ip_mundial, brent, vix, ust2y))) |>
    forecast(h = H) |> as_tibble() |> arrange(date) |> pull(.mean)
  stopifnot(all(is.finite(caminhos[[i]])))
  resultados[[i]] <- bind_rows(univariados, var)
  cat("Benchmarks", i, "/47\n"); flush.console()
}
benchmarks <- bind_rows(resultados)
stopifnot(!anyDuplicated(benchmarks[c(".id", ".model", "variavel", "h")]),
  all(is.finite(benchmarks$CRPS)), all(is.finite(benchmarks$previsao)))
saveRDS(benchmarks, file.path(out, "benchmarks.rds")); saveRDS(caminhos, file.path(out, "caminhos_externos.rds"))
write_csv(benchmarks, file.path(out, "benchmarks.csv"))
status(out, "preparacao", estado = "concluida", origens = 47, variaveis_modelo = 9,
  frequencia = "mensal", passo_cv_meses = 3, horizonte = 24, protocolo = "funcoes nativas; CRPS fabletools")