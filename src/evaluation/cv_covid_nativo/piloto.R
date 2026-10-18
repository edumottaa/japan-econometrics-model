.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
suppressPackageStartupMessages({library(fpp3); library(readr); library(bsvarSIGNs); library(bsvars); library(coda)})
root <- "C:/Users/dudup/OneDrive/japan-econometrics-model"
out <- file.path(root, "data/processed/cv_covid_nativo_20261018")
d <- readRDS(file.path(root, "data/processed/cv_comparacao_20261017_193604/data_frozen.rds"))
externas <- c("ust2y", "ip_mundial", "brent", "vix")
domesticas <- c("atividade", "cpi_core", "juro_curto", "inclinacao", "usdjpy")
resultados <- list()
for (fixos in c(FALSE, TRUE)) for (i in c(26L, 27L)) {
  n <- 180L + 3L * (i - 1L); p <- 6L
  treino <- d |> slice_head(n = n) |> as_tibble()
  covid <- match(as.Date("2020-03-01"), as.Date(treino$date)) - p
  set.seed(123)
  resultado <- tryCatch({
    spec <- specify_bsvarSIGN$new(data = as.matrix(select(treino, all_of(domesticas))),
      foreign = as.matrix(select(treino, all_of(externas))), p = p,
      hyper_mu = FALSE, hyper_delta = !fixos, hyper_lambda = !fixos,
      hyper_psi = FALSE, hyper_covid = covid, sign_structural = matrix(NA_real_, 9, 9))
    spec$estimate_hyper(S = 5000, burn_in = 2500)
    post <- bsvars::estimate(spec, S = 2500, show_progress = FALSE)
    tibble(fix_lambda_delta = fixos, fold = i, ess = post$posterior$ess,
      distintos = ncol(unique(matrix(post$posterior$A, ncol = 2500), MARGIN = 2)), erro = NA_character_)
  }, error = function(e) tibble(fix_lambda_delta = fixos, fold = i, ess = NA_real_,
    distintos = NA_integer_, erro = conditionMessage(e)))
  resultados[[length(resultados) + 1L]] <- resultado
  print(resultado); flush.console()
  write_csv(bind_rows(resultados), file.path(out, "piloto_concentracao.csv"))
}