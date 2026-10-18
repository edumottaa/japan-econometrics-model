args <- commandArgs(trailingOnly = TRUE)
source("C:/Users/dudup/OneDrive/japan-econometrics-model/src/evaluation/comparacao_cv_3_6/common.R")
run_dir <- args[1]; dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
for (d in c("windows", "logs", "status", "figures")) dir.create(file.path(run_dir, d), showWarnings = FALSE)
bundle_path <- file.path(root, "data/processed/diagnostics/data_bundle.rds")
b <- readRDS(bundle_path)
data <- b$data_modelo |> select(date, all_of(vars)) |> drop_na() |> arrange(date)
stopifnot(!any(has_gaps(data)$.gaps), nrow(data) > 180)
config <- list(created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  init = 180, step = 3, h = 12, n_draw = 5000, n_burn = 2500, signs_draws = 2500,
  lag_values = c(3, 6), bvar_hyper = c("lambda", "soc", "sur"), psi_fixed = TRUE,
  signs_lambda = .2, signs_mu = 1, signs_delta = 1, seed = 123,
  origins = seq(180, nrow(data) - 1, by = 3), variables = vars,
  source_bundle = bundle_path, source_md5 = unname(tools::md5sum(bundle_path)),
  packages = sapply(c("BVAR", "bsvarSIGNs", "bsvars", "fable", "fabletools"), function(p) as.character(packageVersion(p))),
  date_min = as.character(min(data$date)), date_max = as.character(max(data$date)), n = nrow(data))
saveRDS(data, file.path(run_dir, "data_frozen.rds")); saveRDS(config, file.path(run_dir, "config.rds"))
jsonlite::write_json(config, file.path(run_dir, "config.json"), pretty = TRUE, auto_unbox = TRUE)
log_line("Base congelada:", nrow(data), "meses;", length(config$origins), "origens.")
# Verificar o CRPS empírico exato contra sua definição por diferenças par a par.
x <- c(-2, -.2, .5, 1, 3)
stopifnot(abs(crps_emp(x, .3) - (mean(abs(x - .3)) - mean(abs(outer(x, x, "-"))) / 2)) < 1e-12)
# Teste de uma previsão AR(1): variâncias teóricas 4 e 5, incluindo propagação.
set.seed(987)
obj <- list(posterior = list(A = array(rep(c(.5, 0), 20000), c(1, 2, 20000)),
  Sigma = array(4, c(1, 1, 20000))), last_draw = list(p = 1))
f <- prever_bsvarsigns(obj, matrix(0, 1, 1), 2)
stopifnot(abs(var(f[1, 1, ]) - 4) < .2, abs(var(f[1, 2, ]) - 5) < .25)
log_line("Verificações de CRPS e previsão recursiva passaram.")
paths <- vector("list", length(config$origins)); rows <- vector("list", length(paths))
for (i in seq_along(config$origins)) {
  n <- config$origins[i]; train <- data |> slice_head(n = n)
  test <- data |> slice((n + 1):min(n + config$h, nrow(data)))
  window_rows <- list()
  for (v in alvos) {
    fc <- train |> select(date, valor = all_of(v)) |>
      model(ar = ARIMA(valor ~ pdq(d = 1, q = 0) + PDQ(D = 0, Q = 0)), naive = NAIVE(valor), drift = RW(valor ~ drift())) |>
      forecast(h = config$h) |> filter(date <= max(data$date))
    for (nm in unique(fc$.model)) {
      a <- fc |> filter(.model == nm); mu <- mean(a$valor); sd <- sqrt(distributional::variance(a$valor))
      window_rows[[paste(v, nm)]] <- score_normal(nm, i, v, max(train$date), a$date, test[[v]], mu, sd)
    }
  }
  fc_var <- train |> model(var = VAR(vars(atividade, cpi_core, juro_curto))) |> forecast(h = config$h)
  pv <- distributional::parameters(fc_var$.distribution)
  for (v in alvos) {
    k <- match(v, c("atividade", "cpi_core", "juro_curto")); nh <- nrow(test)
    window_rows[[paste(v, "var")]] <- score_normal("var", i, v, max(train$date), test$date, test[[v]],
      map_dbl(pv$mu[seq_len(nh)], ~ .x[k]), sqrt(map_dbl(pv$sigma[seq_len(nh)], ~ .x[k, k])))
  }
  paths[[i]] <- train |> model(aicc = VAR(vars(ip_mundial, brent, vix, ust2y))) |>
    forecast(h = config$h) |> as_tibble() |> arrange(date) |> pull(.mean)
  rows[[i]] <- bind_rows(window_rows)
  log_line("Benchmarks e caminhos externos:", i, "/", length(paths))
  write_status(file.path(run_dir, "status/prepare.json"), state = "running", completed = i, total = length(paths))
}
saveRDS(bind_rows(rows), file.path(run_dir, "benchmarks.rds"))
saveRDS(paths, file.path(run_dir, "external_paths.rds"))
write_status(file.path(run_dir, "status/prepare.json"), state = "complete", completed = length(paths), total = length(paths))
log_line("PREPARACAO CONCLUIDA")
