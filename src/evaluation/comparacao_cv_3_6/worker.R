args <- commandArgs(trailingOnly = TRUE)
source("C:/Users/dudup/OneDrive/japan-econometrics-model/src/evaluation/comparacao_cv_3_6/common.R")
run_dir <- args[1]; family <- args[2]; p <- as.integer(args[3])
shard <- as.integer(args[4]); shards <- as.integer(args[5]); job <- paste(family, p, shard, sep = "_")
config <- readRDS(file.path(run_dir, "config.rds")); data <- readRDS(file.path(run_dir, "data_frozen.rds"))
paths <- readRDS(file.path(run_dir, "external_paths.rds"))
ids <- seq_along(config$origins); ids <- ids[(ids - 1) %% shards == shard - 1]
status_file <- file.path(run_dir, "status", paste0(job, ".json"))
if (family == "signs") {
  # Observar os pesos antes da reamostragem, sem mudar o algoritmo do pacote.
  trace("importance_sampling", where = asNamespace("bsvarSIGNs"),
    tracer = quote(assign(".cv_raw_weights", posterior$posterior$w, envir = .GlobalEnv)), print = FALSE)
}
completed <- 0; job_start <- Sys.time()
for (i in ids) {
  window_file <- file.path(run_dir, "windows", sprintf("%s_p%d_w%03d.rds", family, p, i))
  if (file.exists(window_file)) {completed <- completed + 1; next}
  n <- config$origins[i]; train <- data |> slice_head(n = n)
  test <- data |> slice((n + 1):min(n + config$h, nrow(data)))
  Y <- as.matrix(select(as_tibble(train), all_of(vars)))
  start <- Sys.time(); warnings_seen <- character(); stage <- "estimation"
  write_status(status_file, state = "running", family = family, lag = p, fold = i, completed = completed, total = length(ids), pid = Sys.getpid())
  log_line(job, "INICIO", i, "origem", as.character(max(train$date)))
  result <- tryCatch(withCallingHandlers({
    set.seed(config$seed + i)
    if (family == "native") {
      priors <- bv_priors(hyper = config$bvar_hyper,
        mn = bv_minnesota(lambda = bv_lambda(), psi = bv_psi()), soc = bv_soc(), sur = bv_sur(min = .001))
      mh <- bv_mh(adjust_acc = TRUE, acc_lower = .2, acc_upper = .4, acc_change = .1)
      fit <- BVAR::bvar(Y, lags = p, n_draw = config$n_draw, n_burn = config$n_burn,
        n_thin = 1, priors = priors, mh = mh, verbose = FALSE)
      diagnostic <- diagnostico_bvar(fit, p, i)
      stage <- "forecast_normal"; set.seed(100000 + i)
      F <- prever_normal(fit, Y, config$h)
      scores <- score_draws(F, train, test, i, paste0("BVAR_normal_p", p), diagnostic$quality)
      stage <- "forecast_conditional"; set.seed(200000 + i)
      conditional_error <- NULL
      conditional <- tryCatch({
        fc <- predict(fit, horizon = config$h, cond_path = paths[[i]], cond_vars = colnames(paths[[i]]))
        # Confirmar que os caminhos impostos foram respeitados em todos os sorteios.
        err <- max(vapply(seq_len(ncol(paths[[i]])), function(j) {
          k <- match(colnames(paths[[i]])[j], fit$variables)
          max(abs(sweep(fc$fcast[, , k], 2, paths[[i]][, j], "-")))
        }, numeric(1)))
        stopifnot(err < 1e-6)
        score_draws(aperm(fc$fcast, c(3, 2, 1)), train, test, i,
          paste0("BVAR_condicionado_p", p), diagnostic$quality)
      }, error = function(e) {conditional_error <<- conditionMessage(e); NULL})
      list(scores = bind_rows(scores, conditional), diagnostic = diagnostic, error = conditional_error)
    } else {
      spec <- specify_bsvarSIGN$new(data = Y[, 5:10], foreign = Y[, 1:4], p = p,
        hyper_mu = FALSE, hyper_delta = FALSE, hyper_lambda = FALSE, hyper_psi = FALSE, mc.cores = 1)
      .cv_raw_weights <- NULL
      fit <- bsvars::estimate(spec, S = config$signs_draws, show_progress = FALSE)
      diagnostic <- diagnostico_signs(fit, .cv_raw_weights, p, i)
      stage <- "forecast"; set.seed(100000 + i)
      F <- prever_bsvarsigns(fit, Y, config$h)
      list(scores = score_draws(F, train, test, i, paste0("bsvarSIGNs_foreign_p", p), diagnostic$quality),
        diagnostic = diagnostic, error = NULL)
    }
  }, warning = function(w) {warnings_seen <<- c(warnings_seen, conditionMessage(w)); invokeRestart("muffleWarning")}),
    error = function(e) list(scores = tibble(), diagnostic = tibble(family = family, p = p, fold = i,
      quality = "execution_failure"), error = paste(stage, conditionMessage(e), sep = ": ")))
  result$warnings <- unique(warnings_seen); result$origin <- max(train$date)
  result$elapsed_seconds <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  result$stage <- stage
  saveRDS(result, window_file)
  completed <- completed + 1
  log_line(job, "FIM", i, "segundos", round(result$elapsed_seconds, 1), "qualidade", result$diagnostic$quality,
    "erro", if (is.null(result$error)) "nenhum" else result$error)
  write_status(status_file, state = "running", family = family, lag = p, fold = i, completed = completed,
    total = length(ids), pid = Sys.getpid(), last_quality = result$diagnostic$quality)
  rm(result, Y); gc(verbose = FALSE)
}
write_status(status_file, state = "complete", family = family, lag = p, completed = completed,
  total = length(ids), pid = Sys.getpid(), elapsed_seconds = as.numeric(difftime(Sys.time(), job_start, units = "secs")))
log_line(job, "CONCLUIDO")
