.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
task_root <- "C:/Users/dudup/OneDrive/NT Macro"
task_work <- file.path(task_root, "work", "psi_diagnostic")
dir.create(task_work, recursive = TRUE, showWarnings = FALSE)
setwd(task_work)
src <- readLines(file.path(task_root, "model_clean_comments.R"), warn = FALSE)
secrets <- unique(unlist(regmatches(src, gregexpr('"[0-9a-f]{20,}"', src))))
secrets <- gsub('"', '', secrets, fixed = TRUE)
mask <- function(x) {
  for (k in secrets) x <- gsub(k, "[REDACTED]", x, fixed = TRUE)
  x
}
safe_eval <- function(expr, env = .GlobalEnv) {
  withCallingHandlers(eval(expr, envir = env),
    warning = function(w) { cat("WARNING:", mask(conditionMessage(w)), "\n"); invokeRestart("muffleWarning") },
    message = function(m) { cat(mask(conditionMessage(m))); invokeRestart("muffleMessage") })
}
run <- function() {
  exprs <- parse(text = src)
  wanted <- c("series_ids", "fred", "cpi", "url", "le_jgb", "jgb", "ip_mundial",
              "atividade", "shadow", "boj", "call_rate", "juro_curto", "data",
              "data_modelo", "ultimo", "data_bal", "matrix_externa", "matrix_interna")
  cache_names <- c("fred", "cpi", "jgb", "ip_mundial", "atividade", "shadow", "boj")
  for (f in c("ci.xlsx", "cpb_wtm.xlsx")) {
    if (!file.exists(f)) stopifnot(file.copy(file.path(task_root, f), f))
  }
  for (e in exprs) {
    if (!is.call(e)) next
    op <- as.character(e[[1]])[1]
    if (op %in% c("library", "fredr_set_key")) {
      safe_eval(e)
    } else if (op %in% c("<-", "=") && is.symbol(e[[2]])) {
      nm <- as.character(e[[2]])
      if (!nm %in% wanted) next
      cache <- paste0("cache_", nm, ".rds")
      cat("STAGE", nm, "\n")
      flush.console()
      if (nm %in% cache_names && file.exists(cache)) {
        assign(nm, readRDS(cache), envir = .GlobalEnv)
      } else {
        safe_eval(e)
        if (nm %in% cache_names) saveRDS(get(nm, envir = .GlobalEnv), cache)
      }
      value <- get(nm, envir = .GlobalEnv)
      if (!is.null(dim(value))) cat("DIM", nm, paste(dim(value), collapse = " x "), "\n")
      if (nm == "matrix_interna") break
    }
  }
  Y <- cbind(matrix_externa, matrix_interna)
  dates <- as.character(data_bal$date)
  saveRDS(list(Y = Y, dates = dates, data = data, data_modelo = data_modelo,
               matrix_externa = matrix_externa, matrix_interna = matrix_interna), "data_bundle.rds")
  cat("SAMPLE", dates[1], "to", tail(dates, 1), "n =", nrow(Y), "\n")
  cat("PACKAGES bsvarSIGNs", as.character(packageVersion("bsvarSIGNs")),
      "bsvars", as.character(packageVersion("bsvars")), "\n")
  print(colSums(!is.finite(Y)))
  if (any(!is.finite(Y))) stop("Input matrices contain missing or nonfinite values; cannot estimate this exact sample.")
  dY <- Y[-1, , drop = FALSE] - Y[-nrow(Y), , drop = FALSE]
  spec <- specify_bsvarSIGN$new(data = matrix_interna, p = 12, foreign = matrix_externa)
  psi_initial <- spec$prior$hyper[4:13, 1]
  diagnostics <- data.frame(variable = colnames(Y), psi_initial = psi_initial,
    sd_monthly_change = apply(dY, 2, sd), largest_monthly_change = apply(abs(dY), 2, max),
    largest_change_month = dates[-1][apply(abs(dY), 2, which.max)])
  write.csv(diagnostics, "initial_diagnostics.csv", row.names = FALSE)
  print(diagnostics, row.names = FALSE)
  write.csv(data.frame(date = dates, Y, check.names = FALSE), "data_model.csv", row.names = FALSE)
  trace("optim", where = asNamespace("stats"),
        tracer = quote(assign("diagnostic_optim_init", par, .GlobalEnv)),
        exit = quote(assign("diagnostic_optim_result", returnValue(), .GlobalEnv)), print = FALSE)
  on.exit(untrace("optim", where = asNamespace("stats")), add = TRUE)
  cat("ESTIMATE baseline S=15000 burn_in=5000\n")
  flush.console()
  set.seed(123)
  system.time(spec$estimate_hyper(S = 15000, burn_in = 5000)) |> print()
  h <- t(spec$prior$hyper)
  colnames(h) <- c("mu", "delta", "lambda", paste0("psi_", colnames(Y)), paste0("covid", 1:4))
  saveRDS(spec, "baseline_spec.rds")
  saveRDS(h, "baseline_hyper.rds")
  if (exists("diagnostic_optim_result", envir = .GlobalEnv)) {
    opt <- get("diagnostic_optim_result", .GlobalEnv)
    saveRDS(list(initial = diagnostic_optim_init, result = opt), "baseline_optim.rds")
    cat("OPTIM convergence", opt$convergence, "message", mask(opt$message), "\n")
    cat("HESSIAN eigenvalues\n")
    print(eigen(opt$hessian, symmetric = TRUE, only.values = TRUE)$values)
  }
  summary_h <- t(apply(h, 2, function(x) c(median = median(x), p025 = quantile(x, .025),
      p975 = quantile(x, .975), min = min(x), max = max(x), first = x[1], last = tail(x, 1))))
  write.csv(data.frame(parameter = rownames(summary_h), summary_h, row.names = NULL),
            "baseline_hyper_summary.csv", row.names = FALSE)
  cat("HYPER DIM", paste(dim(h), collapse = " x "), "\n")
  print(summary_h)
  png("baseline_psi_traces.png", width = 1800, height = 1200, res = 150)
  par(mfrow = c(5, 2), mar = c(2, 4, 2, 1))
  for (i in 4:13) plot(h[, i], type = "l", main = colnames(h)[i], xlab = "", ylab = "psi")
  dev.off()
  cat("DONE baseline\n")
}
tryCatch(run(), error = function(e) {
  cat("ERROR:", mask(conditionMessage(e)), "\n")
  quit(status = 1)
})
