task_root <- "C:/Users/dudup/OneDrive/NT Macro"
task_work <- file.path(task_root, "work", "psi_diagnostic")
helper <- file.path(task_root, "Modelagem", "corrigir_psi.R")
writeLines(c(readLines(file.path(task_work, "stable_likelihood.R")), "",
             readLines(file.path(task_work, "wrapper_hyper.R"))), helper)
src <- readLines(file.path(task_root, "model_clean_comments.R"), warn = FALSE)
src <- src[!grepl("Ta dando erro", src, fixed = TRUE)]
start <- grep("^# 6 Specify", src)
end <- grep("^#7", src)
replacement <- c(
  "# 6 Specify the BSVAR and estimate the hierarchical prior",
  paste0('source("', helper, '")'),
  "",
  "# March 2020, counting only the observations used after the 12 lags",
  'inicio_covid <- match(as.Date("2020-03-01"), as.Date(data_bal$date)) - 12L',
  "spec <- specify_bsvarSIGN$new(",
  "  data = matrix_interna, p = 12, foreign = matrix_externa,",
  "  # Stage 2 of the plan: reduced form, before structural identification",
  "  sign_structural = matrix(NA_real_, 10, 10),",
  "  hyper_covid = inicio_covid",
  ")",
  "set.seed(123)",
  "estimar_hyper_corrigido(spec, S = 15000, burn_in = 5000, inicio_covid = inicio_covid)",
  "")
src <- c(src[1:(start - 1)], replacement, src[end:length(src)])
start <- grep("^# 8 Estimate", src)
end <- grep("^#Cross", src)
src <- c(src[1:(start - 1)],
  "# Hyperparameter estimation is complete; keep this spec object.",
  "# The following estimate(spec, S = 1000) stage produced an additional",
  "# numerical covariance error in bsvarSIGNs 3.0 and needs a separate repair.",
  'message("Hiperparametros estimados. A etapa posterior do VAR ainda precisa de ajuste numerico.")',
  "", "",
  src[end:length(src)])
output <- file.path(task_root, "model_psi_corrigido.R")
writeLines(src, output)
invisible(parse(helper))
invisible(parse(output))
cat("DELIVERY CREATED AND PARSED\n")
