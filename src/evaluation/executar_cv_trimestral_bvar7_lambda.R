Sys.setlocale("LC_CTYPE", "English_United States.utf8")
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
modelos_cv <- 3L
saida_cv <- "data/processed/cv_trimestral_bvar7_lambda_20261020"
saida <- saida_cv
dir.create(saida, recursive = TRUE, showWarnings = FALSE)
status <- function(estado, erro = NULL) {
  jsonlite::write_json(list(estado = estado, atualizado = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
                           erro = erro), file.path(saida, "status.json"), auto_unbox = TRUE, pretty = TRUE)
}
status("executando")
tryCatch({
  base <- "data/processed/cv_estrutural_mensal_20261019/data_frozen.rds"
  script <- "src/evaluation/CV trimestral BVAR 7 lambda.R"
  data_bal <- readRDS(base)
  jsonlite::write_json(list(base = base, md5_base = unname(tools::md5sum(base)),
                           script = script, md5_script = unname(tools::md5sum(script)),
                           p = 6, H = 24, step = 3, init = 180, S = 2500,
                           checkpoints = FALSE, modelos = modelos_cv, ess_minimo = 30, ess_alerta = 50,
                           hiperparametros = list(lambda = "estimado", mu = 1, delta = 1, psi = "calibrado no treino e fixo"),
                           especificacao_externa_retrospectiva = FALSE),
                      file.path(saida, "configuracao.json"), auto_unbox = TRUE, pretty = TRUE)
  grDevices::pdf(file = NULL)
  source(script, encoding = "UTF-8", echo = FALSE)
  stopifnot(nrow(diagnosticos) == 47L,
            dplyr::n_distinct(diagnosticos$.id) == 47L,
            nrow(metricas_acumuladas) == 20L,
            all(metricas_acumuladas$casos[metricas_acumuladas$horizonte == "H1-H24"] == 1028L),
            file.exists(file.path(saida, "resultados_finais.rds")),
            all(file.exists(file.path(saida, c("cpi_core_h24.png", "atividade_h24.png",
                                               "cpi_core_periodos_h24.png", "atividade_periodos_h24.png")))))
  status("concluido")
}, error = function(e) {
  status("falhou", conditionMessage(e))
  message("FALHA: ", conditionMessage(e))
  quit(status = 1L)
})
