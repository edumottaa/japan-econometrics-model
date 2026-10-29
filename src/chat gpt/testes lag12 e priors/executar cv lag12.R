Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(TZ = "America/Sao_Paulo")
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
source("src/chat gpt/testes lag12 e priors/CV bvars lag12.R", encoding = "UTF-8")
source("src/chat gpt/testes lag12 e priors/0- configuracoes.R", encoding = "UTF-8")
args <- commandArgs(trailingOnly = TRUE)
formato <- args[1]
estrutural <- length(args) >= 2L && args[2] == "estrutural"
piloto <- length(args) >= 3L && args[3] == "piloto"
janelas <- if (length(args) >= 5L) seq.int(as.integer(args[4]), as.integer(args[5])) else seq_len(nrow(origens))
cfg <- configuracoes |> filter(.data$formato == .env$formato)
stopifnot(nrow(cfg) == 1L)
nome_modelo <- paste(if (estrutural) "Estrutural" else "Reduzido", cfg$nome)
tryCatch(rodar_cv(formato, piloto, janelas, estrutural, nome_modelo), error = function(e) {
  pasta <- file.path(raiz_resultados, "cv", if (estrutural) paste0("estrutural_", formato) else formato)
  gravar_status(pasta, "falhou", erro = conditionMessage(e))
  stop(e)
})
