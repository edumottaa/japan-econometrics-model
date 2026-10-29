Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(TZ = "America/Sao_Paulo")
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
source("src/chat gpt/testes lag12 e priors/0- configuracoes.R", encoding = "UTF-8")
cfg <- configuracoes |> filter(formato == "p12_referencia")
treino <- dados_cv |> slice_head(n = 180L)
spec <- preparar_spec(treino, cfg, estrutural = FALSE)
set.seed(124)
inicio <- Sys.time()
post <- bsvars::estimate(spec$spec, S = 50000L, show_progress = TRUE)
cat("CAMPOS:", names(post$posterior), "\n")
print(tibble(propostas = dim(post$posterior$A)[3], ess = post$posterior$ess,
  distintos = ncol(unique(matrix(post$posterior$A, ncol = 50000L), MARGIN = 2)),
  segundos = as.numeric(difftime(Sys.time(), inicio, units = "secs"))))
if (post$posterior$ess >= 50) {
  pasta <- file.path(raiz_resultados, "diagnostico_amostragem")
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  saveRDS(post, file.path(pasta, "piloto_reduzido_50000.rds"))
}
