Sys.setlocale("LC_CTYPE", "English_United States.utf8")
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
source("src/chat gpt/premissas/0- funcoes forecasts.R", encoding = "UTF-8")
args <- commandArgs(trailingOnly = TRUE)
lags <- if (length(args)) as.integer(args[1]) else 6L
stopifnot(all(lags %in% c(6L, 12L)))
for (p in lags) {
  ref <- referencia(p)
  pasta <- file.path(raiz_premissas, paste0("lag", p))
  dez <- readRDS(file.path(pasta, "juro dezembro 1.50/forecast_bvar.rds"))
  stopifnot(identical(select(dez$caminhos, -juro_curto), select(ref$resultado$caminhos, -juro_curto)),
    all(is.na(filter(dez$caminhos, date %in% yearmonth(c("2026 Oct", "2026 Nov")))$juro_curto)),
    all(is.na(filter(dez$caminhos, date > yearmonth("2026 Dec"))$juro_curto)),
    all(filter(dez$resumo, variavel == "juro_curto", date == yearmonth("2026 Dec"))$media == 1.5))
  for (tipo in c("juro dezembro 1.50", "inflacao suave", "tendencia sem prior adicional", "prior inflacao longo prazo")) {
    resultado <- readRDS(file.path(pasta, tipo, "forecast_bvar.rds"))
    stopifnot(identical(resultado$dados_modelo, ref$resultado$dados_modelo),
      nrow(resultado$resumo) == 7L * 24L * 4L,
      all(is.finite(as.matrix(select(resultado$resumo, media:superior_95)))),
      length(list.files(file.path(pasta, tipo), pattern = "[.]png$")) == 15L)
    for (c in unique(resultado$previsoes$cenario)) for (alvo in c("ust2y", "brent")) {
      prev <- as_tibble(resultado$previsoes) |> filter(cenario == c, variavel == alvo) |> arrange(date)
      condicoes <- resultado$caminhos |> filter(cenario == c) |> arrange(date)
      fixas <- !is.na(condicoes[[alvo]])
      valores <- condicoes[[alvo]][fixas]
      if (alvo == "brent") valores <- exp(valores / 100)
      stopifnot(max(abs(prev$.mean[fixas] - valores)) < 1e-8)
    }
  }
  cat("VALIDADO:", p, "lags; quatro cenarios; mesmas bases e condicoes fixas; dezembro 1,50.\n")
}

# Verificar a escala anual da tendencia e a recursao com uma trajetoria conhecida.
a <- readRDS(file.path(raiz_modelos, "lag6/modelo_tendencia.rds"))
post <- a$post$clone(deep = TRUE)
N <- 7L
p <- 6L
g <- numeric(N)
g[4] <- 100 * log(1.02)
A <- matrix(0, N, N * p + 2L)
A[, seq_len(N)] <- diag(.5, N)
A[, N * p + 1L] <- .5 * g / 12
A[, N * p + 2L] <- .5 * g
post$posterior$A <- array(A, c(N, ncol(A), 1L))
post$posterior$Sigma <- array(diag(1e-12, N), c(N, N, 1L))
post$posterior$hyper <- post$posterior$hyper[, 1, drop = FALSE]
dados <- a$dados_modelo
for (k in seq_len(N)) dados[[a$variaveis[k]]] <- g[k] * (seq_len(nrow(dados)) - nrow(dados)) / 12
condicoes <- matrix(NA_real_, 12, N, dimnames = list(NULL, a$variaveis))
condicoes[, 1:2] <- 0
prev <- prever_bvar(post, dados, condicoes, p)$sorteios
stopifnot(max(abs(prev[4, , 1] - g[4] * seq_len(12) / 12)) < 1e-4,
  abs(100 * expm1(g[4] / 100) - 2) < 1e-10)
cat("VALIDADO: tendencia de 2% ao ano e recursao com exogena.\n")
