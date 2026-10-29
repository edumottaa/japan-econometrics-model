Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(TZ = "America/Sao_Paulo")
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
source("src/chat gpt/premissas/0- funcoes forecasts.R", encoding = "UTF-8")
source("src/chat gpt/premissas/5- amostrar prior tendencia.R", encoding = "UTF-8")
args <- commandArgs(trailingOnly = TRUE)
p <- if (length(args)) as.integer(args[1]) else 6L
stopifnot(p %in% c(6L, 12L))
centro_inflacao <- 2
desvio_inflacao <- .5
ref <- referencia(p)
dados <- ref$ajuste$dados_modelo
cfg <- ref$ajuste$configuracao
N <- length(ref$ajuste$variaveis)
pasta_modelo <- file.path(raiz_modelos, paste0("lag", p))
dir.create(pasta_modelo, recursive = TRUE, showWarnings = FALSE)
arquivo <- file.path(pasta_modelo, "modelo_tendencia.rds")
arquivo_propostas <- if (length(args) >= 2L) args[2] else file.path(pasta_modelo, "modelo_propostas.rds")

# Tendencia em anos, centrada no ultimo mes; CPI permanece em 100 * log(nivel).
if (file.exists(arquivo_propostas)) {
  ajuste <- readRDS(arquivo_propostas)
  stopifnot(identical(ajuste$dados_modelo, dados), identical(ajuste$configuracao, cfg))
} else if (file.exists(arquivo)) {
  ajuste <- readRDS(arquivo)
  stopifnot(identical(ajuste$dados_modelo, dados), identical(ajuste$configuracao, cfg))
} else {
  gravar_status(pasta_modelo, "estimando tendencia")
  preparado <- preparar_spec(dados, cfg, estrutural = TRUE)
  Y <- preparado$Y
  sinais <- ref$ajuste$post$last_draw$identification$sign_irf
  eventos <- tibble(date = yearmonth(c("2024 Mar", "2025 Oct")), choque = c(5L, 6L)) |>
    mutate(inicio = match(date, dados$date))
  narrativas <- lapply(seq_len(nrow(eventos)), function(k) specify_narrative(
    start = eventos$inicio[k], periods = 1, type = "S", sign = 1, shock = eventos$choque[k]))
  tendencia <- matrix((seq_len(nrow(Y)) - nrow(Y)) / 12, ncol = 1)
  spec <- specify_bsvarSIGN$new(data = Y[, 3:N, drop = FALSE], foreign = Y[, 1:2, drop = FALSE],
    p = p, exogenous = tendencia, hyper_mu = FALSE, hyper_delta = FALSE,
    hyper_lambda = FALSE, hyper_psi = FALSE, hyper_covid = preparado$covid,
    sign_irf = sinais, sign_narrative = narrativas,
    sign_structural = matrix(NA_real_, N, N), max_tries = 1000L)
  spec$prior$hyper[1:3, ] <- c(cfg$mu, cfg$delta, cfg$lambda)
  set.seed(123)
  if (!is.null(preparado$covid)) spec$estimate_hyper(S = 5000, burn_in = 2500)
  post <- bsvars::estimate(spec, S = 2500, show_progress = TRUE)
  ajuste <- list(post = post, dados_modelo = dados, configuracao = cfg,
    variaveis = ref$ajuste$variaveis, tendencia = tendencia)
  saveRDS(ajuste, arquivo)
}
diagnosticar(ajuste$post, p, N)

# Crescimento domestico de longo prazo, com externas estabilizadas.
post <- ajuste$post
S <- dim(post$posterior$A)[3]
pesos_nativos <- if (is.null(ajuste$pesos_propostas)) rep(1, S) else ajuste$pesos_propostas
stopifnot(length(pesos_nativos) == S, all(is.finite(pesos_nativos)), all(pesos_nativos > 0))
# No bsvarSIGNs 3.0, estes dois campos nao acompanham o resampling nativo.
if (is.null(ajuste$pesos_propostas)) for (s in seq_len(S)) {
  X <- post$last_draw$data_matrices$X
  Y <- post$last_draw$data_matrices$Y
  escala <- escala_covid(post$posterior$hyper[, s], post$last_draw$prior$covid, ncol(Y), N)
  post$posterior$sigma[, , s] <- matrix(rep(escala, each = N), N)
  post$posterior$shocks[, , s] <- post$posterior$B[, , s] %*%
    sweep(Y - post$posterior$A[, , s] %*% X, 2, escala, "/")
}
raizes <- inflacao_lp <- rep(NA_real_, S)
chaves <- character(S)
for (s in seq_len(S)) {
  A <- post$posterior$A[, , s]
  colunas_domesticas <- unlist(lapply(0:(p - 1L), function(j) 3:N + N * j))
  lags <- A[3:N, colunas_domesticas, drop = FALSE]
  Nd <- N - 2L
  companheira <- rbind(lags, cbind(diag(Nd * (p - 1L)), matrix(0, Nd * (p - 1L), Nd)))
  raizes[s] <- max(Mod(eigen(companheira, only.values = TRUE)$values))
  soma <- Reduce(`+`, lapply(seq_len(p), function(j) A[3:N, N * (j - 1L) + 3:N, drop = FALSE]))
  if (raizes[s] < 1 && rcond(diag(Nd) - soma) > 1e-8) {
    crescimento <- solve(diag(Nd) - soma, A[3:N, N * p + 2L])
    inflacao_lp[s] <- 100 * expm1(crescimento[2] / 100)
  }
  chaves[s] <- digest::digest(A, algo = "xxhash64")
}
estaveis <- is.finite(inflacao_lp)
stopifnot(sum(estaveis) >= 50L)
gravar_status(pasta_modelo, "reponderando parametros")
rb <- prior_tendencia(post, p, estaveis, chaves, centro_inflacao, desvio_inflacao)
diagnosticos <- list()
caminhos <- ref$resultado$caminhos
preparar_caminhos <- function(dados) caminhos
for (tipo in c("tendencia sem prior adicional", "prior inflacao longo prazo")) {
  pesos <- pesos_nativos * if (tipo == "tendencia sem prior adicional") as.numeric(estaveis) else rb$pesos
  pesos <- pesos / sum(pesos)
  ess <- 1 / sum(pesos^2)
  ess_distintos <- 1 / sum(tapply(pesos, chaves, sum)^2)
  minimo_ess <- if (tipo == "prior inflacao longo prazo") 30 else 50
  media_lp_calculada <- sum(pesos[estaveis] * inflacao_lp[estaveis])
  d <- tibble(tipo = tipo, estaveis = sum(estaveis), ess_pesos = ess,
    ess_parametros_distintos = ess_distintos, centro = centro_inflacao, desvio = desvio_inflacao,
    precisao = if (ess < 50 || ess_distintos < 50) "exploratorio: ESS abaixo de 50" else "checagem ESS 50 atendida",
    media_lp_ponderada = if (tipo == "tendencia sem prior adicional") media_lp_calculada else NA_real_)
  print(d)
  write_csv(d, file.path(pasta_modelo, paste0("checagem_", if (tipo == "tendencia sem prior adicional") "controle" else "prior", ".csv")))
  stopifnot(ess >= minimo_ess, ess_distintos >= minimo_ess)
  indices <- indices_pesos(pesos, min(S, 2500L))
  novo <- ajuste
  novo$post <- post$clone(deep = TRUE)
  for (nome in names(post$posterior)) {
    x <- post$posterior[[nome]]
    if (length(dim(x)) == 3L) novo$post$posterior[[nome]] <- x[, , indices, drop = FALSE]
    if (length(dim(x)) == 2L) novo$post$posterior[[nome]] <- x[, indices, drop = FALSE]
  }
  novo$post$posterior$ess <- ess
  if (tipo == "prior inflacao longo prazo") {
    set.seed(321)
    coeficientes_novos <- novo$post$posterior$A
    choques_novos <- novo$post$posterior$shocks
    X <- novo$post$last_draw$data_matrices$X
    Y <- novo$post$last_draw$data_matrices$Y
    escalas_novas <- novo$post$posterior$sigma
    matrizes_B <- novo$post$posterior$B
    for (s in seq_along(indices)) {
      coeficientes_novos[, N * p + 1:2, s] <- matrix(sortear_tendencia(rb$condicionais[[indices[s]]]), N, 2L)
      escala <- escalas_novas[1, , s]
      choques_novos[, , s] <- matrizes_B[, , s] %*%
        sweep(Y - coeficientes_novos[, , s] %*% X, 2, escala, "/")
    }
    novo$post$posterior$A <- coeficientes_novos
    novo$post$posterior$shocks <- choques_novos
    crescimento_novo <- vapply(seq_along(indices), function(s) {
      A <- novo$post$posterior$A[, , s]
      soma <- Reduce(`+`, lapply(seq_len(p), function(j) A[3:N, N * (j - 1L) + 3:N, drop = FALSE]))
      100 * expm1(solve(diag(N - 2L) - soma, A[3:N, N * p + 2L])[2] / 100)
    }, numeric(1))
    d$media_lp_ponderada <- mean(crescimento_novo)
    print(d, width = Inf)
  }
  if (ess >= 50) diagnosticar(novo$post, p, N) else {
    # A/Sigma/Theta0 e zeros ja foram checados; somente constante/tendencia mudaram.
    stopifnot(all(is.finite(novo$post$posterior$A)))
    message("Forecast exploratorio: ESS ", round(ess, 1), "; minimo autorizado = 30.")
  }
  if (tipo == "prior inflacao longo prazo") {
    for (narrativa in novo$post$last_draw$identification$sign_narrative) {
      meses <- narrativa$start - p + seq_len(narrativa$periods) - 1L
      stopifnot(all(narrativa$sign * novo$post$posterior$shocks[narrativa$shock, meses, ] >= -1e-8))
    }
  }
  novo$prior_adicional <- list(tipo = tipo, centro = centro_inflacao, desvio = desvio_inflacao,
    externas_longo_prazo = "estabilizadas", estabilidade_domestica = TRUE)
  saveRDS(novo, file.path(pasta_modelo, if (tipo == "prior inflacao longo prazo") "ajuste_prior.rds" else "ajuste_controle.rds"))
  pasta <- file.path(raiz_premissas, paste0("lag", p), tipo)
  resultado <- forecast_ajuste(novo, paste0(p, " lags | ", tipo), pasta)
  resultado$prior <- list(tipo = "fator de prior sobre crescimento de tendencia",
    centro = centro_inflacao, desvio = desvio_inflacao, estabilidade = TRUE,
    propostas_positivas = S, ess_pesos = ess, ess_parametros_distintos = ess_distintos,
    lotes = length(ajuste$lotes_propostas),
    escala_prior = "crescimento logaritmico anual", externas_lp = "estabilizadas",
    amostragem = "Rao-Blackwell e condicional normal truncada de constante e tendencia")
  salvar_previsoes(resultado, pasta)
  write_csv(tibble(sorteio = seq_len(S), raiz = raizes, inflacao_lp_original = inflacao_lp, peso = pesos),
    file.path(pasta, "pesos_parametros.csv"))
  if (tipo == "prior inflacao longo prazo") write_csv(tibble(sorteio = seq_along(indices), inflacao_lp = crescimento_novo),
    file.path(pasta, "sorteios_inflacao_lp.csv"))
  write_csv(d, file.path(pasta, "diagnostico_prior.csv"))
  diagnosticos[[tipo]] <- d
}
write_csv(bind_rows(diagnosticos), file.path(pasta_modelo, "diagnosticos_prior.csv"))
gravar_status(pasta_modelo, "concluido")
