args <- commandArgs(trailingOnly = TRUE)
source("C:/Users/dudup/OneDrive/japan-econometrics-model/src/evaluation/cv_covid_nativo/comum.R")
out <- args[1]; etapa <- as.integer(args[2]); lag <- as.integer(args[3])
dados <- readRDS(file.path(out, "dados.rds")); origens <- readRDS(file.path(out, "origens.rds"))
caminhos <- readRDS(file.path(out, "caminhos_externos.rds"))
selecionados <- modelos |> filter(.data$etapa == .env$etapa, .data$p == .env$lag)
if (length(args) >= 5) selecionados <- selecionados |> filter(.data$modelo == args[5])
janelas <- if (length(args) >= 4) as.integer(strsplit(args[4], ",", fixed = TRUE)[[1]]) else seq_len(nrow(origens))

for (m in seq_len(nrow(selecionados))) for (i in janelas) {
  config <- selecionados[m, ]; nome <- config$modelo
  arquivo <- file.path(out, "windows", sprintf("%s_w%03d.rds", nome, i))
  if (file.exists(arquivo)) next
  # Antes da COVID, as especificacoes foreign sao identicas: reutilizar a mesma previsao nativa.
  if (config$familia == "foreign" && as.Date(origens$origem[i]) < as.Date("2020-03-01")) {
    base <- file.path(out, "windows", sprintf("foreign_p%d_covid_w%03d.rds", lag, i))
    if (base != arquivo && file.exists(base)) {
      r <- readRDS(base)
      if (is.null(r$erro) && is.null(r$tratamento$inicio_covid) && is.null(r$tratamento$colunas_dummy)) {
        r$pontuacoes$.model <- nome; r$config <- config
        r$reutilizado_de <- basename(base); r$segundos_original <- r$segundos; r$segundos <- 0
        salvar_janela(r, arquivo)
        cat(nome, "janela", i, "identica a", basename(base), "\n"); flush.console()
        next
      }
    }
  }
  status(out, nome, estado = "rodando", janela = i, etapa = etapa, lag = lag, pid = Sys.getpid())
  cat(format(Sys.time()), nome, "INICIO", i, "/47\n"); flush.console()
  treino <- dados |> slice_head(n = origens$n[i]); teste <- dados |>
    filter(date > origens$origem[i], date <= origens$origem[i] + H)
  Y <- as.matrix(select(as_tibble(treino), all_of(variaveis)))
  inicio <- Sys.time(); avisos <- character()
  resultado <- tryCatch(withCallingHandlers({
    set.seed(123 + i)
    if (config$familia == "foreign") {
      covid <- if (config$covid) match(as.Date("2020-03-01"), as.Date(treino$date)) - lag else NULL
      if (length(covid) && (is.na(covid) || covid <= 0)) covid <- NULL
      # Quatro parametros COVID: aguardar os tres meses iniciais e um mes de decaimento observado.
      meses_covid <- nrow(treino) - match(as.Date("2020-03-01"), as.Date(treino$date)) + 1L
      if (!is.null(covid) && meses_covid < 4L) covid <- NULL
      D <- if (config$dummies) dummies_covid(treino$date) else NULL
      spec <- specify_bsvarSIGN$new(data = Y[, domesticas], foreign = Y[, externas], p = lag,
        exogenous = D, hyper_mu = FALSE, hyper_delta = FALSE, hyper_lambda = FALSE,
        hyper_psi = FALSE, hyper_covid = covid, sign_structural = matrix(NA_real_, 9, 9))
      if (!is.null(covid)) spec$estimate_hyper(S = 5000, burn_in = 2500)
      hyper <- t(spec$prior$hyper)
      ess_hyper <- if (!is.null(covid)) min(coda::effectiveSize(coda::mcmc(hyper[, 13:16, drop = FALSE]))) else NA_real_
      post <- bsvars::estimate(spec, S = S, show_progress = FALSE)
      lags_domesticos <- unlist(lapply(0:(lag - 1), function(j) 5:9 + 9 * j))
      diagnostico <- tibble(ess_pesos = as.numeric(post$posterior$ess), ess_hyper = ess_hyper,
        distintos = ncol(unique(matrix(post$posterior$A, ncol = S), MARGIN = 2)),
        zero_lags = max(abs(post$posterior$A[1:4, lags_domesticos, ])),
        zero_impacto = max(abs(post$posterior$Theta0[1:4, 5:9, ])))
      diagnostico$valido <- with(diagnostico, is.finite(ess_pesos) && ess_pesos >= 50 &&
        distintos >= 50 && zero_lags < 1e-8 && zero_impacto < 1e-8)
      set.seed(200000 + i)
      futuro_D <- if (is.null(D)) NULL else matrix(0, H, ncol(D), dimnames = list(NULL, colnames(D)))
      stopifnot(as.character(packageVersion("bsvarSIGNs")) == "3.0.0.1")
      F <- bsvarSIGNs::forecast(post, horizon = H, exogenous_forecast = futuro_D)$forecasts
      tratamento <- list(inicio_covid = covid, colunas_dummy = colnames(D), mu = 1, delta = 1, lambda = 0.2,
        meses_covid_observados = if (is.na(meses_covid)) 0L else meses_covid,
        covid_adiada = config$covid && !is.na(meses_covid) && meses_covid < 4L)
    } else {
      priors <- BVAR::bv_priors(hyper = c("lambda", "soc", "sur"),
        mn = BVAR::bv_minnesota(lambda = BVAR::bv_lambda(), psi = BVAR::bv_psi()),
        soc = BVAR::bv_soc(), sur = BVAR::bv_sur(min = 0.001))
      mh <- BVAR::bv_mh(adjust_acc = TRUE, acc_lower = 0.2, acc_upper = 0.4, acc_change = 0.1)
      post <- BVAR::bvar(Y, lags = lag, n_draw = 5000, n_burn = 2500,
        n_thin = 1, priors = priors, mh = mh, verbose = FALSE)
      hyper <- post$hyper
      diagnostico <- tibble(ess_pesos = NA_real_, ess_hyper = min(coda::effectiveSize(coda::mcmc(hyper))),
        distintos = min(apply(hyper, 2, function(x) length(unique(x)))),
        zero_lags = NA_real_, zero_impacto = NA_real_)
      diagnostico$valido <- diagnostico$distintos > 5 && is.finite(diagnostico$ess_hyper) && diagnostico$ess_hyper >= 50
      set.seed(200000 + i)
      fc <- stats::predict(post, horizon = H, cond_path = caminhos[[i]], cond_vars = colnames(caminhos[[i]]))
      F <- aperm(fc$fcast, c(3, 2, 1))
      tratamento <- list(covid = "sem tratamento direto, conforme decisao do usuario", caminhos = caminhos[[i]])
    }
    pontuacoes <- avaliar_sorteios(F, treino, teste, i, nome)
    stopifnot(all(is.finite(pontuacoes$CRPS)), all(is.finite(pontuacoes$previsao)))
    list(pontuacoes = pontuacoes, sorteios = F[5:9, , , drop = FALSE], variaveis = domesticas,
      posterior = post, versao_bsvarSIGNs = as.character(packageVersion("bsvarSIGNs")),
      diagnostico = diagnostico, hiperparametros = hyper, tratamento = tratamento, erro = NULL)
  }, warning = function(w) { avisos <<- c(avisos, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) list(erro = conditionMessage(e)))
  resultado$avisos <- unique(avisos); resultado$config <- config; resultado$janela <- i
  resultado$origem <- origens$origem[i]; resultado$seed <- 123 + i
  resultado$segundos <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))
  salvar_janela(resultado, arquivo)
  cat(format(Sys.time()), nome, "FIM", i, "erro:", if (is.null(resultado$erro)) "nenhum" else resultado$erro, "\n"); flush.console()
  status(out, nome, estado = "janela_salva", janela = i, etapa = etapa, lag = lag, pid = Sys.getpid())
  rm(resultado); if (exists("post", inherits = FALSE)) rm(post); if (exists("F", inherits = FALSE)) rm(F); gc(verbose = FALSE)
}