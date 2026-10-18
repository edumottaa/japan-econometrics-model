.libPaths(c("C:/Users/dudup/OneDrive/japan-econometrics-model/software/bsvarSIGNs-3.0-forecast-fix/library", "C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
library(bsvarSIGNs)
library(bsvars)
library(tidyverse)
library(fpp3)

# Nove variáveis: quatro externas e cinco domésticas, sem bolsa.
# Mu = delta = 1 e lambda = 0,2 fixos; psi fixo após calibração; COVID estimada.
# COVID nativa: permite choques maiores desde março de 2020; não é dummy 0/1 na média.
# Foreign impede que as variáveis domésticas afetem as equações externas.
estimar_bvar_exogeno_covid <- function(dados, p = 6, S = 2500) {
  dados <- dados |> as_tibble() |> arrange(date)
  externas <- c("ust2y", "ip_mundial", "brent", "vix")
  domesticas <- c("atividade", "cpi_core", "juro_curto", "inclinacao", "usdjpy")
  Y <- dados |> select(all_of(c(externas, domesticas))) |> as.matrix()
  stopifnot(all(is.finite(Y)), p >= 1, S >= 50)

  # A data precisa ser contada depois das p observações iniciais usadas nos lags.
  inicio_covid <- match(as.Date("2020-03-01"), as.Date(dados$date)) - p
  if (is.na(inicio_covid) || inicio_covid <= 0) inicio_covid <- NULL
  # Estimar o perfil COVID so com quatro meses observados desde marco/2020.
  if (!is.null(inicio_covid) && nrow(dados) - p - inicio_covid + 1L < 4L) inicio_covid <- NULL

  set.seed(123)
  especificacao <- specify_bsvarSIGN$new(
    data = Y[, domesticas], foreign = Y[, externas], p = p,
    hyper_mu = FALSE, hyper_delta = FALSE,
    hyper_lambda = FALSE, hyper_psi = FALSE,
    hyper_covid = inicio_covid,
    # Etapa de previsão reduzida, sem impor orientação ou sinais econômicos aos choques.
    sign_structural = matrix(NA_real_, 9, 9)
  )
  if (!is.null(inicio_covid)) especificacao$estimate_hyper(S = 2 * S, burn_in = S)
  posterior <- bsvars::estimate(especificacao, S = S, show_progress = FALSE)

  # Mesma checagem do experimental, ajustada de dez para nove variáveis.
  coeficientes <- matrix(posterior$posterior$A, ncol = S)
  lags_domesticos <- unlist(lapply(0:(p - 1), function(j) 5:9 + 9 * j))
  diagnostico <- tibble(
    sorteios = S, ess_ponderacao = posterior$posterior$ess,
    conjuntos_distintos = ncol(unique(coeficientes, MARGIN = 2)),
    valores_validos = all(is.finite(coeficientes)) && all(is.finite(posterior$posterior$Sigma)),
    coef_dom_nas_externas = max(abs(posterior$posterior$A[1:4, lags_domesticos, ])),
    impacto_dom_nas_externas = max(abs(posterior$posterior$Theta0[1:4, 5:9, ]))
  )
  if (!diagnostico$valores_validos || !is.finite(diagnostico$ess_ponderacao) ||
      diagnostico$ess_ponderacao < 50 || diagnostico$conjuntos_distintos < 50 ||
      diagnostico$coef_dom_nas_externas > 1e-8 || diagnostico$impacto_dom_nas_externas > 1e-8) {
    stop("O ajuste nao passou pela checagem numerica; nao interpretar as previsoes.")
  }
  list(posterior = posterior, dados_treino = Y,
       hiperparametros = t(especificacao$prior$hyper), diagnostico = diagnostico)
}

# Para estimar depois, usando as funções nativas do pacote:
# ajuste_exogeno_covid <- estimar_bvar_exogeno_covid(data_bal, p = 6)
# Para três defasagens, trocar apenas p = 3.
