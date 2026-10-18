library(bsvarSIGNs)
library(bsvars)
library(tidyverse)
library(fpp3)

# BVAR experimental: mesmas dez variáveis, duas defasagens e exogeneidade de bloco.
# foreign: o Japão não entra nas equações externas, nem pelos lags nem no impacto.
# Lambda = 0,2 e mu = delta = 1 (padrões do pacote); psi automático em cada treino.
# Nenhum hiperparâmetro é amostrado nesta versão; nenhuma restrição econômica de sinal.
# Duas defasagens não limitam a previsão a dois meses: o sistema prevê recursivamente.
estimar_bvar_exogeno <- function(dados, p = 2, S = 2000) {
  externas <- c("ust2y", "ip_mundial", "brent", "vix")
  domesticas <- c("atividade", "cpi_core", "juro_curto", "inclinacao", "usdjpy", "topix")
  Y <- dados |> as_tibble() |> select(all_of(c(externas, domesticas))) |> as.matrix()
  stopifnot(all(is.finite(Y)))

  set.seed(123)
  especificacao <- specify_bsvarSIGN$new(
    data = Y[, domesticas], foreign = Y[, externas], p = p,
    hyper_mu = FALSE, hyper_delta = FALSE,
    hyper_lambda = FALSE, hyper_psi = FALSE
  )
  posterior <- bsvars::estimate(especificacao, S = S, show_progress = FALSE)

  # Verificar diversidade e tamanho efetivo da reamostragem, sem gráficos de milhares de coeficientes.
  coeficientes <- matrix(posterior$posterior$A, ncol = S)
  lags_domesticos <- unlist(lapply(0:(p - 1), function(j) 5:10 + 10 * j))
  diagnostico <- tibble(
    sorteios = S, ess_ponderacao = posterior$posterior$ess,
    conjuntos_distintos = ncol(unique(coeficientes, MARGIN = 2)),
    valores_validos = all(is.finite(coeficientes)) && all(is.finite(posterior$posterior$Sigma)),
    coef_dom_nas_externas = max(abs(posterior$posterior$A[1:4, lags_domesticos, ])),
    impacto_dom_nas_externas = max(abs(posterior$posterior$Theta0[1:4, 5:10, ]))
  )
  if (!diagnostico$valores_validos || !is.finite(diagnostico$ess_ponderacao) ||
      diagnostico$ess_ponderacao < 50 || diagnostico$conjuntos_distintos < 50 ||
      diagnostico$coef_dom_nas_externas > 1e-8 || diagnostico$impacto_dom_nas_externas > 1e-8) {
    stop("O ajuste não passou pela checagem numérica; não interpretar as previsões.")
  }
  list(posterior = posterior, dados_treino = Y, diagnostico = diagnostico)
}

# Para estimar depois na amostra completa:
# ajuste_exogeno <- estimar_bvar_exogeno(data_bal)
