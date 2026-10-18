# Previsao recursiva para o bsvarSIGNs 3.0, sem exogenas adicionais ou COVID.
# Usa os sorteios posteriores de A e Sigma e as observacoes finais do treino.
# Corrige a inicializacao e a atualizacao das defasagens dos metodos instalados.
prever_bsvarsigns <- function(post, dados_treino, h) {
  n_variaveis <- ncol(dados_treino)
  p <- post$last_draw$p
  n_sorteios <- dim(post$posterior$A)[3]
  previsoes <- array(NA_real_, c(n_variaveis, h, n_sorteios))

  # Estado inicial: ultimo mes, penultimo, ..., seguido da constante.
  estado_inicial <- c(
    as.vector(t(dados_treino[nrow(dados_treino) - (0:(p - 1)), , drop = FALSE])),
    1
  )

  for (s in seq_len(n_sorteios)) {
    estado <- estado_inicial
    A <- post$posterior$A[, , s]
    L <- t(chol(post$posterior$Sigma[, , s]))

    for (j in seq_len(h)) {
      previsoes[, j, s] <- drop(A %*% estado + L %*% rnorm(n_variaveis))

      # Inserir a previsao mais recente, deslocar os lags e manter a constante.
      estado <- c(
        previsoes[, j, s],
        head(estado, n_variaveis * (p - 1)),
        1
      )
    }
  }

  previsoes
}
