if (dim(post_boj$posterior$A)[1] != 9L) {
  stop("Este choque não é identificado no modelo de sete variáveis; script reservado ao modelo antigo de nove.")
}

# Executar após reestimar o modelo estrutural atualizado.
# Executar depois do modelo estrutural, com post_boj, matrix_externa e matrix_interna na sessão.
library(bsvars)
library(bsvarSIGNs)

# 6. Respostas ao choque de risk-off global: impacto e 24 meses seguintes.
# Genérico nativo do bsvars, com o método do bsvarSIGNs; não usa forecast().
irfs_risk_off <- bsvars::compute_impulse_responses(post_boj, horizon = 24)

# Mediana e intervalo de credibilidade de 90%, somente para o choque 4 (risk-off global).
# Risk-off positivo; narrativa de agosto/2024 entra na estimação conjunta.
bandas_risk_off <- apply(irfs_risk_off[, 4, , ], c(1, 2), quantile, probs = c(0.05, 0.5, 0.95))
nomes_variaveis <- c(colnames(matrix_externa), colnames(matrix_interna))
png("C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/IRF (Bvar estrutural - bvarsigns)/IRF choque risk off global.png",
    width = 1800, height = 1600, res = 180)
par_risk_off <- par(mfrow = c(3, 3))
for (v in seq_along(nomes_variaveis)) {
  matplot(0:24, t(bandas_risk_off[, v, ]), type = "l", lty = c(2, 1, 2), col = "black",
          main = nomes_variaveis[v], xlab = "Meses após o choque de risk-off", ylab = "Resposta")
  abline(h = 0, col = "grey")
}
par(par_risk_off)
dev.off()
