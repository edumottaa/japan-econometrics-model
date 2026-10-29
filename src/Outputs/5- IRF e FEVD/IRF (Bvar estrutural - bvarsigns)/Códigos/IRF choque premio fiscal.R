# Executar após reestimar o modelo estrutural atualizado.
# Executar depois do modelo estrutural, com post_boj, matrix_externa e matrix_interna na sessão.
library(bsvars)
library(bsvarSIGNs)

stopifnot("Reestime o modelo estrutural de sete variáveis antes das IRFs." =
            dim(post_boj$posterior$A)[1] == 7L, ncol(matrix_externa) == 2L, ncol(matrix_interna) == 5L)

# 6. Respostas ao choque de prêmio fiscal/de prazo: impacto e 24 meses seguintes.
# Genérico nativo do bsvars, com o método do bsvarSIGNs; não usa forecast().
irfs_premio_fiscal <- bsvars::compute_impulse_responses(post_boj, horizon = 24)

# Mediana e intervalo de credibilidade de 90%, somente para o choque 6 (prêmio fiscal/de prazo).
# Aumento do prêmio fiscal/de prazo; a magnitude é determinada pelo modelo.
bandas_premio_fiscal <- apply(irfs_premio_fiscal[, 6, , ], c(1, 2), quantile, probs = c(0.05, 0.5, 0.95))
nomes_variaveis <- c(colnames(matrix_externa), colnames(matrix_interna))
png("C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/IRF e FEVD/IRF (Bvar estrutural - bvarsigns)/IRF choque premio fiscal.png",
    width = 1800, height = 1600, res = 180)
par_premio_fiscal <- par(mfrow = c(3, 3))
for (v in seq_along(nomes_variaveis)) {
  matplot(0:24, t(bandas_premio_fiscal[, v, ]), type = "l", lty = c(2, 1, 2), col = "black",
          main = nomes_variaveis[v], xlab = "Meses após o choque de prêmio", ylab = "Resposta")
  abline(h = 0, col = "grey")
}
par(par_premio_fiscal)
dev.off()
