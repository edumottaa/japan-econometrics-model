# Executar após reestimar o modelo estrutural atualizado.
# Executar depois do modelo estrutural, com post_boj, matrix_externa e matrix_interna na sessão.
library(bsvars)
library(bsvarSIGNs)

# 6. Respostas ao choque de aperto monetário do Fed: impacto e 24 meses seguintes.
# Genérico nativo do bsvars, com o método do bsvarSIGNs; não usa forecast().
irfs_fed <- bsvars::compute_impulse_responses(post_boj, horizon = 24)

# Mediana e intervalo de credibilidade de 90%, somente para o choque 3 (política do Fed).
# Aperto monetário do Fed; respostas japonesas não são restringidas.
bandas_fed <- apply(irfs_fed[, 3, , ], c(1, 2), quantile, probs = c(0.05, 0.5, 0.95))
nomes_variaveis <- c(colnames(matrix_externa), colnames(matrix_interna))
png("C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/IRF (Bvar estrutural - bvarsigns)/IRF choque politica Fed.png",
    width = 1800, height = 1600, res = 180)
par_fed <- par(mfrow = c(3, 3))
for (v in seq_along(nomes_variaveis)) {
  matplot(0:24, t(bandas_fed[, v, ]), type = "l", lty = c(2, 1, 2), col = "black",
          main = nomes_variaveis[v], xlab = "Meses após o choque do Fed", ylab = "Resposta")
  abline(h = 0, col = "grey")
}
par(par_fed)
dev.off()
