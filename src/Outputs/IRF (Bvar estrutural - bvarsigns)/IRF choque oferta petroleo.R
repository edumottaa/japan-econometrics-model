# Executar após reestimar o modelo estrutural atualizado.
# Executar depois do modelo estrutural, com post_boj, matrix_externa e matrix_interna na sessão.
library(bsvars)
library(bsvarSIGNs)

# 6. Respostas ao choque adverso de oferta de petróleo: impacto e 24 meses seguintes.
# Genérico nativo do bsvars, com o método do bsvarSIGNs; não usa forecast().
irfs_petroleo <- bsvars::compute_impulse_responses(post_boj, horizon = 24)

# Mediana e intervalo de credibilidade de 90%, somente para o choque 2 (oferta de petróleo).
# Choque adverso de oferta de petróleo; respostas japonesas não são restringidas.
bandas_petroleo <- apply(irfs_petroleo[, 2, , ], c(1, 2), quantile, probs = c(0.05, 0.5, 0.95))
nomes_variaveis <- c(colnames(matrix_externa), colnames(matrix_interna))
png("C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/IRF (Bvar estrutural - bvarsigns)/IRF choque oferta petroleo.png",
    width = 1800, height = 1600, res = 180)
par_petroleo <- par(mfrow = c(3, 3))
for (v in seq_along(nomes_variaveis)) {
  matplot(0:24, t(bandas_petroleo[, v, ]), type = "l", lty = c(2, 1, 2), col = "black",
          main = nomes_variaveis[v], xlab = "Meses após o choque de petróleo", ylab = "Resposta")
  abline(h = 0, col = "grey")
}
par(par_petroleo)
dev.off()
