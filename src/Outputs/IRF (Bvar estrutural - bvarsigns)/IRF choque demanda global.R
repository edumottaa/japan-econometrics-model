# Executar após reestimar o modelo estrutural atualizado.
# Executar depois do modelo estrutural, com post_boj, matrix_externa e matrix_interna na sessão.
library(bsvars)
library(bsvarSIGNs)

# 6. Respostas ao choque positivo de demanda global: impacto e 24 meses seguintes.
# Genérico nativo do bsvars, com o método do bsvarSIGNs; não usa forecast().
irfs_demanda_global <- bsvars::compute_impulse_responses(post_boj, horizon = 24)

# Mediana e intervalo de credibilidade de 90%, somente para o choque 1 (demanda global).
# Choque positivo de demanda global; respostas japonesas não são restringidas.
bandas_demanda_global <- apply(irfs_demanda_global[, 1, , ], c(1, 2), quantile, probs = c(0.05, 0.5, 0.95))
nomes_variaveis <- c(colnames(matrix_externa), colnames(matrix_interna))
png("C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/IRF (Bvar estrutural - bvarsigns)/IRF choque demanda global.png",
    width = 1800, height = 1600, res = 180)
par_demanda_global <- par(mfrow = c(3, 3))
for (v in seq_along(nomes_variaveis)) {
  matplot(0:24, t(bandas_demanda_global[, v, ]), type = "l", lty = c(2, 1, 2), col = "black",
          main = nomes_variaveis[v], xlab = "Meses após o choque de demanda global", ylab = "Resposta")
  abline(h = 0, col = "grey")
}
par(par_demanda_global)
dev.off()
