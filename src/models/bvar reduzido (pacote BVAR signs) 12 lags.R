# 5. Construção das matrizes dos blocos externo e doméstico

library(tidyverse)
library(bsvars)
library(bsvarSIGNs)

matrix_externa <- data_bal |>
  as_tibble() |>
  select(ust2y, ip_mundial, brent, vix) |>
  as.matrix()

matrix_interna <- data_bal |>
  as_tibble() |>
  select(atividade, cpi_core, juro_curto, inclinacao, usdjpy, topix) |>
  as.matrix()

# 6. Especificação do BVAR
# foreign acrescenta o bloco externo e restringe a influência das defasagens
# domésticas nas equações externas. Não duplicar as externas em data.
# Estimar apenas lambda; manter mu = delta = 1 e psi nos valores automáticos.
spec = specify_bsvarSIGN$new(
  data = matrix_interna,
  foreign = matrix_externa,
  p = 12,
  hyper_mu = FALSE,
  hyper_delta = FALSE,
  hyper_psi = FALSE
)

# 7. Estimação e inspeção dos sorteios dos hiperparâmetros
set.seed(123)
spec$estimate_hyper(S = 5000, burn_in = 2500)

dim(spec$prior$hyper)

# Cada coluna de prior$hyper é um sorteio; a terceira linha é lambda.
# Mu, delta e psi permanecem fixos; COVID não está ativado.
hyper_draws = t(spec$prior$hyper[3, , drop = FALSE])
colnames(hyper_draws) = "lambda"

# Exibir lambda, o único hiperparâmetro estimado, na aba Plots do RStudio.
graphics.off()
options(device = "RStudioGD")
# Lambda é uma única série: usar margens de um gráfico simples.
par(mfrow = c(1, 1), mar = c(3, 3, 2, 0.5), oma = c(0, 0, 0, 0))
plot(hyper_draws[, "lambda"], type = "l", col = "#F500BD", bty = "n",
     xlab = "Iterações", ylab = "lambda", main = "Minnesota (lambda)",
     cex.axis = 0.8, cex.lab = 0.8, cex.main = 0.9)

# 8. Estimação dos parâmetros do BVAR
set.seed(123)
post = estimate(spec, S = 200)

# 8.1. Checagem simples dos resultados da estimação
# Cada coluna reúne todos os coeficientes de um sorteio, de todas as equações.
coeficientes_sorteados <- matrix(
  post$posterior$A,
  ncol = dim(post$posterior$A)[3]
)

diagnostico_estimacao <- tibble(
  sorteios = ncol(coeficientes_sorteados),
  valores_validos = all(is.finite(coeficientes_sorteados)) &&
    all(is.finite(post$posterior$Sigma)),
  conjuntos_distintos = ncol(unique(coeficientes_sorteados, MARGIN = 2))
)

diagnostico_estimacao

# valores_validos = FALSE: há NA, NaN ou infinito nos resultados.
# conjuntos_distintos = 1: todos os sorteios têm os mesmos coeficientes.
# Ter vários conjuntos é necessário, mas não garante qualidade da estimação.
