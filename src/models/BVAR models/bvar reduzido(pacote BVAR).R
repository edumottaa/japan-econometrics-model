library(BVAR)


# Preparar a entrada numérica do BVAR usando a amostra comum, já sem NAs.
dados_modelo <- data_bal |>
  as_tibble() |>
  select(-date)

# Configurar a prior Minnesota
minnesota <- bv_minnesota(
  lambda = bv_lambda(),
  psi = bv_psi()
)

# Escolher quais hiperparâmetros serão estimados
priors <- bv_priors(
  hyper = c("lambda", "psi", "soc", "sur"),
  mn = minnesota,
  soc = bv_soc(),
  # Limite inferior de delta: evitar a instabilidade numérica observada no CV.
  sur = bv_sur(min = 0.001)
)

# Estimar o BVAR e seus hiperparâmetros
set.seed(123)

modelo_bvar <- bvar(
  data = dados_modelo,
  lags = 12,
  n_draw = 5000,
  n_burn = 2500,
  priors = priors
)

summary(modelo_bvar)
# Trajetórias dos sorteios dos hiperparâmetros após o aquecimento.
# lambda e psi: Minnesota; soc: mu; sur: delta. alpha permanece fixo.
# No pacote BVAR, cada linha de hyper é um sorteio; não precisa transpor.
hyper_draws <- modelo_bvar$hyper
plot.ts(hyper_draws, nc = 2, col = "#F500BD", bty = "n", xlab = "Iterações")