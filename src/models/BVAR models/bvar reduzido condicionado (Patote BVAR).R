library(BVAR)
library(fpp3)


# Preparar a entrada numérica do BVAR usando a amostra comum, já sem NAs.
dados_modelo <- data_bal |>
  as_tibble() |>
  select(-date)

# Configurar a prior Minnesota
minnesota <- bv_minnesota(
  lambda = bv_lambda(),
  psi = bv_psi()
)

# Estimar lambda, SOC e SUR; psi automático e fixo na estimação.
priors <- bv_priors(
  hyper = c("lambda", "soc", "sur"),
  mn = minnesota,
  soc = bv_soc(),
  # Limite inferior de delta: evitar a instabilidade numérica observada no CV.
  sur = bv_sur(min = 0.001)
)

# Ajustar a escala das propostas durante o aquecimento para melhorar a aceitação.
# A faixa de 20%-40% é uma meta de ajuste, não um diagnóstico de convergência.
mh <- bv_mh(
  adjust_acc = TRUE,
  acc_lower = 0.2,
  acc_upper = 0.4,
  acc_change = 0.1
)

# Estimar o BVAR e seus hiperparâmetros
set.seed(123)

modelo_bvar <- bvar(
  data = dados_modelo,
  lags = 6,
  n_draw = 5000,
  n_burn = 2500,
  priors = priors,
  mh = mh
)

summary(modelo_bvar)
# Trajetórias dos sorteios dos hiperparâmetros após o aquecimento.
# lambda: Minnesota; soc: mu; sur: delta. alpha e psi permanecem fixos.
# No pacote BVAR, cada linha de hyper é um sorteio; não precisa transpor.
# Reiniciar os dispositivos e exibir o gráfico na aba Plots do RStudio.
graphics.off()
options(device = "RStudioGD")
hyper_draws <- modelo_bvar$hyper
# Aperto global e priors SOC (mu) e SUR (delta).
plot.ts(hyper_draws[, c("lambda", "soc", "sur"), drop = FALSE],
        nc = 2, col = "#F500BD", bty = "n", xlab = "Iterações",
        main = "Minnesota (lambda), SOC (mu) e SUR (delta)",
        mar.multi = c(0, 4, 0, 1), oma.multi = c(4, 0, 3, 0))

# 2. Previsões do VAR externo selecionado por AICc (VAR(2)).
# Executar antes o script "var externo simples.R", que estima var_ex em data_bal.
# As previsões mantêm as mesmas unidades utilizadas na estimação do BVAR.
h <- 24

previsoes_externas <- var_ex |>
  select(aicc) |>
  forecast(h = h)

previsoes_externas


# 3. Matriz das trajetórias médias externas: uma linha por mês, uma coluna por variável.
# A coluna .mean do forecast multivariado já é uma matriz com os nomes das variáveis.
# Manter as unidades do modelo; não aplicar log novamente.
# Nesta primeira versão, usamos médias fixas, sem transmitir a incerteza do VAR externo.
caminhos_externos <- previsoes_externas |>
  as_tibble() |>
  arrange(date) |>
  pull(.mean)

caminhos_externos


# 4. Previsão do BVAR condicionada às trajetórias médias do VAR externo.
# cond_vars associa cada coluna da matriz à variável correspondente pelo nome.
# As seis variáveis domésticas ficam livres, condicionadas às quatro trajetórias externas.
# O modelo_bvar já estimado é reutilizado; seus coeficientes não são reestimados.
set.seed(123)

previsao_bvar_condicionada <- predict(
  modelo_bvar,
  horizon = h,
  cond_path = caminhos_externos,
  cond_vars = colnames(caminhos_externos)
)

summary(previsao_bvar_condicionada)


# 5. Comparar as médias previstas pelo BVAR com e sem condicionamento.
# Reutilizar o mesmo modelo estimado e o mesmo horizonte.
set.seed(123)
previsao_bvar_incondicional <- predict(modelo_bvar, horizon = h)

# fcast tem dimensões: sorteios x horizontes x variáveis.
medias_incondicionais <- apply(previsao_bvar_incondicional$fcast, c(2, 3), mean)
medias_condicionais <- apply(previsao_bvar_condicionada$fcast, c(2, 3), mean)
colnames(medias_incondicionais) <- colnames(medias_condicionais) <- modelo_bvar$variables

comparacao_bvar <- bind_rows(
  as_tibble(medias_incondicionais) |> mutate(cenario = "Incondicional"),
  as_tibble(medias_condicionais) |> mutate(cenario = "Condicionada")
) |>
  mutate(date = rep(previsoes_externas$date, 2)) |>
  pivot_longer(all_of(modelo_bvar$variables),
               names_to = "variavel", values_to = "previsao")

# Comparação de médias, sem bandas; CPI e demais índices continuam em 100 * log.
grafico_comparacao_bvar <- comparacao_bvar |>
  ggplot(aes(x = as.Date(date), y = previsao, colour = cenario)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ variavel, scales = "free_y", ncol = 3) +
  scale_colour_manual(values = c(Incondicional = "#0072B2", Condicionada = "#D55E00")) +
  labs(title = "BVAR: efeito do condicionamento ao VAR externo",
       subtitle = "Médias previstas nas unidades utilizadas pelo modelo",
       x = NULL, y = NULL, colour = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(grafico_comparacao_bvar)

