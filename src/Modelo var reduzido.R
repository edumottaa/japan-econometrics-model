library(tidyverse)
library(ggplot2)
library(dplyr)
library(fpp3)
library(bsvarSIGNs)




# 4 Initial VAR benchmark ( e sua seleção de lag)
lag_selection <- data_bal |>
  model(
    aicc = VAR(vars(atividade,cpi_core, juro_curto)),
    bic = VAR(vars(atividade,cpi_core, juro_curto), ic = "bic")
  )
lag_selection 

lag_selection |>
  select(aicc) |>
  forecast() |>
  autoplot(data_bal |> filter(year(date) > 2000))




# 5 Build foreign and domestic data matrixes
matrix_externa <- data_bal |>
  as_tibble() |>
  select(ust2y, ip_mundial, brent, vix) |>
  as.matrix()

matrix_interna <- data_bal |>
  as_tibble() |>
  select(atividade, cpi_core, juro_curto, inclinacao, usdjpy, topix) |>
  as.matrix()

# 6 Specify the BVAR
# TODO: Problema: Hiperparametros estimados pelo modelo (gerados pela função estimate_hyper() ) 
#estão dando problema
#              tapa buraco:  hiperparametros fixos no padrao do pacote (lambda
#              = 0.2, mu = 1, delta = 1, psi = variancias da AR)

# TODO: argumento foreign retirado. Ignorou-se a exogenidade do modelo; porém,
#            o peso da endogeidade das váriaveis internas nas externas pode ser 
#              quase nulo com as matrix juntas


spec = specify_bsvarSIGN$new(
  data = cbind(matrix_externa, matrix_interna),   # externas primeiro, depois domesticas
  p    = 12                                       # defasagens do plano
)



#7  Inspect hyperparameter draws  Esse passo foi pulado!
dim(spec$prior$hyper) # 17 parameters: mu, delta, lambda, 10 psi values, and 4 COVID parameters.
#Ta dando erro!!!! 

hyper_draws = t(spec$prior$hyper)
colnames(hyper_draws) = c("mu", "delta", "lambda", paste0("psi", 1:10), paste0("covid", 1:4))

par(mfrow = c(2, 2), mar = c(2, 2, 2, 1))
for (i in 4:7) {
  plot.ts(hyper_draws[, i], main = colnames(hyper_draws)[i], col = "#F500BD", bty = "n", xlab = "")
}

par(mfrow = c(1, 3), mar = c(2, 2, 2, 1))
for (i in 8:10) {
  plot.ts(hyper_draws[, i], main = colnames(hyper_draws)[i], col = "#F500BD", bty = "n", xlab = "")
}

par(mfrow = c(1, 3), mar = c(2, 2, 2, 1))
for (i in 11:13) {
  plot.ts(hyper_draws[, i], main = colnames(hyper_draws)[i], col = "#F500BD", bty = "n", xlab = "")
}


par(mfrow = c(3, 1), mar = c(2, 4, 2, 1))
for (i in 1:3) {
  plot.ts(hyper_draws[, i], main = colnames(hyper_draws)[i], col = "#F500BD", bty = "n", xlab = "")
}

par(mfrow = c(1, 1))





# 7 Estimate VAR parameters
set.seed(123)
post = estimate(spec, S = 1000)







# Cross validation Test, compared to NAIVE and pure Arima
cv_data <- data_bal |>
  stretch_tsibble(.init = 180, .step = 3)   # 1st training sample: Jan/2000 - Dec/2014
                                            #adotour-se step = 3 devido o tempo do treinamento (~1h e 30 com step = 3)

# Fable banchmarks models for CPI
cv_fable <- cv_data |>
  model(
    ar    = ARIMA(cpi_core ~ pdq(d = 1, q = 0) + PDQ(D = 0, Q = 0)),
    naive = NAIVE(cpi_core),
    drift = RW(cpi_core ~ drift())
  )


# Guardar as metricas dos modelos univariados
acc_fable <- cv_fable |>
  fabletools::forecast(h = 12) |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))



# Small VAR (fable)
cv_var <- cv_data |>
  model(var = VAR(vars(atividade, cpi_core, juro_curto)))



# Prever o VAR e sextrair a distribuição marginal do CPI
previsoes_var <- cv_var |>
  fabletools::forecast(h = 12) |>
  filter(date <= max(data_bal$date))

param_var_cv <- distributional::parameters(previsoes_var$.distribution)

previsoes_var_cpi <- tibble(
  .id = previsoes_var$.id,
  .model = previsoes_var$.model,
  date = previsoes_var$date,
  cpi_core = distributional::dist_normal(
    mu = map_dbl(param_var_cv$mu, 2),
    sigma = sqrt(map_dbl(param_var_cv$sigma, ~ .x[2, 2]))
  )
) |>
  as_tsibble(key = c(.id, .model), index = date) |>
  as_fable(response = "cpi_core", distribution = "cpi_core")

acc_var <- previsoes_var_cpi |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))


# CV teste for BVAR: Não tem uma função nos pacotes que automatiza o teste cv 
# para o BVAR, por isso é necessário fazer um código que realize esse teste                   


#1 Organizar dados de aprendizagem e de treino


set.seed(123)

janelas <- unique(cv_data$.id)
resultados_bvar <- vector("list", length(janelas))

for (i in seq_along(janelas)) {
  
  # Dados disponíveis nesta janela
  treino <- cv_data |>
    filter(.id == janelas[i]) |>
    as_tibble() |>
    arrange(date)
  
  origem <- max(treino$date)
  
  # Pular a janela que não tem observações futuras para avaliação
  if (origem >= max(data_bal$date)) next
  
  dados_treino <- treino |>
    select(
      ust2y, ip_mundial, brent, vix,
      atividade, cpi_core, juro_curto,
      inclinacao, usdjpy, topix
    ) |>
    as.matrix()
    
  
  
  
  
#Estimação e previsão do BVAR reduzido
  
  # Estimação com os hiperparâmetros padrão do pacote
  spec_cv <- bsvarSIGNs::specify_bsvarSIGN$new(               
    data = dados_treino,
    p = 12
  )
  
  post_cv <- bsvars::estimate(spec_cv, S = 200)
  
  # Previsão pela função do pacote
  previsao_cv <- bsvarSIGNs::forecast(post_cv, horizon = 12)
  
  # Selecionar os sorteios referentes ao CPI
  pos_cpi <- match("cpi_core", colnames(dados_treino))
  sorteios_cpi <- previsao_cv$forecasts[pos_cpi, , ]
  
  resultados_bvar[[i]] <- tibble(
    .id = janelas[i],
    .model = "BVAR",
    date = origem + seq_len(12),
    h = seq_len(12),
    sorteios = split(sorteios_cpi, row(sorteios_cpi))
  )
}


# adaptação do BVAR ao pacote fable afim de usar a função acurracy()

previsoes_bvar <- bind_rows(resultados_bvar) |>
  mutate(
    cpi_core = distributional::dist_sample(sorteios),
    .mean = mean(cpi_core)
  ) |>
  select(-sorteios) |>
  filter(date <= max(data_bal$date)) |>
  as_tsibble(key = c(.id, .model), index = date) |>
  fabletools::as_fable(
    response = "cpi_core",
    distribution = "cpi_core"
  )



# caçculo da acuracia do BVAR
acc_bvar <- previsoes_bvar |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))



# Comparação das métricas para CPI
cv_results <- bind_rows(
  acc_fable,
  acc_var,
  acc_bvar
) |>
  select(.model, .type, RMSE, MAE, CRPS) |>
  arrange(RMSE)

cv_results





cv_results |>
  select(.model, RMSE, MAE, CRPS) |>
  pivot_longer(-.model, names_to = "metrica", values_to = "valor") |>
  ggplot(aes(x = .model, y = valor, fill = .model)) +
  geom_col(na.rm = TRUE) +
  facet_wrap(~ metrica, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Comparação dos modelos — menor é melhor",
    x = NULL, y = NULL
  )






#Metodo mais simples: Holdout


# Holdout: reservar os últimos 24 meses
h <- 24

treino <- data_bal |>
  slice_head(n = nrow(data_bal) - h)

teste <- data_bal |>
  slice_tail(n = h)

medidas <- list(RMSE = RMSE, MAE = MAE, CRPS = CRPS)


# Modelos univariados
fit_fable <- treino |>
  model(
    ar = ARIMA(cpi_core ~ pdq(d = 1, q = 0) + PDQ(D = 0, Q = 0)),
    naive = NAIVE(cpi_core),
    drift = RW(cpi_core ~ drift())
  )

prev_fable <- fit_fable |>
  fabletools::forecast(h = h)

# VAR benchmark
fit_var <- treino |>
  model(var = VAR(vars(atividade, cpi_core, juro_curto)))

prev_var <- fit_var |>
  fabletools::forecast(h = h)

# Selecionar a distribuicao do CPI para calcular o CRPS do VAR
param_var <- distributional::parameters(prev_var$.distribution)

# CPI e a segunda variavel na formula do VAR
prev_var_cpi <- tibble(
  .model = prev_var$.model,
  date = prev_var$date,
  cpi_core = distributional::dist_normal(
    mu = map_dbl(param_var$mu, 2),
    sigma = sqrt(map_dbl(param_var$sigma, ~ .x[2, 2]))
  )
) |>
  as_tsibble(key = .model, index = date) |>
  as_fable(response = "cpi_core", distribution = "cpi_core")





dados_treino <- treino |>
  as_tibble() |>
  select(
    ust2y, ip_mundial, brent, vix,
    atividade, cpi_core, juro_curto,
    inclinacao, usdjpy, topix
  ) |>
  as.matrix()

spec_holdout <- bsvarSIGNs::specify_bsvarSIGN$new(
  data = dados_treino,
  p = 12
)

set.seed(123)
post_holdout <- bsvars::estimate(spec_holdout, S = 200)

prev_bvar <- bsvarSIGNs::forecast(post_holdout, horizon = h)

# Selecionar os sorteios de CPI
pos_cpi <- match("cpi_core", colnames(dados_treino))
sorteios_cpi <- prev_bvar$forecasts[pos_cpi, , ]

# Organizar para usar accuracy()
prev_bvar_cpi <- tibble(
  .model = "BVAR",
  date = teste$date,
  cpi_core = distributional::dist_sample(split(sorteios_cpi, row(sorteios_cpi)))
) |>
  as_tsibble(key = .model, index = date) |>
  as_fable(response = "cpi_core", distribution = "cpi_core")




acc_fable <- prev_fable |>
  accuracy(data_bal, measures = medidas)

acc_var <- prev_var_cpi |>
  accuracy(data_bal, measures = medidas)

acc_bvar <- prev_bvar_cpi |>
  accuracy(data_bal, measures = medidas)

holdout_results <- bind_rows(acc_fable, acc_var, acc_bvar) |>
  select(.model, .type, RMSE, MAE, CRPS) |>
  arrange(RMSE)

holdout_results
