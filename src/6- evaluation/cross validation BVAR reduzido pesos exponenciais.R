# Validação temporal: BVAR reduzido com pesos exponenciais, VAR e univariados
#TODO: outras métricas do plano



library(tidyverse)
library(fpp3)
library(fabletools)
library(distributional)
library(bsvars)
library(bsvarSIGNs)

# previsões mensais até 24 meses.
p <- 6
h <- 24
rho_peso <- 0.995
lambda <- 0.2
mu <- delta <- 1
saida_cv_pesos <- "src/Outputs/Evaluation/CV BVAR reduzido pesos exponenciais"
dir.create(saida_cv_pesos, recursive = TRUE, showWarnings = FALSE)
stopifnot(rho_peso > 0, rho_peso <= 1)
stopifnot("Use o bsvarSIGNs 3.0 da biblioteca padrão do R." =
            as.character(packageVersion("bsvarSIGNs")) == "3.0")

# Lacunas internas alterariam o significado das defasagens e das datas previstas.
if (any(tsibble::has_gaps(data_bal)$.gaps)) {
  stop("A remoção dos NAs deixou meses ausentes no meio da amostra. Corrija essas lacunas antes do CV.")
}

cv_data <- data_bal |>
  select(date, ust2y, brent, atividade, cpi_core, juro_curto, inclinacao, usdjpy) |>
  arrange(date) |>
  stretch_tsibble(.init = 180, .step = 3)   # Primeira janela: 180 observações completas. (2000 até 2014)
                                            # Avançar três meses por janela para reduzir o tempo de estimação.

#1. Modelos univariados de referência para o CPI
cv_fable <- cv_data |>
  model(
    ar    = ARIMA(cpi_core ~ pdq(d = 1, q = 0) + PDQ(D = 0, Q = 0)),
    naive = NAIVE(cpi_core),
    drift = RW(cpi_core ~ drift())
  )


# Calcular e guardar as métricas dos modelos univariados
acc_fable <- cv_fable |>
  forecast(h = h) |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))



# Mesmos benchmarks para a atividade, usando as mesmas janelas.
cv_fable_atividade <- cv_data |>
  model(
    ar = ARIMA(atividade ~ pdq(d = 1, q = 0) + PDQ(D = 0, Q = 0)),
    naive = NAIVE(atividade),
    drift = RW(atividade ~ drift())
  )

acc_fable_atividade <- cv_fable_atividade |>
  forecast(h = h) |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))

#2. VAR de referência com atividade, CPI e juro curto
cv_var <- cv_data |>
  model(var = VAR(vars(atividade, cpi_core, juro_curto)))



# Prever o VAR uma única vez
previsoes_var <- cv_var |>
  forecast(h = h) |>
  filter(date <= max(data_bal$date))

# RMSE e MAE: calcular diretamente e selecionar o CPI
acc_var <- previsoes_var |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE)) |>
  filter(.response == "cpi_core")

# CRPS: extrair a distribuição marginal do CPI, a segunda variável do VAR
param_var_cv <- parameters(previsoes_var$.distribution)

previsoes_var_cpi <- tibble(
  .id = previsoes_var$.id,
  .model = previsoes_var$.model,
  date = previsoes_var$date,
  cpi_core = dist_normal(
    mu = map_dbl(param_var_cv$mu, 2),
    sigma = sqrt(map_dbl(param_var_cv$sigma, ~ .x[2, 2]))
  )
) |>
  as_tsibble(key = c(.id, .model), index = date) |>
  as_fable(response = "cpi_core", distribution = "cpi_core")

crps_var <- previsoes_var_cpi |>
  accuracy(data_bal, measures = list(CRPS = CRPS)) |>
  select(.model, .type, CRPS)

# Reunir as três métricas do CPI para a tabela comparativa
acc_var <- acc_var |>
  left_join(crps_var, by = c(".model", ".type"))

# A atividade é a primeira variável do mesmo VAR: extrair sua distribuição marginal.
previsoes_var_atividade <- tibble(
  .id = previsoes_var$.id,
  .model = previsoes_var$.model,
  date = previsoes_var$date,
  atividade = dist_normal(
    mu = map_dbl(param_var_cv$mu, 1),
    sigma = sqrt(map_dbl(param_var_cv$sigma, ~ .x[1, 1]))
  )
) |>
  as_tsibble(key = c(.id, .model), index = date) |>
  as_fable(response = "atividade", distribution = "atividade")

acc_var_atividade <- previsoes_var_atividade |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))

# Previsões já existentes: impedir que uma execução repetida reinicie o BVAR.
if (exists("previsoes_bvar", inherits = FALSE) && exists("previsoes_bvar_atividade", inherits = FALSE)) {
  stop("O BVAR ja tem previsoes nesta sessao. Para comparar periodos, execute somente a secao 6 ou o arquivo resultados CV BVAR reduzido por periodo.R. Para um novo CV intencional, remova antes previsoes_bvar e previsoes_bvar_atividade.")
}

# 3. Validação temporal do BVAR reduzido
# Reestimar no treino; lambda/mu/delta/psi fixos e apenas os quatro parâmetros COVID estimados.

set.seed(123)

janelas <- unique(cv_data$.id)
resultados_bvar <- vector("list", length(janelas))
resultados_bvar_atividade <- vector("list", length(janelas))
resultados_bvar_todos <- vector("list", length(janelas))

for (i in seq_along(janelas)) {

  # 9.3.1. Preparar a janela de treino
  treino <- cv_data |>
    filter(.id == janelas[i]) |>
    as_tibble() |>
    arrange(date)

  origem <- max(treino$date)
  if (origem >= max(data_bal$date)) next

  message("Janela ", i, "/", length(janelas), " - origem: ", origem)
  teste <- data_bal |> as_tibble() |> filter(date > origem, date <= origem + h) |> arrange(date)
  H <- nrow(teste)
  tendencia <- matrix((seq_len(nrow(treino)) - nrow(treino)) / 12, ncol = 1)
  tendencia_futura <- matrix(seq_len(H) / 12, ncol = 1)

  dados_treino <- treino |>
    select(
      ust2y, brent,
      atividade, cpi_core, juro_curto,
      inclinacao, usdjpy
    ) |>
    as.matrix()

  pos_cpi <- match("cpi_core", colnames(dados_treino))
  pos_atividade <- match("atividade", colnames(dados_treino))

  #3.2. Estimar o BVAR reduzido
  matrix_externa <- dados_treino[, 1:2, drop = FALSE]
  matrix_interna <- dados_treino[, 3:7, drop = FALSE]

  inicio_covid <- match(as.Date("2020-03-01"), as.Date(treino$date)) - p
  if (is.na(inicio_covid) || inicio_covid <= 0) inicio_covid <- NULL
  # Esperar quatro meses observados para estimar o perfil COVID de quatro parâmetros.
  if (!is.null(inicio_covid) && nrow(treino) - p - inicio_covid + 1L < 4L) inicio_covid <- NULL

  set.seed(123 + i + 1000)
  spec_cv <- bsvarSIGNs::specify_bsvarSIGN$new(
    data = matrix_interna,
    foreign = matrix_externa,
    exogenous = tendencia,
    p = p,
    hyper_mu = FALSE,
    hyper_delta = FALSE,
    hyper_lambda = FALSE,
    hyper_psi = FALSE,
    hyper_covid = inicio_covid,
    sign_structural = matrix(NA_real_, 7, 7)
  )
  spec_cv$prior$hyper[1:3, ] <- c(mu, delta, lambda)

  # Pesos por precisão, calculados novamente dentro de cada treino.
  Y_cv <- spec_cv$data_matrices$Y
  X_cv <- spec_cv$data_matrices$X
  pesos <- rho_peso^rev(seq_len(ncol(Y_cv)) - 1)
  spec_cv$data_matrices$Y <- sweep(Y_cv, 2, sqrt(pesos), "*")
  spec_cv$data_matrices$X <- sweep(X_cv, 2, sqrt(pesos), "*")
  spec_cv$prior$Y <- spec_cv$data_matrices$Y
  spec_cv$prior$X <- spec_cv$data_matrices$X
  if (!is.null(inicio_covid)) spec_cv$estimate_hyper(S = 5000, burn_in = 2500)
  post_cv <- bsvars::estimate(spec_cv, S = 2500, show_progress = FALSE)

  stopifnot(all(is.finite(post_cv$posterior$A)), all(is.finite(post_cv$posterior$Sigma)),
    is.finite(post_cv$posterior$ess), post_cv$posterior$ess >= 50)

  # Recuperar as unidades originais antes do forecast.
  post_cv$last_draw$data_matrices$Y <- Y_cv
  post_cv$last_draw$data_matrices$X <- X_cv
  post_cv$last_draw$prior$Y <- Y_cv
  post_cv$last_draw$prior$X <- X_cv
  caminhos <- as.matrix(select(teste, ust2y, brent))
  condicoes <- cbind(caminhos, matrix(NA_real_, H, 5))
  set.seed(200000 + i + 1000)
  previsao_cv <- bsvarSIGNs::forecast(post_cv, horizon = H,
    conditional_forecast = condicoes, exogenous_forecast = tendencia_futura)

  # Mesma recursão do CV com pesos; arrays locais evitam copiar o objeto a cada mês.
  F <- previsao_cv$forecasts
  medias <- previsao_cv$forecast_mean
  choques_cv <- F - medias
  estado_inicial_cv <- c(t(dados_treino[nrow(dados_treino) - (0:(p - 1)), , drop = FALSE]),
    1, tendencia_futura[1, 1])
  for (s in seq_len(dim(F)[3])) {
    estado_cv <- estado_inicial_cv
    sigma <- post_cv$posterior$Sigma[, , s]
    ganho <- sigma[, 1:2, drop = FALSE] %*% solve(sigma[1:2, 1:2, drop = FALSE])
    for (j in seq_len(H)) {
      media <- drop(post_cv$posterior$A[, , s] %*% estado_cv)
      ajuste_externo <- media[1:2] - medias[1:2, j, s]
      medias[, j, s] <- media
      F[, j, s] <- media + choques_cv[, j, s] - drop(ganho %*% ajuste_externo)
      F[1:2, j, s] <- caminhos[j, ]
      estado_cv <- c(F[, j, s], head(estado_cv, ncol(dados_treino) * (p - 1)), 1, (j + 1) / 12)
    }
  }
  stopifnot(all(is.finite(F)),
    max(abs(sweep(F[1:2, , , drop = FALSE], c(1, 2), t(caminhos), "-"))) < 1e-8)
  # forecasts: variáveis (externas primeiro) x horizontes x sorteios.
  sorteios_cpi <- matrix(F[pos_cpi, , ], nrow = H)

  resultados_bvar[[i]] <- tibble(
    .id = janelas[i],
    .model = "bvar reduzido com pesos",
    date = origem + seq_len(H),
    h = seq_len(H),
    sorteios = split(sorteios_cpi, row(sorteios_cpi))
  )

  # Avaliar a atividade usando os mesmos sorteios do BVAR exógeno.
  sorteios_atividade <- matrix(F[pos_atividade, , ], nrow = H)
  resultados_bvar_atividade[[i]] <- tibble(
    .id = janelas[i],
    .model = "bvar reduzido com pesos",
    date = origem + seq_len(H),
    h = seq_len(H),
    sorteios = split(sorteios_atividade, row(sorteios_atividade))
  )

  # Preservar também as distribuições das cinco variáveis domésticas.
  resultados_bvar_todos[[i]] <- map_dfr(colnames(matrix_interna), function(alvo) {
    sorteios <- matrix(F[match(alvo, colnames(dados_treino)), , ], nrow = H)
    tibble(.id = janelas[i], .model = "bvar reduzido com pesos", variavel = alvo,
      date = origem + seq_len(H), h = seq_len(H), valor = dist_sample(split(sorteios, row(sorteios))))
  })

}

#3.3. Adaptar as distribuições marginais do CPI para accuracy()
previsoes_bvar <- bind_rows(resultados_bvar) |>
  mutate(
    cpi_core = dist_sample(sorteios),
    .mean = mean(cpi_core)
  ) |>
  select(-sorteios) |>
  filter(date <= max(data_bal$date)) |>
  as_tsibble(key = c(.id, .model), index = date) |>
  as_fable(response = "cpi_core", distribution = "cpi_core")

acc_bvar <- previsoes_bvar |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))


# Adaptar também as distribuições marginais da atividade.
previsoes_bvar_atividade <- bind_rows(resultados_bvar_atividade) |>
  mutate(
    atividade = dist_sample(sorteios),
    .mean = mean(atividade)
  ) |>
  select(-sorteios) |>
  filter(date <= max(data_bal$date)) |>
  as_tsibble(key = c(.id, .model), index = date) |>
  as_fable(response = "atividade", distribution = "atividade")

acc_bvar_atividade <- previsoes_bvar_atividade |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))

#4. Comparar as métricas de previsão do CPI
cv_results <- bind_rows(
  acc_fable,
  acc_var,
  acc_bvar
) |>
  select(.model, .type, RMSE, MAE, CRPS) |>
  arrange(RMSE)

cv_results





grafico_cv_cpi <- cv_results |>
  select(.model, RMSE, CRPS) |>
  pivot_longer(-.model, names_to = "metrica", values_to = "valor") |>
  mutate(
    .model = recode(.model, "bvar reduzido com pesos" = "BVAR reduzido com pesos", "BVAR exógeno COVID" = "BVAR reduzido com pesos",
                    ar = "AR", naive = "Naive", drift = "Drift", var = "VAR"),
    .model = factor(.model, levels = c("VAR", "Drift", "Naive", "AR", "BVAR reduzido com pesos")),
    metrica = factor(metrica, levels = c("RMSE", "CRPS"))
  ) |>
  ggplot(aes(x = valor, y = .model, fill = .model)) +
  geom_col(width = 0.62, na.rm = TRUE) +
  geom_text(aes(label = formatC(valor, format = "f", digits = 2, decimal.mark = ","),
                fontface = if_else(.model == "BVAR reduzido com pesos", "bold", "plain")),
            hjust = -0.15, size = 3.8, colour = "#263747", show.legend = FALSE) +
  facet_wrap(vars(metrica), ncol = 2, scales = "free_x") +
  scale_fill_manual(values = c("BVAR reduzido com pesos" = "#D96830", "AR" = "#355C7D",
                               "Naive" = "#668BA8", "Drift" = "#92AEC4", "VAR" = "#B9CAD8")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_line(colour = "#E8EDF2"),
        axis.text.y = element_text(colour = "#263747"), strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 17),
        plot.margin = margin(12, 20, 12, 12)) +
  labs(title = "Validação temporal · CPI core", subtitle = "Horizontes de 1 a 24 meses reunidos · Menor é melhor",
       x = NULL, y = NULL)
print(grafico_cv_cpi)
ggsave(file.path(saida_cv_pesos, "CV bvar reduzido pesos - CPI.png"),
       plot = grafico_cv_cpi, width = 11, height = 4.8, dpi = 180, bg = "white")

# 5. Comparar as métricas de previsão da atividade
cv_results_atividade <- bind_rows(
  acc_fable_atividade,
  acc_var_atividade,
  acc_bvar_atividade
) |>
  select(.model, .type, RMSE, MAE, CRPS) |>
  arrange(RMSE)

cv_results_atividade

grafico_cv_atividade <- cv_results_atividade |>
  select(.model, RMSE, CRPS) |>
  pivot_longer(-.model, names_to = "metrica", values_to = "valor") |>
  mutate(
    .model = recode(.model, "bvar reduzido com pesos" = "BVAR reduzido com pesos", "BVAR exógeno COVID" = "BVAR reduzido com pesos",
                    ar = "AR", naive = "Naive", drift = "Drift", var = "VAR"),
    .model = factor(.model, levels = c("VAR", "Drift", "Naive", "AR", "BVAR reduzido com pesos")),
    metrica = factor(metrica, levels = c("RMSE", "CRPS"))
  ) |>
  ggplot(aes(x = valor, y = .model, fill = .model)) +
  geom_col(width = 0.62, na.rm = TRUE) +
  geom_text(aes(label = formatC(valor, format = "f", digits = 2, decimal.mark = ","),
                fontface = if_else(.model == "BVAR reduzido com pesos", "bold", "plain")),
            hjust = -0.15, size = 3.8, colour = "#263747", show.legend = FALSE) +
  facet_wrap(vars(metrica), ncol = 2, scales = "free_x") +
  scale_fill_manual(values = c("BVAR reduzido com pesos" = "#D96830", "AR" = "#355C7D",
                               "Naive" = "#668BA8", "Drift" = "#92AEC4", "VAR" = "#B9CAD8")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_line(colour = "#E8EDF2"),
        axis.text.y = element_text(colour = "#263747"), strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 17),
        plot.margin = margin(12, 20, 12, 12)) +
  labs(title = "Validação temporal · Atividade", subtitle = "Horizontes de 1 a 24 meses reunidos · Menor é melhor",
       x = NULL, y = NULL)
print(grafico_cv_atividade)
ggsave(file.path(saida_cv_pesos, "CV bvar reduzido pesos - atividade.png"),
       plot = grafico_cv_atividade, width = 11, height = 4.8, dpi = 180, bg = "white")

# 6. Comparar por período da DATA PREVISTA: 2015-2019, 2020-2021 e 2022 em diante.
# Com o CV concluído na sessão, executar somente daqui até o final, sem reestimar.
# Recalcular apenas as previsões dos univariados já ajustados; não executar model().
previsoes_fable_periodos <- forecast(cv_fable, h = 24)
previsoes_fable_atividade_periodos <- forecast(cv_fable_atividade, h = 24)

# Reunir as distribuições marginais dos cinco modelos e dos dois alvos.
previsoes_periodos <- bind_rows(
  previsoes_fable_periodos |> as_tibble() |>
    transmute(.id, .model, date, variavel = "cpi_core", valor = unname(cpi_core)),
  previsoes_var_cpi |> as_tibble() |>
    transmute(.id, .model, date, variavel = "cpi_core", valor = unname(cpi_core)),
  previsoes_bvar |> as_tibble() |>
    transmute(.id, .model, date, variavel = "cpi_core", valor = unname(cpi_core)),
  previsoes_fable_atividade_periodos |> as_tibble() |>
    transmute(.id, .model, date, variavel = "atividade", valor = unname(atividade)),
  previsoes_var_atividade |> as_tibble() |>
    transmute(.id, .model, date, variavel = "atividade", valor = unname(atividade)),
  previsoes_bvar_atividade |> as_tibble() |>
    transmute(.id, .model, date, variavel = "atividade", valor = unname(atividade))
) |>
  filter(date <= max(data_bal$date), year(date) >= 2015) |>
  mutate(periodo = case_when(
    year(date) <= 2019 ~ "2015-2019",
    year(date) <= 2021 ~ "2020-2021",
    TRUE ~ "2022 em diante"
  ))

# Exigir os mesmos pares janela/data nos cinco modelos; não comparar amostras diferentes.
casos_comuns_periodos <- previsoes_periodos |>
  count(variavel, .id, date, name = "n_modelos")
stopifnot("Há casos ausentes ou duplicados entre os cinco modelos." =
            all(casos_comuns_periodos$n_modelos == 5L))

observados_periodos <- data_bal |>
  as_tibble() |>
  select(date, cpi_core, atividade) |>
  pivot_longer(-date, names_to = "variavel", values_to = "valor") |>
  as_tsibble(key = variavel, index = date)

previsoes_periodos <- previsoes_periodos |>
  as_tsibble(key = c(variavel, .id, .model), index = date) |>
  as_fable(response = "valor", distribution = "valor")

# Todos os horizontes observados de 1 a 24 meses, reunidos dentro de cada período.
cv_resultados_periodos <- previsoes_periodos |>
  accuracy(observados_periodos, by = c("variavel", "periodo", ".model"),
           measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS)) |>
  select(variavel, periodo, .model, RMSE, MAE, CRPS) |>
  arrange(variavel, periodo, RMSE)
print(cv_resultados_periodos, n = Inf)

# Contagens por modelo: uma observação é um par janela/data prevista.
cv_casos_periodos <- previsoes_periodos |>
  as_tibble() |>
  count(variavel, periodo, .model, name = "casos")
print(cv_casos_periodos, n = Inf)

graficos_cv_periodos <- list()
for (alvo in c("cpi_core", "atividade")) {
  grafico_periodos <- cv_resultados_periodos |>
    filter(variavel == alvo) |>
    select(periodo, .model, RMSE, CRPS) |>
    pivot_longer(c(RMSE, CRPS), names_to = "metrica", values_to = "erro") |>
    mutate(
      .model = recode(.model, "bvar reduzido com pesos" = "BVAR reduzido com pesos", "BVAR exógeno COVID" = "BVAR reduzido com pesos",
                      ar = "AR", naive = "Naive", drift = "Drift", var = "VAR"),
      .model = factor(.model, levels = c("VAR", "Drift", "Naive", "AR", "BVAR reduzido com pesos")),
      metrica = factor(metrica, levels = c("RMSE", "CRPS")),
      periodo = factor(periodo, levels = c("2015-2019", "2020-2021", "2022 em diante"))
    ) |>
    ggplot(aes(x = erro, y = .model, fill = .model)) +
    geom_col(width = 0.62) +
    geom_text(aes(label = formatC(erro, format = "f", digits = 2, decimal.mark = ","),
                  fontface = if_else(.model == "BVAR reduzido com pesos", "bold", "plain")),
              hjust = -0.15, size = 3.5, colour = "#263747", show.legend = FALSE) +
    facet_grid(periodo ~ metrica, scales = "free_x") +
    scale_fill_manual(values = c("BVAR reduzido com pesos" = "#D96830", "AR" = "#355C7D",
                                 "Naive" = "#668BA8", "Drift" = "#92AEC4", "VAR" = "#B9CAD8")) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(), panel.grid.major.x = element_line(colour = "#E8EDF2"),
          axis.text.y = element_text(colour = "#263747"), strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold", size = 17),
          plot.margin = margin(12, 20, 12, 12)) +
    labs(title = paste("Validação por período ·", if (alvo == "cpi_core") "CPI core" else "Atividade"),
         subtitle = "Data prevista · Horizontes de 1 a 24 meses reunidos · Menor é melhor",
         x = NULL, y = NULL)
  graficos_cv_periodos[[alvo]] <- grafico_periodos
  print(grafico_periodos)
  ggsave(file.path(saida_cv_pesos, paste0("CV bvar reduzido pesos - ",
                if (alvo == "cpi_core") "CPI" else "atividade", " por periodo.png")),
         plot = grafico_periodos, width = 11, height = 9, dpi = 180, bg = "white")
}


# 6.1. Métricas do BVAR para todas as variáveis domésticas, nos mesmos períodos.
previsoes_bvar_todas <- bind_rows(resultados_bvar_todos) |>
  mutate(.mean = mean(valor), periodo = case_when(year(date) <= 2019 ~ "2015-2019",
    year(date) <= 2021 ~ "2020-2021", TRUE ~ "2022 em diante")) |>
  as_tsibble(key = c(variavel, .id, .model), index = date) |>
  as_fable(response = "valor", distribution = "valor")
observados_bvar_todos <- data_bal |> as_tibble() |>
  select(date, all_of(colnames(matrix_interna))) |>
  pivot_longer(-date, names_to = "variavel", values_to = "valor") |>
  as_tsibble(key = variavel, index = date)
cv_resultados_bvar_todos <- previsoes_bvar_todas |>
  accuracy(observados_bvar_todos, by = c("variavel", ".model"),
    measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))
cv_resultados_bvar_todos_periodos <- previsoes_bvar_todas |>
  accuracy(observados_bvar_todos, by = c("variavel", "periodo", ".model"),
    measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))
print(cv_resultados_bvar_todos, n = Inf)
print(cv_resultados_bvar_todos_periodos, n = Inf)
