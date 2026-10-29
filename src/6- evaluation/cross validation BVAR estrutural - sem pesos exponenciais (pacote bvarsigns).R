# 9. CV estrutural: sete variáveis, p=6, condicional ex post a Treasury e Brent.
# Mesma estrutura externa e calendário trimestral do teste 1 reduzido.
#TODO: outras métricas do plano



library(tidyverse)
library(fpp3)
library(fabletools)
library(distributional)
library(bsvars)
library(bsvarSIGNs)

# Mesmo calendário do CV original; previsões mensais até 24 meses.
p <- 6
h <- 24
stopifnot("Use o bsvarSIGNs 3.0 da biblioteca padrão do R." =
            as.character(packageVersion("bsvarSIGNs")) == "3.0")
# Correção das defasagens em R no bloco 9.3.2; pacote oficial sem modificações.

# Usar a amostra comum data_bal, preparada no feature_engineering.R.

# Lacunas internas alterariam o significado das defasagens e das datas previstas.
if (any(tsibble::has_gaps(data_bal)$.gaps)) {
  stop("A remoção dos NAs deixou meses ausentes no meio da amostra. Corrija essas lacunas antes do CV.")
}

cv_data <- data_bal |>
  stretch_tsibble(.init = 180, .step = 3)   # Primeira janela: 180 observações completas. (2000 até 2014)
                                            # Avançar três meses por janela: mesmo calendário do teste 1 reduzido.

# Checkpoints: previsoes e diagnosticos de cada janela; posterior completo nao e gravado.
diretorio_checkpoint_estrutural <- "C:/Users/dudup/OneDrive/japan-econometrics-model/data/processed/cv_estrutural_7vars_expost_trimestral_p6"
dir.create(file.path(diretorio_checkpoint_estrutural, "checkpoints"), recursive = TRUE, showWarnings = FALSE)
configuracao_checkpoint_estrutural <- list(
  dados = as.data.frame(data_bal), p = p, h = h, step = 3L,
  ex_post = c("ust2y", "brent"),

  script = unname(tools::md5sum("C:/Users/dudup/OneDrive/japan-econometrics-model/src/6- evaluation/cross validation BVAR estrutural (pacote bvarsigns).R")),
  bsvarSIGNs = as.character(packageVersion("bsvarSIGNs")),
  bsvars = as.character(packageVersion("bsvars")), R = R.version.string
)
arquivo_configuracao_estrutural <- file.path(diretorio_checkpoint_estrutural, "configuracao.rds")
if (file.exists(arquivo_configuracao_estrutural)) {
  stopifnot("A base, configuracao ou codigo mudou; nao reutilizar checkpoints diferentes." =
              identical(readRDS(arquivo_configuracao_estrutural), configuracao_checkpoint_estrutural))
} else {
  saveRDS(configuracao_checkpoint_estrutural, arquivo_configuracao_estrutural, compress = FALSE)
}

# 9.1. Modelos univariados de referência para o CPI
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

# 9.2. VAR de referência com atividade, CPI e juro curto
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
if (exists("previsoes_bvar_estrutural", inherits = FALSE) && exists("previsoes_bvar_estrutural_atividade", inherits = FALSE)) {
  stop("O BVAR estrutural ja tem previsoes nesta sessao. Para comparar periodos, execute somente a secao 9.6. Para um novo CV intencional, remova antes previsoes_bvar_estrutural e previsoes_bvar_estrutural_atividade.")
}

# 9.3. Validação temporal do BVAR estrutural com COVID
# Reestimar no treino; lambda/mu/delta/psi fixos e apenas os quatro parâmetros COVID estimados.

set.seed(123)

janelas <- unique(cv_data$.id)
resultados_bvar_estrutural <- vector("list", length(janelas))
resultados_bvar_estrutural_atividade <- vector("list", length(janelas))
diagnosticos_cv_estrutural <- vector("list", length(janelas))
caminhos_externos_cv_estrutural <- vector("list", length(janelas))

for (i in seq_along(janelas)) {

  # 9.3.1. Preparar a janela de treino
  treino <- cv_data |>
    filter(.id == janelas[i]) |>
    as_tibble() |>
    arrange(date)

  origem <- max(treino$date)
  if (origem >= max(data_bal$date)) next

  message("Janela ", i, "/", length(janelas), " - origem: ", origem)

  arquivo_checkpoint_estrutural <- file.path(diretorio_checkpoint_estrutural, "checkpoints",
                                             sprintf("janela_%03d.rds", i))
  if (file.exists(arquivo_checkpoint_estrutural)) {
    janela_checkpoint_estrutural <- readRDS(arquivo_checkpoint_estrutural)
    stopifnot(identical(janela_checkpoint_estrutural$origem, origem),
              identical(janela_checkpoint_estrutural$indice, i))
    resultados_bvar_estrutural[[i]] <- janela_checkpoint_estrutural$cpi
    resultados_bvar_estrutural_atividade[[i]] <- janela_checkpoint_estrutural$atividade
    diagnosticos_cv_estrutural[[i]] <- janela_checkpoint_estrutural$diagnostico
    caminhos_externos_cv_estrutural[[i]] <- janela_checkpoint_estrutural$caminhos_externos
    message("Checkpoint recuperado; janela ja concluida.")
    next
  }

  # Somente Treasury e Brent realizados entram como condições futuras.
  teste <- data_bal |>
    filter(date > origem, date <= origem + h) |>
    as_tibble()
  H <- nrow(teste)
  caminhos <- teste |> select(ust2y, brent) |> as.matrix()
  condicoes <- cbind(caminhos, matrix(NA_real_, H, 5))
  stopifnot(H > 0L, all(is.finite(caminhos)))
  caminhos_externos_cv_estrutural[[i]] <- bind_cols(
    tibble(.id = janelas[i], origem = origem, date = teste$date), as_tibble(caminhos))

  dados_treino <- treino |>
    select(
      ust2y, brent,
      atividade, cpi_core, juro_curto,
      inclinacao, usdjpy
    ) |>
    as.matrix()

  colnames(condicoes) <- colnames(dados_treino)
  pos_cpi <- match("cpi_core", colnames(dados_treino))
  pos_atividade <- match("atividade", colnames(dados_treino))

  # 9.3.2. Estimar o BVAR estrutural com COVID, como no script do modelo
  matrix_externa <- dados_treino[, 1:2, drop = FALSE]
  matrix_interna <- dados_treino[, 3:7, drop = FALSE]

  inicio_covid <- match(as.Date("2020-03-01"), as.Date(treino$date)) - p
  if (is.na(inicio_covid) || inicio_covid <= 0) inicio_covid <- NULL
  # Esperar quatro meses observados para estimar o perfil COVID de quatro parâmetros.
  if (!is.null(inicio_covid) && nrow(treino) - p - inicio_covid + 1L < 4L) inicio_covid <- NULL

  # Respostas x choques x horizontes; posições 1:13 representam meses 0:12.
  # Variáveis: ust2y, brent, atividade, cpi_core, juro_curto, inclinacao, usdjpy.
  # Choques: Fed, petróleo, demanda doméstica, oferta doméstica, BoJ, prêmio fiscal/prazo, fluxo/UIP.
  sinais_cv_estrutural <- array(NA_real_, dim = c(7, 7, 13))
  
  # BoJ (5): juro sobe, curva achata; atividade e CPI caem nos meses 3 a 12.
  sinais_cv_estrutural[5, 5, 1:2] <- 1
  sinais_cv_estrutural[6, 5, 1:2] <- -1
  sinais_cv_estrutural[c(3, 4), 5, 4:13] <- -1
  
  # Demanda doméstica (3): atividade, CPI e juro sobem.
  sinais_cv_estrutural[3, 3, 1:2] <- 1
  sinais_cv_estrutural[4, 3, 1:2] <- 1
  sinais_cv_estrutural[5, 3, 1:2] <- 1
  
  # Oferta doméstica (4): atividade sobe e CPI cai.
  sinais_cv_estrutural[3, 4, 1:2] <- 1
  sinais_cv_estrutural[4, 4, 1:2] <- -1
  
  # Prêmio fiscal/de prazo (6): curva inclina e USDJPY sobe.
  # Esses sinais não provam uma causa fiscal exclusiva.
  sinais_cv_estrutural[6, 6, 1:2] <- 1
  sinais_cv_estrutural[7, 6, 1:2] <- 1
  
  # Fed (1): Treasury de 2 anos sobe e Brent cai; respostas japonesas livres.
  sinais_cv_estrutural[1, 1, 1:2] <- 1
  sinais_cv_estrutural[2, 1, 1:2] <- -1
  
  # Petróleo (2): Brent sobe; respostas japonesas livres.
  # Sem IP mundial, esse sinal sozinho não separa oferta de petróleo de demanda global.
  sinais_cv_estrutural[2, 2, 1:2] <- 1
  
  # Fluxo/UIP (7): USDJPY sobe, inclinação cai e CPI sobe nos meses 3 a 12.
  sinais_cv_estrutural[7, 7, 1:2] <- 1
  sinais_cv_estrutural[6, 7, 1:2] <- -1
  sinais_cv_estrutural[4, 7, 4:13] <- 1

  # 9.3.2. Restrições do modelo estrutural, com narrativas apenas de meses no treino.
  # BoJ, prêmio fiscal e cinco intervenções do MoF; narrativas de risk-off retiradas.
  eventos_cv_estrutural <- tibble(
    mes = as.Date(c("2024-03-01", "2025-10-01",
                    "2022-09-01", "2022-10-01", "2024-04-01", "2024-05-01", "2024-07-01")),
    choque = c(5, 6, 7, 7, 7, 7, 7),
    sinal = c(1, 1, -1, -1, -1, -1, -1)
  ) |>
    mutate(inicio = match(mes, as.Date(treino$date))) |>
    filter(!is.na(inicio), inicio > p)
  narrativas_cv_estrutural <- list()
  for (k in seq_len(nrow(eventos_cv_estrutural))) {
    narrativas_cv_estrutural[[k]] <- bsvarSIGNs::specify_narrative(
      start = eventos_cv_estrutural$inicio[k], periods = 1, type = "S",
      sign = eventos_cv_estrutural$sinal[k], shock = eventos_cv_estrutural$choque[k]
    )
  }
  # start usa a posição no treino original; o pacote desconta p internamente.
  # CV histórico com a base atual; sem reconstrução dos vintages de divulgação.

  set.seed(123 + i)
  spec_cv_estrutural <- bsvarSIGNs::specify_bsvarSIGN$new(
    data = matrix_interna,
    foreign = matrix_externa,
    p = p,
    hyper_mu = FALSE,
    hyper_delta = FALSE,
    hyper_lambda = FALSE,
    hyper_psi = FALSE,
    hyper_covid = inicio_covid,
    sign_irf = sinais_cv_estrutural,
    sign_narrative = narrativas_cv_estrutural,
    sign_structural = matrix(NA_real_, 7, 7)
  )
  if (!is.null(inicio_covid)) spec_cv_estrutural$estimate_hyper(S = 5000, burn_in = 2500)
  post_cv_estrutural <- bsvars::estimate(spec_cv_estrutural, S = 2500, show_progress = FALSE)

  # Mesmas checagens numéricas do modelo estrutural, guardadas por janela.
  coeficientes_cv_estrutural <- matrix(post_cv_estrutural$posterior$A,
                                      ncol = dim(post_cv_estrutural$posterior$A)[3])
  lags_domesticos_cv <- unlist(lapply(0:(p - 1), function(j) 3:7 + 7 * j))
  diagnosticos_cv_estrutural[[i]] <- tibble(
    .id = janelas[i], origem = origem, narrativas = length(narrativas_cv_estrutural),
    valores_validos = all(is.finite(coeficientes_cv_estrutural)) &&
      all(is.finite(post_cv_estrutural$posterior$Sigma)) && all(is.finite(post_cv_estrutural$posterior$Theta0)),
    ess_ponderacao = post_cv_estrutural$posterior$ess,
    conjuntos_distintos = ncol(unique(coeficientes_cv_estrutural, MARGIN = 2)),
    coef_dom_nas_externas = max(abs(post_cv_estrutural$posterior$A[1:2, lags_domesticos_cv, ])),
    impacto_dom_nas_externas = max(abs(post_cv_estrutural$posterior$Theta0[1:2, 3:7, ]))
  )
  print(diagnosticos_cv_estrutural[[i]])
  if (!diagnosticos_cv_estrutural[[i]]$valores_validos ||
      !is.finite(diagnosticos_cv_estrutural[[i]]$ess_ponderacao) ||
      diagnosticos_cv_estrutural[[i]]$ess_ponderacao < 50 ||
      diagnosticos_cv_estrutural[[i]]$conjuntos_distintos < 50 ||
      diagnosticos_cv_estrutural[[i]]$coef_dom_nas_externas > 1e-8 ||
      diagnosticos_cv_estrutural[[i]]$impacto_dom_nas_externas > 1e-8) {
    saveRDS(diagnosticos_cv_estrutural[[i]], file.path(diretorio_checkpoint_estrutural, sprintf("falha_diagnostico_%03d.rds", i)))
    stop("A janela ", i, " nao passou pela checagem numerica; nao comparar metricas de um CV incompleto.")
  }

  set.seed(123)
  previsao_cv_estrutural <- bsvarSIGNs::forecast(
    post_cv_estrutural, horizon = H, conditional_forecast = condicoes)

  # Mesma correção da recursão do teste 1, preservando Treasury e Brent realizados.
  # Manter os choques nativos e a escala COVID do pacote; iniciar no último dado.
  choques_cv <- previsao_cv_estrutural$forecasts - previsao_cv_estrutural$forecast_mean
  stopifnot(all(is.finite(choques_cv)))
  estado_inicial_cv <- c(t(dados_treino[nrow(dados_treino) - (0:(p - 1)), , drop = FALSE]), 1)
  for (s in seq_len(dim(previsao_cv_estrutural$forecasts)[3])) {
    estado_cv <- estado_inicial_cv
    sigma <- post_cv_estrutural$posterior$Sigma[, , s]
    ganho <- sigma[, 1:2, drop = FALSE] %*% solve(sigma[1:2, 1:2, drop = FALSE])
    for (j in seq_len(H)) {
      media <- drop(post_cv_estrutural$posterior$A[, , s] %*% estado_cv)
      ajuste_externo <- media[1:2] - previsao_cv_estrutural$forecast_mean[1:2, j, s]
      previsao_cv_estrutural$forecast_mean[, j, s] <- media
      previsao_cv_estrutural$forecasts[, j, s] <- media + choques_cv[, j, s] - drop(ganho %*% ajuste_externo)
      previsao_cv_estrutural$forecasts[1:2, j, s] <- caminhos[j, ]
      estado_cv <- c(previsao_cv_estrutural$forecasts[, j, s], head(estado_cv, ncol(dados_treino) * (p - 1)), 1)
    }
  }
  stopifnot(all(is.finite(previsao_cv_estrutural$forecasts)),
            max(abs(sweep(previsao_cv_estrutural$forecasts[1:2, , , drop = FALSE],
                          c(1, 2), t(caminhos), "-"))) < 1e-8)
  # forecasts: variáveis (externas primeiro) x horizontes x sorteios.
  sorteios_cpi <- matrix(previsao_cv_estrutural$forecasts[pos_cpi, , ], nrow = H)

  resultados_bvar_estrutural[[i]] <- tibble(
    .id = janelas[i],
    .model = "BVAR estrutural 7 | petroleo/UST ex post",
    date = teste$date,
    h = seq_len(H),
    sorteios = split(sorteios_cpi, row(sorteios_cpi))
  )

  # Avaliar a atividade usando os mesmos sorteios do BVAR estrutural.
  sorteios_atividade <- matrix(previsao_cv_estrutural$forecasts[pos_atividade, , ], nrow = H)
  resultados_bvar_estrutural_atividade[[i]] <- tibble(
    .id = janelas[i],
    .model = "BVAR estrutural 7 | petroleo/UST ex post",
    date = teste$date,
    h = seq_len(H),
    sorteios = split(sorteios_atividade, row(sorteios_atividade))
  )
  # Uma gravacao sem compressao por janela; arquivo temporario evita checkpoint incompleto.
  saveRDS(list(indice = i, origem = origem,
               cpi = resultados_bvar_estrutural[[i]],
               atividade = resultados_bvar_estrutural_atividade[[i]],
               diagnostico = diagnosticos_cv_estrutural[[i]],
               caminhos_externos = caminhos_externos_cv_estrutural[[i]]),
          paste0(arquivo_checkpoint_estrutural, ".tmp"), compress = FALSE)
  stopifnot(file.rename(paste0(arquivo_checkpoint_estrutural, ".tmp"), arquivo_checkpoint_estrutural))
  message("Checkpoint salvo: ", basename(arquivo_checkpoint_estrutural), " - ", Sys.time())
}

diagnosticos_cv_estrutural <- bind_rows(diagnosticos_cv_estrutural)
caminhos_externos_cv_estrutural <- bind_rows(caminhos_externos_cv_estrutural)

# 9.3.3. Adaptar as distribuições marginais do CPI para accuracy()
previsoes_bvar_estrutural <- bind_rows(resultados_bvar_estrutural) |>
  mutate(
    cpi_core = dist_sample(sorteios),
    .mean = mean(cpi_core)
  ) |>
  select(-sorteios) |>
  filter(date <= max(data_bal$date)) |>
  as_tsibble(key = c(.id, .model), index = date) |>
  as_fable(response = "cpi_core", distribution = "cpi_core")

acc_bvar_estrutural <- previsoes_bvar_estrutural |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))


# Adaptar também as distribuições marginais da atividade.
previsoes_bvar_estrutural_atividade <- bind_rows(resultados_bvar_estrutural_atividade) |>
  mutate(
    atividade = dist_sample(sorteios),
    .mean = mean(atividade)
  ) |>
  select(-sorteios) |>
  filter(date <= max(data_bal$date)) |>
  as_tsibble(key = c(.id, .model), index = date) |>
  as_fable(response = "atividade", distribution = "atividade")

acc_bvar_estrutural_atividade <- previsoes_bvar_estrutural_atividade |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))

# 9.4. Comparar as métricas de previsão do CPI
cv_results_estrutural <- bind_rows(
  acc_fable,
  acc_var,
  acc_bvar_estrutural
) |>
  select(.model, .type, RMSE, MAE, CRPS) |>
  arrange(RMSE)

cv_results_estrutural





grafico_cv_estrutural_cpi <- cv_results_estrutural |>
  select(.model, RMSE, CRPS) |>
  pivot_longer(-.model, names_to = "metrica", values_to = "valor") |>
  ggplot(aes(x = .model, y = valor, fill = .model)) +
  geom_col(na.rm = TRUE) +
  facet_wrap(~ metrica, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "CV do CPI — menor é melhor",
    subtitle = "BVAR estrutural 7: Treasury e Brent realizados (condicional ex post)",
    x = NULL, y = NULL
  )

print(grafico_cv_estrutural_cpi)
ggsave(file.path(diretorio_checkpoint_estrutural, "CV estrutural - CPI.png"), grafico_cv_estrutural_cpi, width = 11, height = 5, dpi = 160)

# 9.5. Comparar as métricas de previsão da atividade
cv_results_estrutural_atividade <- bind_rows(
  acc_fable_atividade,
  acc_var_atividade,
  acc_bvar_estrutural_atividade
) |>
  select(.model, .type, RMSE, MAE, CRPS) |>
  arrange(RMSE)

cv_results_estrutural_atividade

grafico_cv_estrutural_atividade <- cv_results_estrutural_atividade |>
  select(.model, RMSE, CRPS) |>
  pivot_longer(-.model, names_to = "metrica", values_to = "valor") |>
  ggplot(aes(x = .model, y = valor, fill = .model)) +
  geom_col(na.rm = TRUE) +
  facet_wrap(~ metrica, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "CV da atividade — menor é melhor",
    subtitle = "BVAR estrutural 7: Treasury e Brent realizados (condicional ex post)",
    x = NULL, y = NULL
  )

print(grafico_cv_estrutural_atividade)
ggsave(file.path(diretorio_checkpoint_estrutural, "CV estrutural - atividade.png"), grafico_cv_estrutural_atividade, width = 11, height = 5, dpi = 160)

# 9.6. Comparar por período da DATA PREVISTA: 2015-2019, 2020-2021 e 2022 em diante.
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
  previsoes_bvar_estrutural |> as_tibble() |>
    transmute(.id, .model, date, variavel = "cpi_core", valor = unname(cpi_core)),
  previsoes_fable_atividade_periodos |> as_tibble() |>
    transmute(.id, .model, date, variavel = "atividade", valor = unname(atividade)),
  previsoes_var_atividade |> as_tibble() |>
    transmute(.id, .model, date, variavel = "atividade", valor = unname(atividade)),
  previsoes_bvar_estrutural_atividade |> as_tibble() |>
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
cv_resultados_periodos_estrutural <- previsoes_periodos |>
  accuracy(observados_periodos, by = c("variavel", "periodo", ".model"),
           measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS)) |>
  select(variavel, periodo, .model, RMSE, MAE, CRPS) |>
  arrange(variavel, periodo, RMSE)
print(cv_resultados_periodos_estrutural, n = Inf)

# Contagens por modelo: uma observação é um par janela/data prevista.
cv_casos_periodos_estrutural <- previsoes_periodos |>
  as_tibble() |>
  count(variavel, periodo, .model, name = "casos")
print(cv_casos_periodos_estrutural, n = Inf)

for (alvo in c("cpi_core", "atividade")) {
  grafico_periodos <- cv_resultados_periodos_estrutural |>
    filter(variavel == alvo) |>
    pivot_longer(c(RMSE, CRPS), names_to = "metrica", values_to = "erro") |>
    ggplot(aes(x = erro, y = .model, fill = .model)) +
    geom_col() +
    facet_wrap(vars(periodo, metrica), ncol = 2, scales = "free_x") +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = paste("CV por período:", alvo, "— menor é melhor"),
         subtitle = "Data prevista; H1-H24; BVAR estrutural 7 condicional ex post",
         caption = "Treasury e Brent futuros realizados; diagnóstico condicional ex post.",
         x = NULL, y = NULL)
  print(grafico_periodos)
  ggsave(file.path(diretorio_checkpoint_estrutural, paste0("CV estrutural - ", alvo, " por periodo.png")), grafico_periodos, width = 11, height = 9, dpi = 160)
}

# Resultados finais em disco: somente apos todas as janelas e metricas terminarem.
write_csv(cv_results_estrutural, file.path(diretorio_checkpoint_estrutural, "metricas_cpi.csv"))
write_csv(cv_results_estrutural_atividade, file.path(diretorio_checkpoint_estrutural, "metricas_atividade.csv"))
write_csv(cv_resultados_periodos_estrutural, file.path(diretorio_checkpoint_estrutural, "metricas_por_periodo.csv"))
write_csv(cv_casos_periodos_estrutural, file.path(diretorio_checkpoint_estrutural, "casos_por_periodo.csv"))
write_csv(diagnosticos_cv_estrutural, file.path(diretorio_checkpoint_estrutural, "diagnosticos.csv"))
write_csv(caminhos_externos_cv_estrutural, file.path(diretorio_checkpoint_estrutural, "caminhos_externos.csv"))
saveRDS(list(cpi = cv_results_estrutural, atividade = cv_results_estrutural_atividade,
             periodos = cv_resultados_periodos_estrutural, casos = cv_casos_periodos_estrutural,
             diagnosticos = diagnosticos_cv_estrutural,
             caminhos_externos = caminhos_externos_cv_estrutural),
        file.path(diretorio_checkpoint_estrutural, "resultados_finais.rds"), compress = FALSE)
