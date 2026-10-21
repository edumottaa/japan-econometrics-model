# CV mensal com reestimacao trimestral; tres BVARs e os benchmarks existentes.
# Executar com data_bal preparado. Sem checkpoints ou retomada de janelas.
library(tidyverse)
library(fpp3)
library(distributional)
library(bsvars)
library(bsvarSIGNs)
library(BVAR)

p <- 6
h <- 24
S <- 2500
if (!exists("modelos_cv")) modelos_cv <- 1:3
if (!exists("saida_cv")) saida_cv <- "data/processed/cv_trimestral_3_bvars_20261020"
dir.create(saida_cv, recursive = TRUE, showWarnings = FALSE)
stopifnot(as.character(packageVersion("bsvarSIGNs")) == "3.0")

dados_cv <- data_bal |>
  select(date, ust2y, ip_mundial, brent, vix, atividade, cpi_core,
         juro_curto, inclinacao, usdjpy) |>
  arrange(date)
stopifnot(!any(tsibble::has_gaps(dados_cv)$.gaps),
          all(is.finite(as.matrix(select(as_tibble(dados_cv), -date)))))
cv_data <- dados_cv |> stretch_tsibble(.init = 180, .step = 3)
janelas <- unique(cv_data$.id)
nomes_bvar <- c("BVAR 7 | petroleo/UST", "BVAR 9 | ARIMA externos", "BVAR 7 | hiper estimados")
observados <- dados_cv |>
  as_tibble() |>
  select(date, atividade, cpi_core) |>
  pivot_longer(-date, names_to = "variavel", values_to = "valor") |>
  as_tsibble(key = variavel, index = date)

# 1. Benchmarks: mesmos modelos e mesmas origens dos BVARs.
message("INICIO benchmarks | ", length(janelas), " origens trimestrais")
cv_alvos <- cv_data |>
  select(.id, date, atividade, cpi_core) |>
  pivot_longer(c(atividade, cpi_core), names_to = "variavel", values_to = "valor") |>
  as_tsibble(key = c(variavel, .id), index = date)
previsoes_refs <- cv_alvos |>
  model(ar = ARIMA(valor ~ pdq(d = 1, q = 0) + PDQ(D = 0, Q = 0)),
        naive = NAIVE(valor), drift = RW(valor ~ drift())) |>
  forecast(h = h) |>
  as_tibble() |>
  group_by(variavel, .id, .model) |>
  arrange(date, .by_group = TRUE) |>
  mutate(h = row_number()) |>
  ungroup() |>
  filter(date <= max(dados_cv$date))

previsoes_var <- cv_data |>
  model(var = VAR(vars(atividade, cpi_core, juro_curto))) |>
  forecast(h = h)
param_var <- parameters(previsoes_var$.distribution)
previsoes_var_alvos <- bind_rows(
  tibble(variavel = "atividade", .id = previsoes_var$.id,
         .model = "var", date = previsoes_var$date,
         valor = dist_normal(map_dbl(param_var$mu, 1), sqrt(map_dbl(param_var$sigma, ~ .x[1, 1])))),
  tibble(variavel = "cpi_core", .id = previsoes_var$.id,
         .model = "var", date = previsoes_var$date,
         valor = dist_normal(map_dbl(param_var$mu, 2), sqrt(map_dbl(param_var$sigma, ~ .x[2, 2]))))
) |>
  group_by(variavel, .id, .model) |>
  arrange(date, .by_group = TRUE) |>
  mutate(h = row_number(), .mean = mean(valor)) |>
  ungroup() |>
  filter(date <= max(dados_cv$date))
message("FIM benchmarks")

# 2. Priors do pacote BVAR: lambda, SOC (mu) e SUR (delta) estimados; psi fixo.
priors_cv <- BVAR::bv_priors(
  hyper = c("lambda", "soc", "sur"),
  mn = BVAR::bv_minnesota(lambda = BVAR::bv_lambda(), psi = BVAR::bv_psi()),
  soc = BVAR::bv_soc(), sur = BVAR::bv_sur(min = 0.001)
)
mh_cv <- BVAR::bv_mh(adjust_acc = TRUE, acc_lower = 0.2,
                    acc_upper = 0.4, acc_change = 0.1)
resultados_bvar <- diagnosticos <- ordens_arima <- list()

for (i in seq_along(janelas)) {
  treino <- cv_data |>
    filter(.id == janelas[i]) |>
    as_tibble() |>
    arrange(date)
  origem <- max(treino$date)
  teste <- dados_cv |>
    filter(date > origem, date <= origem + h) |>
    as_tibble()
  if (!nrow(teste)) next
  H <- nrow(teste)
  message("JANELA ", i, "/", length(janelas), " | origem ", origem, " | H observado ", H)

  # Modelo 2: lags fixados por solicitacao do usuario, escolhidos ate jul/2024.
  # Ordens dos erros fixas: IP (0,1,1), VIX (3,0,0); reestimar somente coeficientes.
  treino_externo <- treino |> select(-.id) |> as_tsibble(index = date)
  fit_ip <- treino_externo |>
    model(reg_arima = ARIMA(ip_mundial ~ 0 + brent + lag(brent, 1) + pdq(0, 1, 1) + PDQ(0, 0, 0)))
  fit_vix <- treino_externo |>
    model(reg_arima = ARIMA(vix ~ 1 + brent + lag(brent, 1) + lag(brent, 2) +
                              lag(brent, 3) + lag(brent, 4) + pdq(3, 0, 0) + PDQ(0, 0, 0)))
  futuro_externo <- teste |> select(date, brent) |> as_tsibble(index = date)
  prev_ip <- fit_ip |> forecast(new_data = futuro_externo)
  prev_vix <- fit_vix |> forecast(new_data = futuro_externo)
  caminhos_9 <- cbind(ust2y = teste$ust2y, ip_mundial = prev_ip$.mean,
                      brent = teste$brent, vix = prev_vix$.mean)
  stopifnot(nrow(caminhos_9) == H, all(is.finite(caminhos_9)),
            identical(prev_ip$date, teste$date), identical(prev_vix$date, teste$date))
  for (alvo in c("ip_mundial", "vix")) {
    ajuste <- if (alvo == "ip_mundial") fit_ip$reg_arima[[1]]$fit else fit_vix$reg_arima[[1]]$fit
    ordens_arima[[paste(i, alvo)]] <- tibble(
      .id = janelas[i], origem = origem, variavel = alvo,
      p = ajuste$spec$p, d = ajuste$spec$d, q = ajuste$spec$q,
      P = ajuste$spec$P, D = ajuste$spec$D, Q = ajuste$spec$Q,
      AICc = ajuste$fit$AICc)
  }

  for (m in modelos_cv) {
    inicio <- Sys.time()
    message("INICIO ", nomes_bvar[m], " | janela ", i)
    if (m <= 2) {
      # Foreign primeiro, seguido pelas cinco variaveis domesticas.
      externas <- if (m == 1) c("ust2y", "brent") else c("ust2y", "ip_mundial", "brent", "vix")
      domesticas <- c("atividade", "cpi_core", "juro_curto", "inclinacao", "usdjpy")
      dados_treino <- treino |> select(all_of(c(externas, domesticas))) |> as.matrix()
      N <- ncol(dados_treino)
      E <- length(externas)
      caminhos <- if (m == 1) as.matrix(select(teste, ust2y, brent)) else caminhos_9
      condicoes <- cbind(caminhos, matrix(NA_real_, H, N - E))
      inicio_covid <- match(as.Date("2020-03-01"), as.Date(treino$date)) - p
      if (is.na(inicio_covid) || inicio_covid <= 0) inicio_covid <- NULL
      if (!is.null(inicio_covid) && nrow(treino) - p - inicio_covid + 1L < 4L) inicio_covid <- NULL
      set.seed(123 + i + 1000 * m)
      spec_cv <- bsvarSIGNs::specify_bsvarSIGN$new(
        data = dados_treino[, (E + 1):N], foreign = dados_treino[, 1:E], p = p,
        hyper_mu = FALSE, hyper_delta = FALSE, hyper_lambda = FALSE,
        hyper_psi = FALSE, hyper_covid = inicio_covid,
        sign_structural = matrix(NA_real_, N, N))
      if (!is.null(inicio_covid)) spec_cv$estimate_hyper(S = 5000, burn_in = 2500)
      post_cv <- bsvars::estimate(spec_cv, S = S, show_progress = FALSE)
      coeficientes <- matrix(post_cv$posterior$A, ncol = S)
      lags_domesticos <- unlist(lapply(0:(p - 1), function(j) (E + 1):N + N * j))
      diagnostico <- tibble(
        .id = janelas[i], origem = origem, .model = nomes_bvar[m],
        finitos = all(is.finite(coeficientes)) && all(is.finite(post_cv$posterior$Sigma)),
        distintos = ncol(unique(coeficientes, MARGIN = 2)),
        ess_pesos = post_cv$posterior$ess, ess_hyper = NA_real_,
        zero_lags = max(abs(post_cv$posterior$A[1:E, lags_domesticos, ])),
        zero_impacto = max(abs(post_cv$posterior$Theta0[1:E, (E + 1):N, ])))
      print(diagnostico)
      stopifnot(diagnostico$finitos, is.finite(diagnostico$ess_pesos),
                diagnostico$ess_pesos >= 50, diagnostico$distintos >= 50,
                diagnostico$zero_lags < 1e-8, diagnostico$zero_impacto < 1e-8)
      set.seed(200000 + i + 1000 * m)
      previsao_cv <- bsvarSIGNs::forecast(post_cv, horizon = H, conditional_forecast = condicoes)

      # Mesma correcao em R do CV original, adaptada as condicoes externas.
      choques_cv <- previsao_cv$forecasts - previsao_cv$forecast_mean
      estado_inicial <- c(t(dados_treino[nrow(dados_treino) - (0:(p - 1)), , drop = FALSE]), 1)
      for (s in seq_len(S)) {
        estado <- estado_inicial
        sigma <- post_cv$posterior$Sigma[, , s]
        ganho <- sigma[, 1:E, drop = FALSE] %*% solve(sigma[1:E, 1:E, drop = FALSE])
        for (j in seq_len(H)) {
          media <- drop(post_cv$posterior$A[, , s] %*% estado)
          ajuste_externo <- media[1:E] - previsao_cv$forecast_mean[1:E, j, s]
          previsao_cv$forecast_mean[, j, s] <- media
          previsao_cv$forecasts[, j, s] <- media + choques_cv[, j, s] - drop(ganho %*% ajuste_externo)
          previsao_cv$forecasts[1:E, j, s] <- caminhos[j, ]
          estado <- c(previsao_cv$forecasts[, j, s], head(estado, N * (p - 1)), 1)
        }
      }
      F <- previsao_cv$forecasts
      stopifnot(max(abs(sweep(F[1:E, , , drop = FALSE], c(1, 2), t(caminhos), "-"))) < 1e-8)
    } else {
      # Modelo 3: retirar somente Treasury e petroleo. Sem condicoes futuras.
      dados_treino <- treino |>
        select(ip_mundial, vix, atividade, cpi_core, juro_curto, inclinacao, usdjpy) |>
        as.matrix()
      set.seed(123 + i + 1000 * m)
      post_bvar <- BVAR::bvar(dados_treino, lags = p, n_draw = 5000, n_burn = 2500,
                              n_thin = 1, priors = priors_cv, mh = mh_cv, verbose = FALSE)
      ess_hyper <- min(coda::effectiveSize(coda::mcmc(post_bvar$hyper)))
      diagnostico <- tibble(
        .id = janelas[i], origem = origem, .model = nomes_bvar[m],
        finitos = length(post_bvar$beta) > 0 && length(post_bvar$sigma) > 0 &&
          all(is.finite(post_bvar$beta)) && all(is.finite(post_bvar$sigma)),
        distintos = min(apply(post_bvar$hyper, 2, function(x) length(unique(x)))),
        ess_pesos = NA_real_, ess_hyper = ess_hyper,
        zero_lags = NA_real_, zero_impacto = NA_real_)
      print(diagnostico)
      stopifnot(diagnostico$finitos, is.finite(ess_hyper), ess_hyper >= 50,
                diagnostico$distintos >= 50)
      set.seed(200000 + i + 1000 * m)
      previsao_bvar <- stats::predict(post_bvar, horizon = H)
      F <- aperm(previsao_bvar$fcast, c(3, 2, 1))
    }
    stopifnot(all(is.finite(F)), dim(F)[2] == H, dim(F)[3] == S)
    diagnostico$segundos <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))
    diagnosticos[[paste(i, m)]] <- diagnostico
    for (alvo in c("atividade", "cpi_core")) {
      posicao <- match(alvo, colnames(dados_treino))
      sorteios <- matrix(F[posicao, , ], nrow = H)
      resultados_bvar[[paste(i, m, alvo)]] <- tibble(
        variavel = alvo, .id = janelas[i], .model = nomes_bvar[m],
        date = teste$date, h = seq_len(H),
        valor = dist_sample(split(sorteios, row(sorteios))))
    }
    message("FIM ", nomes_bvar[m], " | janela ", i, " | segundos ", round(diagnostico$segundos, 1))
    rm(F)
    if (exists("previsao_cv")) rm(previsao_cv, post_cv, choques_cv)
    if (exists("previsao_bvar")) rm(previsao_bvar, post_bvar)
    gc(verbose = FALSE)
  }
}

# 3. Mesmos casos nos sete modelos e nos dois alvos.
# Compatibilidade: o CRPS do fable espera os elementos internos de dist_sample.
crps_cv <- function(.dist, .actual) {
  amostral <- vapply(vctrs::vec_data(.dist), inherits, logical(1), "dist_sample")
  stopifnot(all(amostral) || !any(amostral))
  if (all(amostral)) .dist <- vctrs::vec_data(.dist)
  fabletools::CRPS(.dist = .dist, .actual = .actual)
}
# Padronizar os nomes antes de combinar distribuicoes de origens diferentes.
dimnames(previsoes_refs$valor) <- NULL
previsoes <- bind_rows(previsoes_refs, previsoes_var_alvos,
                       bind_rows(resultados_bvar) |> mutate(.mean = mean(valor))) |>
  select(variavel, .id, .model, date, h, valor, .mean)
dimnames(previsoes$valor) <- "valor"
casos <- previsoes |> count(variavel, .id, date, h, name = "modelos")
stopifnot(all(casos$modelos == 4L + length(modelos_cv)), nrow(casos) > 0,
          n_distinct(previsoes$.model) == 4L + length(modelos_cv), all(is.finite(previsoes$.mean)))
origens <- cv_data |> as_tibble() |> summarise(origem = max(date), .by = .id)
previsoes <- previsoes |>
  left_join(origens, by = ".id") |>
  mutate(periodo = case_when(year(date) <= 2019 ~ "2015-2019",
                            year(date) <= 2021 ~ "2020-2021", TRUE ~ "2022 em diante")) |>
  as_tsibble(key = c(variavel, .id, .model), index = date) |>
  as_fable(response = "valor", distribution = "valor")

# Parear os observados; calcular o CRPS oficial do fable em cada grupo de avaliacao.
pontuacoes <- as_tibble(previsoes) |>
  left_join(as_tibble(observados) |> rename(observado = valor), by = c("variavel", "date")) |>
  mutate(erro = observado - .mean)
stopifnot(nrow(pontuacoes) == nrow(previsoes), all(is.finite(pontuacoes$erro)))

# H12/H24 acumulados, como o CV original; guardar tambem os horizontes exatos.
metricas_acumuladas <- bind_rows(
  pontuacoes |> filter(h <= 12) |> mutate(horizonte = "H1-H12"),
  pontuacoes |> filter(h <= 24) |> mutate(horizonte = "H1-H24")
) |>
  summarise(RMSE = sqrt(mean(erro^2)), MAE = mean(abs(erro)), CRPS = crps_cv(valor, observado),
            casos = n(), .by = c(variavel, horizonte, .model)) |>
  arrange(variavel, horizonte, RMSE)
metricas_exatas <- pontuacoes |>
  filter(h %in% c(12, 24)) |>
  summarise(RMSE = sqrt(mean(erro^2)), MAE = mean(abs(erro)), CRPS = crps_cv(valor, observado),
            casos = n(), .by = c(variavel, h, .model)) |>
  arrange(variavel, h, RMSE)
metricas_periodos <- pontuacoes |>
  summarise(RMSE = sqrt(mean(erro^2)), MAE = mean(abs(erro)), CRPS = crps_cv(valor, observado),
            casos = n(), .by = c(variavel, periodo, .model)) |>
  arrange(variavel, periodo, RMSE)
stopifnot(all(is.finite(metricas_acumuladas$CRPS)), all(is.finite(metricas_exatas$CRPS)),
          all(is.finite(metricas_periodos$CRPS)))
print(metricas_acumuladas, n = Inf)
print(metricas_exatas, n = Inf)

# 4. Arquivos finais: nenhuma janela intermediaria e salva.
diagnosticos <- bind_rows(diagnosticos)
ordens_arima <- bind_rows(ordens_arima)
write_csv(metricas_acumuladas, file.path(saida_cv, "metricas_h12_h24.csv"))
write_csv(metricas_exatas, file.path(saida_cv, "metricas_horizontes_exatos.csv"))
write_csv(metricas_periodos, file.path(saida_cv, "metricas_por_periodo_h24.csv"))
write_csv(diagnosticos, file.path(saida_cv, "diagnosticos.csv"))
write_csv(ordens_arima, file.path(saida_cv, "ordens_arima.csv"))
write_csv(select(pontuacoes, -valor), file.path(saida_cv, "pontuacoes.csv"))
saveRDS(list(previsoes = previsoes, metricas = metricas_acumuladas,
             horizontes_exatos = metricas_exatas, periodos = metricas_periodos,
             diagnosticos = diagnosticos, ordens_arima = ordens_arima),
        file.path(saida_cv, "resultados_finais.rds"))

# 5. Graficos: benchmarks em azuis e tres BVARs em cores contrastantes.
cores <- c(ar = "#355C7D", naive = "#668BA8", drift = "#92AEC4", var = "#B9CAD8",
           "BVAR 7 | petroleo/UST" = "#D96830", "BVAR 9 | ARIMA externos" = "#A44725",
           "BVAR 7 | hiper estimados" = "#DDA936")
for (alvo in c("cpi_core", "atividade")) {
  titulo <- if (alvo == "cpi_core") "CPI core" else "Atividade"
  grafico <- metricas_acumuladas |>
    filter(variavel == alvo, horizonte == "H1-H24") |>
    pivot_longer(c(RMSE, CRPS), names_to = "metrica", values_to = "valor") |>
    mutate(.model = factor(.model, levels = c("var", "drift", "naive", "ar", nomes_bvar)),
           metrica = factor(metrica, levels = c("RMSE", "CRPS"))) |>
    ggplot(aes(valor, .model, fill = .model)) +
    geom_col(width = .62) +
    geom_text(aes(label = formatC(valor, format = "f", digits = 2, decimal.mark = ",")),
              hjust = -.15, size = 3.5) +
    facet_wrap(vars(metrica), scales = "free_x", ncol = 2) +
    scale_fill_manual(values = cores) +
    scale_x_continuous(expand = expansion(mult = c(0, .25))) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold"), plot.margin = margin(12, 25, 12, 12)) +
    labs(title = paste("CV trimestral -", titulo),
         subtitle = "Horizontes observados de 1 a 24 meses reunidos - Menor e melhor",
         caption = "BVARs 1 e 2 condicionais ex post; lags do modelo 2 fixados apos selecao retrospectiva.",
         x = NULL, y = NULL)
  print(grafico)
  ggsave(file.path(saida_cv, paste0(alvo, "_h24.png")), grafico,
         width = 12, height = 5.8, dpi = 180, bg = "white")

  grafico_periodos <- metricas_periodos |>
    filter(variavel == alvo) |>
    pivot_longer(c(RMSE, CRPS), names_to = "metrica", values_to = "valor") |>
    mutate(.model = factor(.model, levels = c("var", "drift", "naive", "ar", nomes_bvar)),
           metrica = factor(metrica, levels = c("RMSE", "CRPS")),
           periodo = factor(periodo, levels = c("2015-2019", "2020-2021", "2022 em diante"))) |>
    ggplot(aes(valor, .model, fill = .model)) +
    geom_col(width = .62) +
    geom_text(aes(label = formatC(valor, format = "f", digits = 2, decimal.mark = ",")),
              hjust = -.15, size = 3.1) +
    facet_wrap(vars(periodo, metrica), scales = "free_x", ncol = 2) +
    scale_fill_manual(values = cores) +
    scale_x_continuous(expand = expansion(mult = c(0, .25))) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none", panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold"), plot.margin = margin(12, 25, 12, 12)) +
    labs(title = paste("CV trimestral por periodo -", titulo),
         subtitle = "Periodo da data prevista - H1 a H24 observados reunidos",
         caption = "BVARs 1 e 2 condicionais ex post; lags do modelo 2 fixados apos selecao retrospectiva.",
         x = NULL, y = NULL)
  print(grafico_periodos)
  ggsave(file.path(saida_cv, paste0(alvo, "_periodos_h24.png")), grafico_periodos,
         width = 12, height = 12, dpi = 180, bg = "white")
}
message("CONCLUIDO | ", nrow(diagnosticos), " ajustes BVAR | ", nrow(pontuacoes), " casos avaliados")
