# Comparações do CV do modelo atual com produção mundial, tendência e JGB30Y−5Y.
# Executar depois do CV e de previsao historica BVAR 2022 a 2026.R.
library(tidyverse)
library(fpp3)
library(distributional)

# 1. Resultados e benchmarks nas mesmas janelas do CV original.
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
saida <- file.path(raiz, 'src/Outputs/2- Evaluation/Desempenho BVAR reduzido/CV pesos producao mundial JGB30Y-5Y')
cv <- readRDS(file.path(saida, 'cv completo.rds'))
stopifnot(identical(cv$configuracao$inclinacao, '30Y-5Y'))
dados <- cv$dados
domesticas <- c('atividade', 'cpi_core', 'juro_curto', 'inclinacao', 'usdjpy')
nomes <- c(atividade = 'Atividade japonesa', cpi_core = 'CPI core', juro_curto = 'Juro curto',
  inclinacao = 'Spread JGB 30a menos 5a', usdjpy = 'USD/JPY')
unidades <- c(atividade = 'Erro em 100 × log do índice', cpi_core = 'Erro em 100 × log do índice',
  juro_curto = 'Pontos percentuais', inclinacao = 'Pontos percentuais', usdjpy = 'Erro em 100 × log do câmbio')
origens <- cv$cv_data |> as_tibble() |> group_by(.id) |> summarise(origem = max(date), .groups = 'drop')
bench <- list()
for (alvo in domesticas) {
  ajuste <- cv$cv_data |> select(.id, date, valor = all_of(alvo)) |> model(
    AR = ARIMA(valor ~ pdq(d = 1, q = 0) + PDQ(D = 0, Q = 0)),
    Naive = NAIVE(valor), Drift = RW(valor ~ drift()))
  bench[[alvo]] <- ajuste |> forecast(h = 24) |> as_tibble() |>
    transmute(.id, .model, date, variavel = alvo, valor = unname(valor), .mean)
}
ajuste_var <- cv$cv_data |> model(VAR = VAR(vars(atividade, cpi_core, juro_curto)))
prev_var <- ajuste_var |> forecast(h = 24)
parametros <- parameters(prev_var$.distribution)
bench_var <- map_dfr(seq_len(3), function(k) {
  tibble(.id = prev_var$.id, .model = 'VAR', date = prev_var$date,
    variavel = c('atividade', 'cpi_core', 'juro_curto')[k],
    valor = dist_normal(mu = map_dbl(parametros$mu, k),
      sigma = sqrt(map_dbl(parametros$sigma, ~ .x[k, k])))) |>
    mutate(.mean = mean(valor))
})
bvar <- cv$previsoes |> as_tibble() |>
  transmute(.id, .model = 'BVAR ex post', date, variavel, valor = unname(valor), .mean)
previsoes <- bind_rows(bvar, bind_rows(bench), bench_var) |>
  filter(date <= max(dados$date)) |> left_join(origens, by = '.id') |>
  mutate(h = as.integer(date - origem), periodo = case_when(year(date) <= 2019 ~ '2015–2019',
    year(date) <= 2021 ~ '2020–2021', TRUE ~ '2022–2026'))
esperados <- tibble(variavel = domesticas, n_esperado = c(5L, 5L, 5L, 4L, 4L))
contagens <- previsoes |> count(variavel, .id, date) |> left_join(esperados, by = 'variavel')
stopifnot(all(contagens$n == contagens$n_esperado), all(previsoes$h >= 1), all(previsoes$h <= 24))
observados <- dados |> as_tibble() |> select(date, all_of(domesticas)) |>
  pivot_longer(-date, names_to = 'variavel', values_to = 'valor') |>
  as_tsibble(key = variavel, index = date)
# CRPS empirico pela identidade ordenada, sem construir matrizes 2500 x 2500.
# E a mesma metrica aplicada as mesmas distribuicoes, com calculo mais leve.
observados_tabela <- observados |> as_tibble() |> rename(observado = valor)
escores <- previsoes |> left_join(observados_tabela, by = c('date', 'variavel'))
param_bvar <- parameters(escores$valor[escores$.model == 'BVAR ex post'])$x
idx_bvar <- which(escores$.model == 'BVAR ex post')
escores$crps <- NA_real_
escores$crps[idx_bvar] <- map2_dbl(param_bvar, escores$observado[idx_bvar], function(x, y) {
  n <- length(x)
  mean(abs(x - y)) - sum((2 * seq_len(n) - n - 1) * sort(x)) / n^2
})
idx_normal <- which(escores$.model != 'BVAR ex post')
param_normal <- parameters(escores$valor[idx_normal])
z <- (escores$observado[idx_normal] - param_normal$mu) / param_normal$sigma
escores$crps[idx_normal] <- ifelse(param_normal$sigma > 0,
  param_normal$sigma * (z * (2 * pnorm(z) - 1) + 2 * dnorm(z) - 1 / sqrt(pi)),
  abs(escores$observado[idx_normal] - param_normal$mu))
escores <- escores |> mutate(erro = .mean - observado)
geral <- escores |> group_by(variavel, .model) |>
  summarise(RMSE = sqrt(mean(erro^2)), MAE = mean(abs(erro)), CRPS = mean(crps), .groups = 'drop') |> mutate(periodo = 'Geral')
periodos <- escores |> group_by(variavel, periodo, .model) |>
  summarise(RMSE = sqrt(mean(erro^2)), MAE = mean(abs(erro)), CRPS = mean(crps), .groups = 'drop')
metricas <- bind_rows(geral, periodos) |> select(variavel, periodo, .model, RMSE, MAE, CRPS) |>
  arrange(variavel, periodo, RMSE)
verificacao <- geral |> filter(.model == 'BVAR ex post') |>
  inner_join(cv$todas |> as_tibble() |> select(variavel, RMSE_original = RMSE, MAE_original = MAE, CRPS_original = CRPS), by = 'variavel')
stopifnot(max(abs(verificacao$RMSE - verificacao$RMSE_original)) < 1e-8,
  max(abs(verificacao$MAE - verificacao$MAE_original)) < 1e-8,
  max(abs(verificacao$CRPS - verificacao$CRPS_original)) < .01)
# No relatorio, preservar o CRPS calculado por accuracy() no CV original.
crps_original <- bind_rows(cv$todas |> as_tibble() |> mutate(periodo = 'Geral'),
  cv$todas_periodos |> as_tibble() |> mutate(periodo = recode(periodo,
    '2015-2019' = '2015–2019', '2020-2021' = '2020–2021', '2022 em diante' = '2022–2026'))) |>
  select(variavel, periodo, CRPS_original = CRPS)
metricas <- metricas |> left_join(crps_original, by = c('variavel', 'periodo')) |>
  mutate(CRPS = if_else(.model == 'BVAR ex post', CRPS_original, CRPS)) |> select(-CRPS_original)
utils::write.csv(verificacao, file.path(saida, 'verificacao metricas.csv'), row.names = FALSE)
stopifnot(all(is.finite(as.matrix(select(metricas, RMSE, MAE, CRPS)))))
utils::write.csv(metricas, file.path(saida, 'comparacao benchmarks geral e periodos.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
saveRDS(list(previsoes = previsoes, metricas = metricas, origens = origens,
  contagens = contagens), file.path(saida, 'comparacao benchmarks.rds'))

# 2. Mesmo eixo de erro entre os periodos de cada variavel.
cores <- c('BVAR ex post' = '#2563A6', AR = '#C37B2E', Naive = '#70836B', Drift = '#999999', VAR = '#8563A0')
for (alvo in domesticas) {
  grafico <- metricas |> filter(variavel == alvo) |>
    mutate(periodo = factor(periodo, levels = c('Geral', '2015–2019', '2020–2021', '2022–2026')),
      .model = factor(.model, levels = rev(names(cores)))) |>
    ggplot(aes(RMSE, .model, fill = .model)) + geom_col(width = .6) +
    geom_text(aes(label = formatC(RMSE, digits = 2, format = 'f', decimal.mark = ',')),
      hjust = -.15, size = 4.5) + facet_wrap(vars(periodo), ncol = 2) +
    scale_fill_manual(values = cores) + scale_x_continuous(expand = expansion(mult = c(0, .24))) +
    theme_minimal(base_size = 15) + theme(legend.position = 'none', panel.spacing.x = unit(.8, 'cm'), panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(), plot.title = element_text(face = 'bold')) +
    labs(title = paste('Validação temporal', nomes[alvo], sep = ' | '), x = paste('RMSE |', unidades[alvo]), y = NULL,
      subtitle = 'Janelas expansivas | Reestimação a cada 3 meses | Horizontes de 1 a 24 meses',
      caption = 'Menor é melhor. BVAR recebe Treasury, Brent e produção mundial realizados; benchmarks não recebem esses caminhos.')
  ggsave(file.path(saida, paste0('cv ', alvo, '.png')), grafico, width = 10.4, height = 6.6, dpi = 180, bg = 'white')
}

# 3. Previsoes na pratica: blocos de 24 meses das mesmas janelas do CV.
# Origens fixadas antes de comparar os erros, sem escolher os melhores episodios.
selecionadas <- origens |> filter(origem %in% yearmonth(c('2021 Dec', '2023 Dec', '2025 Dec')))
pratica <- previsoes |> filter(.id %in% selecionadas$.id, .model %in% c('BVAR ex post', 'AR', 'Naive'))
historico <- dados |> as_tibble() |> select(date, all_of(domesticas)) |>
  pivot_longer(-date, names_to = 'variavel', values_to = 'valor')
transformar <- function(tabela) {
  tabela |> arrange(date) |> mutate(valor = if (first(variavel) %in% c('atividade', 'cpi_core'))
    100 * expm1((valor - lag(valor, 12)) / 100) else if (first(variavel) == 'usdjpy') exp(valor / 100) else valor)
}
hist_plot <- historico |> group_by(variavel) |> group_modify(~ transformar(.x |> mutate(variavel = .y$variavel)) |> select(-variavel)) |>
  ungroup() |> filter(date >= yearmonth('2020 Jan'))
pratica_plot <- pratica |> group_by(variavel, .id, .model) |> group_modify(function(.x, .y) {
  origem <- first(.x$origem)
  hist <- historico |> filter(variavel == .y$variavel, date <= origem) |> select(date, valor)
  unido <- bind_rows(hist, .x |> transmute(date, valor = .mean)) |>
    mutate(variavel = .y$variavel) |> transformar() |> filter(date > origem)
  unido |> select(date, valor)
}) |> ungroup()
unidades_plot <- c(atividade = 'Variação em 12 meses (%)', cpi_core = 'Inflação em 12 meses (%)',
  juro_curto = '% a.a.', inclinacao = 'Pontos percentuais', usdjpy = 'Ienes por dólar')
for (alvo in domesticas) {
  g <- ggplot() + geom_line(data = filter(hist_plot, variavel == alvo), aes(as.Date(date), valor, colour = 'Observado'), linewidth = .9) +
    geom_line(data = filter(pratica_plot, variavel == alvo), aes(as.Date(date), valor, colour = .model, group = interaction(.id, .model)), linewidth = .8) +
    geom_vline(xintercept = as.Date(c('2022-01-01', '2024-01-01', '2026-01-01')), linetype = 'dotted', colour = '#999999') +
    scale_colour_manual(values = c(cores, Observado = '#171717')) +
    scale_x_date(limits = as.Date(c('2020-01-01', '2026-08-01')), date_breaks = '1 year', date_labels = '%Y') +
    theme_minimal(base_size = 13) + theme(legend.position = 'bottom', panel.grid.minor = element_blank(), plot.title = element_text(face = 'bold')) +
    labs(title = paste(nomes[alvo], 'previsões contra os dados observados', sep = ' | '), x = NULL, y = unidades_plot[alvo], colour = NULL,
      subtitle = 'Previsões feitas em dez/2021, dez/2023 e dez/2025 | Até 24 meses por bloco',
      caption = 'Linhas pontilhadas iniciam cada previsão. BVAR ex post conhece os caminhos externos realizados. Dados revisados da amostra atual.')
  ggsave(file.path(saida, paste0('pratica ', alvo, '.png')), g, width = 11, height = 4.6, dpi = 180, bg = 'white')
}

# 4. Origem unica no fim de 2021, sem reestimar ate agosto de 2026.
fixa <- readRDS(file.path(saida, 'previsao origem fixa 2021.rds'))
centrais <- map_dfr(names(fixa$trajetorias), function(tipo) {
  F <- fixa$trajetorias[[tipo]]
  map_dfr(seq_along(domesticas), function(k) tibble(date = fixa$teste$date,
    variavel = domesticas[k], .model = tipo, valor = rowMeans(matrix(F[k + 3L, , ], nrow = nrow(fixa$teste)))))
})
centrais <- bind_rows(centrais, fixa$benchmarks |> filter(.model %in% c('AR', 'Naive')) |>
  transmute(date, variavel, .model, valor = .mean))
fixa_plot <- centrais |> group_by(variavel, .model) |> group_modify(function(.x, .y) {
  hist <- historico |> filter(variavel == .y$variavel, date <= fixa$origem) |> select(date, valor)
  bind_rows(hist, .x |> select(date, valor)) |> mutate(variavel = .y$variavel) |>
    transformar() |> filter(date > fixa$origem) |> select(date, valor)
}) |> ungroup()
saveRDS(list(hist = hist_plot, pratica = pratica_plot, fixa = fixa_plot), file.path(saida, 'series graficos.rds'))
for (alvo in domesticas) {
  g <- ggplot() + geom_line(data = filter(hist_plot, variavel == alvo), aes(as.Date(date), valor, colour = 'Observado'), linewidth = .9) +
    geom_line(data = filter(fixa_plot, variavel == alvo), aes(as.Date(date), valor, colour = .model), linewidth = .8) +
    geom_vline(xintercept = as.Date('2022-01-01'), linetype = 'dotted', colour = '#888888') +
    scale_colour_manual(values = c(cores, 'BVAR sem externas realizadas' = '#AD4D4D', Observado = '#171717')) +
    scale_x_date(limits = as.Date(c('2020-01-01', '2026-08-01')), date_breaks = '1 year', date_labels = '%Y') +
    theme_minimal(base_size = 13) + theme(legend.position = 'bottom', panel.grid.minor = element_blank(), plot.title = element_text(face = 'bold')) +
    guides(colour = guide_legend(nrow = 2)) +
    labs(title = paste(nomes[alvo], 'previsão feita no fim de 2021', sep = ' | '), x = NULL, y = unidades_plot[alvo], colour = NULL,
      subtitle = 'Treino de jan/2000 a dez/2021 | Previsão de jan/2022 a ago/2026 | Sem reestimação',
      caption = 'Azul: três caminhos externos realizados. Vermelho: nenhum dado futuro na estimação ou no condicionamento. Séries revisadas, não vintages históricos.')
  ggsave(file.path(saida, paste0('origem fixa ', alvo, '.png')), g, width = 11, height = 4.9, dpi = 180, bg = 'white')
}
utils::write.csv(fixa_plot |> mutate(date = as.Date(date)), file.path(saida, 'trajetorias origem fixa.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
message('GRAFICOS_E_BENCHMARKS_CONCLUIDOS')
