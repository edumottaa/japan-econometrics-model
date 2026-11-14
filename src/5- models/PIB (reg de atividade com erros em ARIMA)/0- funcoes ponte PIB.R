# Funções compartilhadas pelo teste de lags e pelo CV das pontes do PIB.
# Mesma amostra por candidato; nenhuma seleção usa as observações do teste.

library(fpp3)
library(tidyverse)

# 1. KPSS: H0 = estacionariedade. Usar explicitamente lags curtos em cada etapa.
# ADF é diagnóstico complementar: H0 = raiz unitária; discordância é inconclusiva.
diferencas_pib <- function(x) {
  x <- x[is.finite(x)]
  for (d in 0:2) {
    z <- if (d == 0L) x else diff(x, differences = d)
    if (feasts::unitroot_kpss(z, lags = "short")[["kpss_pvalue"]] >= .05) return(d)
  }
  stop("KPSS ainda rejeita após duas diferenças; rever a série antes de estimar.")
}

testar_series_pib <- function(dados) {
  variaveis <- intersect(c("pib", "atividade", "pib_crescimento", "atividade_crescimento"), names(dados))
  map_dfr(variaveis, function(nome) {
    x <- dados[[nome]]; x <- x[is.finite(x)]
    kpss_resultado <- feasts::unitroot_kpss(x, lags = "short")
    adf_resultado <- urca::ur.df(x, type = "drift", lags = 4, selectlags = "AIC")
    tibble(variavel = nome, n = length(x), kpss = kpss_resultado[["kpss_stat"]],
      kpss_p = kpss_resultado[["kpss_pvalue"]], adf = as.numeric(adf_resultado@teststat[1, "tau2"]),
      adf_critico_5 = adf_resultado@cval["tau2", "5pct"],
      adf_rejeita_raiz = adf < adf_critico_5,
      diferencas_kpss = diferencas_pib(x), D_sugerido = feasts::unitroot_nsdiffs(x)[[1]])
  })
}

# 2. Diferenciação escolhida ANTES de comparar AICc.
# Manter a janela usada no CV anterior: seis observações iniciais fora da resposta.
# Assim, retirar a ponte com juros não muda a estimação nem os resultados da simples.
preparar_treino_ponte <- function(dados, max_lag = 4L) {
  d <- max(diferencas_pib(dados$pib_crescimento), diferencas_pib(dados$atividade_crescimento))
  dados <- dados |> mutate(pib_crescimento = if_else(row_number() <= max_lag + 2L,
    NA_real_, pib_crescimento))
  list(dados = dados, d = d)
}

# 3. Atividade: trimestre atual e até 2 anteriores.
# p,q = 0:2; busca exaustiva por AICc, sem aproximação; D = 0 (séries ajustadas).
# AICc só é comparado dentro de uma mesma transformação de y e amostra.
ajustar_pontes_pib <- function(dados) {
  preparo <- preparar_treino_ponte(dados)
  base <- preparo$dados
  grade <- tibble(familia = "atividade", lag_atividade = 0:2) |>
    mutate(especificacao = paste0("atividade_a", lag_atividade))
  ajustes <- vector("list", nrow(grade))
  for (i in seq_len(nrow(grade))) {
    termos <- c("atividade_crescimento", if (grade$lag_atividade[i] > 0L)
      paste0("lag(atividade_crescimento, ", seq_len(grade$lag_atividade[i]), ")"))
    formula <- as.formula(paste("pib_crescimento ~ 1 +", paste(termos, collapse = " + "),
      "+ pdq(p = 0:2, d =", preparo$d, ", q = 0:2) + PDQ(0, 0, 0)"))
    ajustes[[i]] <- base |> model(arima = ARIMA(!!formula, stepwise = FALSE, approximation = FALSE))
  }
  tabela <- map2_dfr(ajustes, seq_along(ajustes), function(ajuste, i) {
    glance(ajuste) |> transmute(indice = i, AICc, BIC, log_lik, sigma2)
  }) |> left_join(grade |> mutate(indice = row_number()), by = "indice") |>
    filter(is.finite(AICc)) |>
    mutate(n_termos = lag_atividade + 1L) |>
    group_by(familia) |> mutate(delta_AICc = AICc - min(AICc)) |> ungroup() |>
    arrange(familia, AICc)
  stopifnot(n_distinct(tabela$familia) == 1L)
  # Entre candidatos a menos de 2 pontos do mínimo, preferir menos preditores.
  escolhidos <- tabela |> filter(delta_AICc < 2) |>
    group_by(familia) |> arrange(n_termos, AICc, .by_group = TRUE) |>
    slice_head(n = 1L) |> ungroup() |>
    mutate(d = preparo$d, D = 0L, n_treino = sum(is.finite(base$pib_crescimento)))
  modelos <- set_names(map(escolhidos$indice, ~ ajustes[[.x]]), escolhidos$familia)
  list(modelos = modelos, tabela = tabela, escolhidos = escolhidos, preparo = preparo)
}

# 4. Agregar a atividade prevista nas janelas salvas do CV do BVAR.
preparar_futuro_pontes <- function(historico, previsoes, ultimo_pib, log_bvar = TRUE) {
  # O CV guarda 100 * log(atividade); o forecast final já guarda o índice em nível.
  futuro <- previsoes |> as_tibble() |> filter(variavel == "atividade") |> arrange(date)
  futuro <- futuro |> transmute(date, atividade = if (log_bvar)
    map_dbl(parameters(valor)$x, ~ mean(exp(.x / 100))) else mean(valor))

  bind_rows(historico |> as_tibble() |> select(date, atividade) |>
      filter(date < min(futuro$date)), futuro) |>
    arrange(date) |>
    as_tsibble(index = date) |>
    index_by(trimestre = yearquarter(date)) |>
    summarise(meses = n(), atividade = mean(atividade)) |>
    rename(date = trimestre) |>
    filter(meses == 3L) |>
    mutate(atividade_crescimento = 100 * (atividade / lag(atividade) - 1)) |>
    filter(date > ultimo_pib) |>
    select(date, atividade_crescimento)
}
