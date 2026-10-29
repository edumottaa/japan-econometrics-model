# Previsoes condicionais do BVAR estrutural nos quatro cenarios.

library(fpp3)
library(tidyverse)
library(distributional)
library(bsvars)
library(bsvarSIGNs)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
saida <- "src/Outputs/Forecast BVAR estrutural/pesos exponenciais"
dir.create(saida, recursive = TRUE, showWarnings = FALSE)



# 1. Reutilizar o modelo estimado e transformar suas informações em um tibble
if (!exists("post_boj") || !exists("dados_boj")) {
  stop("Execute o BVAR estrutural com os dados domesticos atualizados antes do forecast.")
}
variaveis <- c("ust2y", "brent", "atividade", "cpi_core", "juro_curto", "inclinacao", "usdjpy")
variaveis_domesticas <- variaveis[3:7]
dados_modelo <- dados_boj |>
  as_tibble() |>
  select(date, all_of(variaveis)) |>
  arrange(date) |>
  as_tsibble(index = date)
matriz_modelo <- dados_modelo |> as_tibble() |> select(-date) |> as.matrix()
p <- as.integer(post_boj$last_draw$p)   #p: num de defasagens
S <- dim(post_boj$posterior$A)[3]       #s: num de sorteios para hiperparametros de identificação
N <- length(variaveis)                  # n: num de variaveis
identificacao <- post_boj$last_draw$identification$sign_irf
stopifnot(p == 6L, N == 7L, S >= 50L,
          !any(has_gaps(dados_modelo)$.gaps), all(is.finite(matriz_modelo)),
          identical(dim(post_boj$posterior$A), c(N, N * p + 2L, S)),
          isTRUE(all.equal(unname(post_boj$last_draw$data_matrices$Y),
                           unname(t(matriz_modelo[-seq_len(p), ])))),
          all(is.na(identificacao[, c(3, 4, 7), ])),
          all(vapply(c(1, 2, 5, 6), function(k) any(!is.na(identificacao[, k, ])), logical(1))))

# Mesmos limites numericos da estimacao
coeficientes <- matrix(post_boj$posterior$A, ncol = S)
lags_domesticos <- unlist(lapply(0:(p - 1), function(j) 3:7 + N * j))
stopifnot(all(is.finite(coeficientes)), all(is.finite(post_boj$posterior$Sigma)),
          all(is.finite(post_boj$posterior$Theta0)),
          is.finite(post_boj$posterior$ess), post_boj$posterior$ess >= 50,
          ncol(unique(coeficientes, MARGIN = 2)) >= 50,
          max(abs(post_boj$posterior$A[1:2, lags_domesticos, ])) <= 1e-8,
          max(abs(post_boj$posterior$Theta0[1:2, 3:7, ])) <= 1e-8)



# 2. Preparar os caminhos/cenários das váriaveis externas
source("src/7- forecast/scenarios ust2y/codigos/1- preparar treasury2y.R", local = TRUE, encoding = "UTF-8")
source("src/7- forecast/scenarios brent/codigos/3- feature engineering cenarios brent.R", local = TRUE, encoding = "UTF-8")
saida <- "src/Outputs/Forecast BVAR estrutural/pesos exponenciais"
origem <- max(dados_modelo$date)
h <- 24L
datas_futuras <- origem + seq_len(h)
externas_observadas <- readRDS("data/cache/cache_fred.rds") |>
  transmute(date = yearmonth(date), ust2y_observado = DGS2,
            brent_observado = 100 * log(DCOILBRENTEU)) |>
  filter(date > origem, date < yearmonth(data_curva))

# Dados domesticos ja publicados apos o fim da amostra entram como observacoes
domesticas_observadas <- readRDS("data/cache/cache_domesticas_bvar.rds") |>
  mutate(across(c(atividade, cpi_core, usdjpy), ~ 100 * log(.x))) |>
  filter(date > origem, date < yearmonth(data_curva))

# tabela dos cenários
caminhos_forecast <- crossing(date = datas_futuras, cenario = nomes_cenarios) |>
  left_join(as_tibble(caminho_ust2y), by = "date") |>
  left_join(as_tibble(cenarios_brent_modelo) |> mutate(cenario = as.character(cenario)),
            by = c("date", "cenario")) |>
  left_join(externas_observadas, by = "date") |>
  left_join(domesticas_observadas, by = "date") |>
  mutate(fonte_ust2y = case_when(!is.na(ust2y) ~ "Bloomberg: zero como proxy",
                               !is.na(ust2y_observado) ~ "FRED: observado",
                               TRUE ~ "Livre no BVAR"),
         fonte_brent = case_when(date == yearmonth("2026 Nov") ~ "Hipotese: US$ 111",
                                !is.na(brent) ~ "Cenario Brent",
                                !is.na(brent_observado) ~ "FRED: observado",
                                TRUE ~ "Livre no BVAR"),
         ust2y = coalesce(ust2y, ust2y_observado),
         brent = coalesce(brent, brent_observado)) |>
  select(date, cenario, ust2y, brent, all_of(variaveis_domesticas), fonte_ust2y, fonte_brent) |>
  arrange(cenario, date)

print(caminhos_forecast |> filter(fonte_ust2y == "Livre no BVAR" | fonte_brent == "Livre no BVAR"))



# 3. Forecast
# Continuar a tendência em anos a partir do último mês observado.
tendencia_futura <- matrix(seq_len(h) / 12, ncol = 1)
resultados <- vector("list", length(nomes_cenarios))
checagens <- vector("list", length(nomes_cenarios))
estado_inicial <- c(t(matriz_modelo[nrow(matriz_modelo) - (0:(p - 1)), , drop = FALSE]),
                    1, tendencia_futura[1, 1])
for (i in seq_along(nomes_cenarios)) {
  caminhos <- caminhos_forecast |>
    filter(cenario == nomes_cenarios[i]) |>
    arrange(date)
  condicoes <- matrix(NA_real_, h, N, dimnames = list(NULL, variaveis))
  condicoes[,] <- as.matrix(select(caminhos, all_of(variaveis)))
  message("INICIO FORECAST: ", nomes_cenarios[i], " | ", format(Sys.time(), "%H:%M:%S"))
  set.seed(123)
  previsao <- post_boj |> forecast(horizon = h, conditional_forecast = condicoes,
                                  exogenous_forecast = tendencia_futura)

  
  # Corrigir as defasagens do forecast 3.0 
  inovacoes <- previsao$forecasts - previsao$forecast_mean
  for (s in seq_len(S)) {
    estado <- estado_inicial
    sigma <- post_boj$posterior$Sigma[, , s]
    for (j in seq_len(h)) {
      media <- drop(post_boj$posterior$A[, , s] %*% estado)
      fixas <- which(!is.na(condicoes[j, ]))
      sorteio <- media + inovacoes[, j, s]
      if (length(fixas) > 0L) {
        ganho <- sigma[, fixas, drop = FALSE] %*% solve(sigma[fixas, fixas, drop = FALSE])
        sorteio <- sorteio - drop(ganho %*% (media[fixas] - previsao$forecast_mean[fixas, j, s]))
        sorteio[fixas] <- condicoes[j, fixas]
      }
      previsao$forecasts[, j, s] <- sorteio
      previsao$forecast_mean[, j, s] <- media
      estado <- c(sorteio, head(estado, N * (p - 1)), 1, (j + 1) / 12)
    }
  }
  F <- previsao$forecasts
  erro_condicoes <- if (any(!is.na(condicoes))) {
    max(abs(apply(F, 3, function(x) t(x)[!is.na(condicoes)]) - condicoes[!is.na(condicoes)]))
  } else 0
  stopifnot(all(is.finite(F)), erro_condicoes <= 1e-8)
  checagens[[i]] <- tibble(cenario = nomes_cenarios[i], origem, h, sorteios = S,
                           finitos = all(is.finite(F)), erro_condicoes)

  
  # Voltar a indices, cambio e taxas antes de resumir a distribuicao
  por_variavel <- vector("list", length(variaveis_domesticas))
  for (k in seq_along(variaveis_domesticas)) {
    sorteios <- matrix(F[k + 2L, , ], nrow = h)
    if (variaveis_domesticas[k] %in% c("atividade", "cpi_core", "usdjpy")) sorteios <- exp(sorteios / 100)
    stopifnot(all(is.finite(sorteios)))
    por_variavel[[k]] <- tibble(.model = "BVAR estrutural", cenario = nomes_cenarios[i],
      variavel = variaveis_domesticas[k], date = datas_futuras,
      valor = dist_sample(split(sorteios, row(sorteios))))
    dimnames(por_variavel[[k]]$valor) <- NULL
  }
  resultados[[i]] <- bind_rows(por_variavel)
  message("FIM FORECAST: ", nomes_cenarios[i], " | ", format(Sys.time(), "%H:%M:%S"))
}

# 4. Distribuicoes no formato fpp3 e tabela de medias e intervalos
previsoes_bvar <- bind_rows(resultados) |>
  mutate(.mean = mean(valor))
dimnames(previsoes_bvar$valor) <- "valor"
previsoes_bvar <- previsoes_bvar |>
  as_tsibble(key = c(.model, cenario, variavel), index = date) |>
  as_fable(response = "valor", distribution = "valor")
resumo_forecast <- previsoes_bvar |>
  as_tibble() |>
  transmute(cenario, variavel, date, media = .mean,
            mediana = median(valor), inferior_95 = quantile(valor, 0.025),
            inferior_80 = quantile(valor, 0.10), superior_80 = quantile(valor, 0.90),
            superior_95 = quantile(valor, 0.975))
diagnostico_forecast <- bind_rows(checagens)

saveRDS(list(previsoes = previsoes_bvar, resumo = resumo_forecast,
             caminhos = caminhos_forecast, diagnosticos = diagnostico_forecast,
             dados_modelo = dados_modelo, origem = origem,
             pesos_exponenciais = attr(post_boj, "pesos_exponenciais")), file.path(saida, "forecast_bvar.rds"))
write_csv(resumo_forecast |> mutate(date = as.Date(date)), file.path(saida, "resumo_forecast.csv"))
write_csv(caminhos_forecast |> mutate(date = as.Date(date)), file.path(saida, "caminhos_externos.csv"))
write_csv(diagnostico_forecast |> mutate(origem = as.Date(origem)), file.path(saida, "diagnosticos.csv"))
