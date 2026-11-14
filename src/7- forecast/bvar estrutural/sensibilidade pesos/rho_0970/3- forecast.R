# Mesma estrutura do forecast original; cenarios alternativos via choque de oferta.
.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))

library(fpp3)
library(tidyverse)
library(distributional)
library(bsvars)
library(bsvarSIGNs)

raiz <- "C:/Users/dudup/OneDrive/japan-econometrics-model"
saida <- "C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/sensibilidade pesos/rho_0970"
dir.create(saida, recursive = TRUE, showWarnings = FALSE)



# 1. Reutilizar o modelo estimado e transformar suas informações em um tibble
fonte_modelo <- file.path(raiz, "data/processed/sensibilidade_pesos_sem_tendencia/rho_0970")
post_boj <- readRDS(file.path(fonte_modelo, "modelo.rds"))
dados_boj <- readRDS(file.path(fonte_modelo, "dados.rds"))
variaveis <- c("ust2y", "brent", "ip_mundial", "atividade", "cpi_core", "juro_curto", "inclinacao", "usdjpy")
variaveis_domesticas <- variaveis[4:8]
dados_modelo <- dados_boj |>
  as_tibble() |>
  select(date, all_of(variaveis)) |>
  arrange(date) |>
  as_tsibble(index = date)
matriz_modelo <- dados_modelo |> as_tibble() |> select(-date) |> as.matrix()
p <- as.integer(post_boj$last_draw$p)   #p: num de defasagens
S <- dim(post_boj$posterior$A)[3]       # S: todos os sorteios disponiveis na posterior
N <- length(variaveis)                  # n: num de variaveis
identificacao <- post_boj$last_draw$identification$sign_irf
stopifnot(p == 6L, N == 8L, S == 2500L,
          !any(has_gaps(dados_modelo)$.gaps), all(is.finite(matriz_modelo)),
          identical(dim(post_boj$posterior$A), c(N, N * p + 1L, S)),
          isTRUE(all.equal(unname(post_boj$last_draw$data_matrices$Y),
                           unname(t(matriz_modelo[-seq_len(p), ])))),
          all(is.na(identificacao[, c(3, 4, 5, 8), ])),
          all(vapply(c(1, 2, 6, 7), function(k) any(!is.na(identificacao[, k, ])), logical(1))))

# Mesmos limites numericos da estimacao
coeficientes <- matrix(post_boj$posterior$A, ncol = S)
lags_domesticos <- unlist(lapply(0:(p - 1), function(j) 4:8 + N * j))
stopifnot(all(is.finite(coeficientes)), all(is.finite(post_boj$posterior$Sigma)),
          all(is.finite(post_boj$posterior$Theta0)),
          is.finite(post_boj$posterior$ess), post_boj$posterior$ess >= 50,
          ncol(unique(coeficientes, MARGIN = 2)) >= 50,
          max(abs(post_boj$posterior$A[1:3, lags_domesticos, ])) <= 1e-8,
          max(abs(post_boj$posterior$Theta0[1:3, 4:8, ])) <= 1e-8)



# 2. Preparar os caminhos/cenários das váriaveis externas

fonte_caminhos <- file.path(raiz, "src/Outputs/4- Forecast/sensibilidade pesos/rho_0970")
caminhos_forecast <- readRDS(file.path(fonte_caminhos, "caminhos_novos.rds"))
respostas_oferta <- readRDS(file.path(fonte_caminhos, "resposta oferta.rds"))
nomes_cenarios <- c("Base", "Benigno", "Adverso", "Adverso estrutural")
origem <- max(dados_modelo$date)
h <- 24L
datas_futuras <- origem + seq_len(h)

# 3. Forecast
# Sem tendencia linear; somente constante e defasagens.
resultados <- vector("list", length(nomes_cenarios))
checagens <- vector("list", length(nomes_cenarios))
estado_inicial <- c(t(matriz_modelo[nrow(matriz_modelo) - (0:(p - 1)), , drop = FALSE]),
                    1)
# O pacote preve o Base; cenarios reutilizam essa mesma previsao
caminhos_base <- caminhos_forecast |>
  filter(cenario == "Base") |>
  arrange(date)
condicoes <- as.matrix(select(caminhos_base, all_of(variaveis)))
  message("INICIO FORECAST: ", "Base", " | ", format(Sys.time(), "%H:%M:%S"))
  set.seed(123)
  previsao <- post_boj |> forecast(horizon = h, conditional_forecast = condicoes)

  
# Corrigir as defasagens do forecast 3.0 (Afim de levar em conta as defasagens no forecast)
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
      estado <- c(sorteio, head(estado, N * (p - 1)), 1)
    }
  }
  F_base <- previsao$forecasts
  erro_base <- max(abs(apply(F_base, 3, function(x) t(x)[!is.na(condicoes)]) - condicoes[!is.na(condicoes)]))
  stopifnot(all(is.finite(F_base)), erro_base <= 1e-8)
  saveRDS(previsao, file.path(saida, "previsao_base_pacote.rds"))

#  Construção dos cenários divergentes do base, somando os efeitos das IRFs decorrentes do choque de oferta do petróleo
#  Trajetória do cenário = trajetória base + efeito da oferta
  
trajetorias <- list()
for (i in seq_along(nomes_cenarios)) {
  F <- F_base
  resposta <- respostas_oferta[[nomes_cenarios[i]]]
  for (s in seq_len(S)) F[, , s] <- F_base[, , s] + t(resposta)
  caminhos <- caminhos_forecast |> filter(cenario == nomes_cenarios[i]) |> arrange(date) 
  fixas_brent <- which(!is.na(caminhos$brent))   #Conferir se o Brent chegou ao resultado desejado
  erro_condicoes <- max(abs(F[2, fixas_brent, ] - caminhos$brent[fixas_brent]))
  stopifnot(all(is.finite(F)), erro_condicoes <= 1e-8)
  trajetorias[[nomes_cenarios[i]]] <- F
  checagens[[i]] <- tibble(cenario = nomes_cenarios[i], origem, h, sorteios = S,
    finitos = all(is.finite(F)), erro_condicoes)

  # Voltar a indices, cambio e taxas antes de resumir a distribuicao
  por_variavel <- vector("list", length(variaveis_domesticas))
  for (k in seq_along(variaveis_domesticas)) {
    sorteios <- matrix(F[k + 3L, , ], nrow = h)
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


# 5. Preparação dos resultados para os gráficos
# variação em 12 meses: CPI e atividade
resumos_yoy <- list()
for (alvo in c("atividade", "cpi_core")) {
  historico_nivel <- exp(tail(dados_modelo[[alvo]], 12) / 100)
  for (cenario_atual in nomes_cenarios) {
    previsto <- previsoes_bvar |> as_tibble() |>
      filter(variavel == alvo, cenario == cenario_atual) |> arrange(date)
    sorteios <- do.call(rbind, parameters(previsto$valor)$x)
    completo <- rbind(matrix(historico_nivel, 12, S), sorteios)
    variacao <- 100 * (completo[12 + seq_len(h), , drop = FALSE] / completo[seq_len(h), , drop = FALSE] - 1)
    resumos_yoy[[length(resumos_yoy) + 1L]] <- tibble(cenario = cenario_atual,
      variavel = alvo, date = datas_futuras, media = rowMeans(variacao),
      mediana = apply(variacao, 1, median), inferior_95 = apply(variacao, 1, quantile, .025),
      inferior_80 = apply(variacao, 1, quantile, .10), superior_80 = apply(variacao, 1, quantile, .90),
      superior_95 = apply(variacao, 1, quantile, .975))
  }
}
resumo_grafico <- bind_rows(filter(resumo_forecast, !variavel %in% c("atividade", "cpi_core")),
  bind_rows(resumos_yoy)) |>
  rename(inferior95 = inferior_95, inferior80 = inferior_80,
    superior80 = superior_80, superior95 = superior_95) |>
  mutate(modelo = "producao")
# As bandas dos alternativos nao incorporam a incerteza da IRF central
resumo_grafico <- resumo_grafico |>
  mutate(across(c(inferior95, inferior80, superior80, superior95),
    ~ if_else(cenario == "Base", .x, NA_real_)))
saveRDS(resumo_grafico, file.path(saida, "resumo.rds"))
write_csv(resumo_grafico |> mutate(date = as.Date(date)), file.path(saida, "resumo.csv"))
atividade_niveis <- previsoes_bvar |> as_tibble() |> filter(variavel == "atividade") |>
  transmute(cenario, date, atividade = .mean)
saveRDS(atividade_niveis, file.path(saida, "atividade niveis.rds"))
saveRDS(trajetorias, file.path(saida, "trajetorias modelos.rds"))
cat("FORECAST_NA_ESTRUTURA_ORIGINAL_CONCLUIDO S", S, "\n")

