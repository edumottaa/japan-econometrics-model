# Mesma estrutura do forecast original; cenarios alternativos via choque de oferta.
.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))

library(fpp3)
library(tidyverse)
library(distributional)
library(bsvars)
library(bsvarSIGNs)

raiz <- "C:/Users/dudup/OneDrive/japan-econometrics-model"
saida <- "C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao"
dir.create(saida, recursive = TRUE, showWarnings = FALSE)



# 1. Reutilizar o modelo estimado e transformar suas informações em um tibble
fonte_modelo <- file.path(raiz, "data/processed/bvar_producao_tendencia_jgb30_5_20261112")
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
stopifnot(p == 6L, N == 8L, S >= 50L,
          !any(has_gaps(dados_modelo)$.gaps), all(is.finite(matriz_modelo)),
          identical(dim(post_boj$posterior$A), c(N, N * p + 2L, S)),
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

fonte_caminhos <- file.path(raiz, "src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao")
caminhos_forecast <- readRDS(file.path(fonte_caminhos, "caminhos_novos.rds"))
respostas_oferta <- readRDS(file.path(fonte_caminhos, "resposta oferta.rds"))
nomes_cenarios <- c("Base", "Benigno", "Adverso", "Adverso estrutural")
origem <- max(dados_modelo$date)
fim_forecast <- yearmonth('2028 Sep')
h <- as.integer(fim_forecast-origem)
datas_futuras <- origem + seq_len(h)
stopifnot(tail(datas_futuras,1)==fim_forecast)
banda_juro <- c(inferior=1.75,superior=2.25)

# 3. Forecast
# implementação da variavel de t~endencia: continuar a tendência em anos a partir do último mês observado
tendencia_futura <- matrix(seq_len(h) / 12, ncol = 1)
resultados <- vector("list", length(nomes_cenarios))
checagens <- vector("list", length(nomes_cenarios))
estado_inicial <- c(t(matriz_modelo[nrow(matriz_modelo) - (0:(p - 1)), , drop = FALSE]),
                    1, tendencia_futura[1, 1])
# O pacote preve o Base; cenarios reutilizam essa mesma previsao
caminhos_base <- caminhos_forecast |>
  filter(cenario == "Base") |>
  arrange(date)
condicoes <- as.matrix(select(caminhos_base, all_of(variaveis)))
stopifnot(nrow(condicoes)==h,identical(caminhos_base$date,datas_futuras))
  message("INICIO FORECAST: ", "Base", " | ", format(Sys.time(), "%H:%M:%S"))
  set.seed(123)
  previsao <- post_boj |> forecast(horizon = h, conditional_forecast = condicoes,
                                  exogenous_forecast = tendencia_futura)

  
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
      estado <- c(sorteio, head(estado, N * (p - 1)), 1, (j + 1) / 12)
    }
  }
  F_base <- previsao$forecasts
  erro_base <- max(abs(apply(F_base, 3, function(x) t(x)[!is.na(condicoes)]) - condicoes[!is.na(condicoes)]))
  stopifnot(all(is.finite(F_base)), erro_base <= 1e-8)
  saveRDS(previsao, file.path(saida, "previsao_base_pacote.rds"))

# 3.1. Banda terminal: Condicionamento do juros terminal a 2% +/- 0,25%
ganhos_banda <- array(0,c(N,h,S))
media_terminal <- variancia_terminal <- numeric(S)
for (s in seq_len(S)) {
  A <- post_boj$posterior$A[,,s]
  sigma <- post_boj$posterior$Sigma[,,s]
  transicoes <- covariancias <- vector('list',h)
  centro <- estado_inicial
  for (j in seq_len(h)) {
    fixas <- which(!is.na(condicoes[j,]))
    ajuste <- diag(N)
    Q <- sigma
    media <- drop(A %*% centro)
    if (length(fixas)) {
      ganho <- sigma[,fixas,drop=FALSE] %*% solve(sigma[fixas,fixas,drop=FALSE])
      media <- media+drop(ganho %*% (condicoes[j,fixas]-media[fixas]))
      ajuste[,fixas] <- ajuste[,fixas,drop=FALSE]-ganho
      Q <- sigma-ganho %*% sigma[fixas,,drop=FALSE]
      Q[fixas,] <- 0; Q[,fixas] <- 0
      media[fixas] <- condicoes[j,fixas]
    }
    transicoes[[j]] <- ajuste %*% A[,seq_len(N*p),drop=FALSE]
    covariancias[[j]] <- (Q+t(Q))/2
    centro <- c(media,head(centro,N*(p-1)),1,(j+1)/12)
  }
  media_terminal[s] <- centro[6]
  # Sensibilidade do juro terminal as inovacoes de cada mes.
  peso <- rep(0,N*p); peso[6] <- 1
  projecoes <- vector('list',h)
  for (j in h:1) {
    projecoes[[j]] <- drop(covariancias[[j]] %*% peso[seq_len(N)])
    peso <- drop(crossprod(transicoes[[j]],peso[seq_len(N)]))+
      c(peso[-seq_len(N)],rep(0,N))
  }
  cov_estado <- rep(0,N*p)
  for (j in seq_len(h)) {
    cov_j <- drop(transicoes[[j]] %*% cov_estado)+projecoes[[j]]
    ganhos_banda[,j,s] <- cov_j
    cov_estado <- c(cov_j,head(cov_estado,N*(p-1)))
  }
  variancia_terminal[s] <- cov_estado[6]
  stopifnot(is.finite(variancia_terminal[s]),variancia_terminal[s]>0)
  ganhos_banda[,,s] <- ganhos_banda[,,s]/variancia_terminal[s]
}
# Usar os mesmos quantis nos cenarios; calculo em log evita perda numerica nas caudas.
set.seed(456)
quantis_banda <- runif(S)
diagnosticos_banda <- list()

#  Construção dos cenários divergentes do base, somando os efeitos das IRFs decorrentes do choque de oferta do petróleo
#  Trajetória do cenário = trajetória base + efeito da oferta
  
trajetorias <- list()
for (i in seq_along(nomes_cenarios)) {
  F <- F_base
  resposta <- respostas_oferta[[nomes_cenarios[i]]]
  for (s in seq_len(S)) F[, , s] <- F_base[, , s] + t(resposta)
  for (s in seq_len(S)) {
    mu_final <- media_terminal[s]+resposta[h,'juro_curto']
    desvio <- sqrt(variancia_terminal[s])
    a <- (banda_juro['inferior']-mu_final)/desvio
    b <- (banda_juro['superior']-mu_final)/desvio
    direita <- mu_final<=mean(banda_juro)
    log_maior <- pnorm(if(direita) a else b,lower.tail=!direita,log.p=TRUE)
    log_menor <- pnorm(if(direita) b else a,lower.tail=!direita,log.p=TRUE)
    log_prob <- log_maior+log1p(-quantis_banda[s]*(-expm1(log_menor-log_maior)))
    terminal <- mu_final+desvio*qnorm(log_prob,lower.tail=!direita,log.p=TRUE)
    F[,,s] <- F[,,s]+ganhos_banda[,,s]*(terminal-F[6,h,s])
  }
  stopifnot(all(F[6,h,]>=banda_juro['inferior']-1e-10),
            all(F[6,h,]<=banda_juro['superior']+1e-10))
  diagnosticos_banda[[i]] <- tibble(cenario=nomes_cenarios[i],date=fim_forecast,
    limite_inferior=banda_juro['inferior'],limite_superior=banda_juro['superior'],
    minimo=min(F[6,h,]),media=mean(F[6,h,]),maximo=max(F[6,h,]),sorteios=S)
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
             pesos_exponenciais = attr(post_boj, "pesos_exponenciais"),
             banda_juro=banda_juro,data_banda=fim_forecast), file.path(saida, "forecast_bvar.rds"))
write_csv(bind_rows(diagnosticos_banda) |> mutate(date=as.Date(date)),
          file.path(saida,'diagnosticos banda juro.csv'))
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
