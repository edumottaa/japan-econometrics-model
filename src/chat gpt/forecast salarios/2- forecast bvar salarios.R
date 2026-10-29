# BVAR original: acrescentar salarios; seis lags e mesmos priors e cenarios.
library(fpp3)
library(tidyverse)
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
source("src/5- models/Comparacao hiato e priors/0- funcoes bvar.R", encoding = "UTF-8")
source("src/chat gpt/forecast salarios/1- preparar salarios.R", encoding = "UTF-8")

# 1. Preservar a base e a identificacao do modelo de referencia.
referencia <- readRDS("data/processed/forecast_overnight_fim_mes/modelo.rds")
cfg <- referencia$configuracao |> mutate(formato = "salarios")
alvos_base <- c(alvos_base, "salario")
variaveis <- c(externas, alvos_base)
dados_boj <- referencia$dados_modelo |>
  left_join(select(salarios_modelo, date, salario), by = "date")
stopifnot(cfg$p == 6L, cfg$lambda == .20, cfg$mu == 1, cfg$delta == 1,
          identical(select(dados_boj, -salario), referencia$dados_modelo),
          !any(has_gaps(dados_boj)$.gaps),
          all(is.finite(as.matrix(select(as_tibble(dados_boj), -date)))))
preparado <- preparar_spec(dados_boj, cfg, estrutural = TRUE)
stopifnot(length(preparado$variaveis) == 8L,
          identical(unname(preparado$spec$identification$sign_irf[1:7, 1:7, ]),
                    unname(referencia$post$last_draw$identification$sign_irf)),
          all(is.na(preparado$spec$identification$sign_irf[8, , ])),
          all(is.na(preparado$spec$identification$sign_irf[, 8, ])))

# 2. Estimar uma vez; reutilizar apenas se a base e a configuracao coincidirem.
pasta_modelo <- "data/processed/forecast_bvar_salarios"
saida <- "src/Outputs/Forecast BVAR estrutural/salarios"
dir.create(pasta_modelo, recursive = TRUE, showWarnings = FALSE)
dir.create(saida, recursive = TRUE, showWarnings = FALSE)
arquivo_modelo <- file.path(pasta_modelo, "modelo.rds")
reutilizar <- FALSE
if (file.exists(arquivo_modelo)) {
  ajuste <- readRDS(arquivo_modelo)
  reutilizar <- identical(ajuste$dados_modelo, dados_boj) && identical(ajuste$configuracao, cfg)
}
if (!reutilizar) {
  gravar_status(pasta_modelo, "estimando", variaveis = 8L, lags = cfg$p)
  ajuste <- estimar_bvar(dados_boj, cfg, estrutural = TRUE)
  ajuste$dados_modelo <- dados_boj
  ajuste$configuracao <- cfg
  saveRDS(ajuste, arquivo_modelo)
}

# 3. Mesmos caminhos externos; salarios futuros ficam livres no BVAR.
anterior <- readRDS("src/Outputs/Forecast BVAR estrutural/overnight fim de mes/forecast_bvar.rds")
stopifnot(identical(anterior$dados_modelo, referencia$dados_modelo))
caminhos <- anterior$caminhos |> mutate(salario = NA_real_) |>
  select(date, cenario, all_of(variaveis))
gravar_status(pasta_modelo, "forecast")
resultados <- checagens <- list()
for (cenario_atual in unique(caminhos$cenario)) {
  message("FORECAST | salarios | ", cenario_atual)
  caminho <- caminhos |> filter(cenario == cenario_atual) |> arrange(date)
  condicoes <- caminho |> select(all_of(variaveis)) |> as.matrix()
  prev <- prever_bvar(ajuste$post, dados_boj, condicoes, cfg$p)
  resultados[[cenario_atual]] <- bind_rows(lapply(seq_along(variaveis), function(k) {
    sorteios <- matrix(prev$sorteios[k, , ], nrow = nrow(caminho))
    if (variaveis[k] %in% c("brent", "atividade", "cpi_core", "usdjpy"))
      sorteios <- exp(sorteios / 100)
    stopifnot(all(is.finite(sorteios)))
    x <- tibble(.model = "BVAR estrutural | salarios | 6 lags", cenario = cenario_atual,
      variavel = variaveis[k], date = caminho$date,
      valor = dist_sample(split(sorteios, row(sorteios))))
    dimnames(x$valor) <- NULL
    x
  }))
  checagens[[cenario_atual]] <- tibble(cenario = cenario_atual,
    erro_condicoes = prev$erro_condicoes, sorteios = dim(prev$sorteios)[3],
    finitos = all(is.finite(prev$sorteios)))
}
previsoes <- bind_rows(resultados) |> mutate(.mean = mean(valor))
dimnames(previsoes$valor) <- "valor"
previsoes <- previsoes |> as_tsibble(key = c(.model, cenario, variavel), index = date) |>
  as_fable(response = "valor", distribution = "valor")
resumo <- previsoes |> as_tibble() |> transmute(.model, cenario, variavel, date, media = .mean,
  mediana = median(valor), inferior_95 = quantile(valor, .025), inferior_80 = quantile(valor, .10),
  superior_80 = quantile(valor, .90), superior_95 = quantile(valor, .975))
resultado <- list(previsoes = previsoes, resumo = resumo, caminhos = caminhos,
  diagnosticos = bind_rows(checagens), dados_modelo = dados_boj, origem = anterior$origem)
stopifnot(nrow(previsoes) == 4L * 8L * 24L,
          all(resultado$diagnosticos$erro_condicoes <= 1e-8))
saveRDS(resultado, file.path(saida, "forecast_bvar.rds"))
write_csv(resumo |> mutate(date = as.Date(date)), file.path(saida, "resumo_forecast.csv"))
write_csv(ajuste$diagnostico, file.path(saida, "diagnostico_estimacao.csv"))
write_csv(resultado$diagnosticos, file.path(saida, "validacao_forecast.csv"))
gravar_status(pasta_modelo, "forecast_concluido", origem = as.character(resultado$origem))
source("src/chat gpt/forecast salarios/3- graficos forecast salarios.R", encoding = "UTF-8")
gravar_status(pasta_modelo, "concluido", variaveis = 8L, lags = cfg$p,
  origem = as.character(resultado$origem), lambda = cfg$lambda, mu = cfg$mu, delta = cfg$delta)
