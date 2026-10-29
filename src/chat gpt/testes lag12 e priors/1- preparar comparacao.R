Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(TZ = "America/Sao_Paulo")
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
source("src/chat gpt/testes lag12 e priors/0- configuracoes.R", encoding = "UTF-8")
dir.create(raiz_resultados, recursive = TRUE, showWarnings = FALSE)
dir.create(saida_lag12, recursive = TRUE, showWarnings = FALSE)
base_forecast <- readRDS(file.path(raiz_anterior, "base_forecast.rds")) |>
  select(date, all_of(c(externas, alvos_base)))
bases <- list(base_cv = dados_cv, base_forecast = base_forecast,
  benchmarks = readRDS(file.path(raiz_anterior, "benchmarks.rds")) |> filter(variavel %in% alvos_base))
for (nome in names(bases)) {
  arq <- file.path(raiz_resultados, paste0(nome, ".rds"))
  if (file.exists(arq)) stopifnot(identical(readRDS(arq), bases[[nome]])) else saveRDS(bases[[nome]], arq)
}
stopifnot(nrow(dados_cv) == 319L, nrow(base_forecast) == 320L, nrow(origens) == 47L,
  max(dados_cv$date) == yearmonth("2026 Jul"), max(base_forecast$date) == yearmonth("2026 Aug"))
write_csv(configuracoes, file.path(raiz_resultados, "grade.csv"))
saveRDS(configuracoes, file.path(raiz_resultados, "grade.rds"))
for (i in seq_len(nrow(configuracoes))) for (estrutural in c(FALSE, TRUE)) {
  cfg <- configuracoes[i, ]
  spec <- preparar_spec(base_forecast, cfg, estrutural)
  stopifnot(ncol(spec$Y) == 7L, spec$spec$p == cfg$p,
    all(spec$spec$prior$hyper[1:3, 1] == c(cfg$mu, cfg$delta, cfg$lambda)),
    ncol(spec$spec$data_matrices$X) == nrow(base_forecast) - cfg$p)
}
write_json(list(init = 180, step = 3, H = 24, S = 2500, janelas = 47,
  condicao_cv = "UST e Brent realizados; cinco domesticas livres",
  identificacao = "mesmos sinais e narrativas; Fed, petroleo, BoJ, premio; fluxo residual",
  dados = "mesmas bases congeladas da comparacao de 24/10; sem novas variaveis",
  selecao = "grade reduzida por CV; referencia e candidato p12 tambem validados no estrutural",
  taxa = "overnight efetiva media mensal; nao confundir com alvo vigente de 1,25%",
  fonte_pesquisa = "docs/Pesquisa subsidios fiscal comunicacao e priors.md"),
  file.path(raiz_resultados, "configuracao.json"), auto_unbox = TRUE, pretty = TRUE)
cat("VALIDADO: oito configuracoes; 47 origens; sete variaveis; dados e cenarios preservados.\n")
