# Mesmas origens e benchmarks do CV anterior; hiato futuro nao condicionado.
source("src/5- models/Comparacao hiato e priors/0- funcoes bvar.R", encoding = "UTF-8")
dados_cv <- readRDS(file.path(raiz_resultados, "base_cv.rds"))
cv_data <- dados_cv |> stretch_tsibble(.init = 180L, .step = 3L)
origens <- cv_data |> as_tibble() |> summarise(origem = max(date), .by = .id)
observados <- dados_cv |> as_tibble() |> select(date, all_of(c(alvos_base, "hiato"))) |>
  pivot_longer(-date, names_to = "variavel", values_to = "observado")

preparar_benchmarks <- function() {
  arquivo <- file.path(raiz_resultados, "benchmarks.rds")
  if (file.exists(arquivo)) return(readRDS(arquivo))
  anterior <- readRDS(paste0("data/processed/",
    "cv_6_bvars_estruturais_sem_demanda_oferta_fluxo_topix_20261022/teste_1/resultados_finais.rds"))
  refs <- anterior$previsoes |> filter(!grepl("BVAR", .model))
  gap <- cv_data |> select(.id, date, valor = hiato) |>
    model(AR = ARIMA(valor ~ pdq(d = 0, q = 0) + PDQ(0, 0, 0)),
          naive = NAIVE(valor), drift = RW(valor ~ drift())) |>
    forecast(h = 24L) |> as_tibble() |>
    group_by(.id, .model) |> arrange(date, .by_group = TRUE) |>
    mutate(h = row_number(), .mean = mean(valor), variavel = "hiato") |> ungroup() |>
    filter(date <= max(dados_cv$date)) |> select(all_of(names(refs)))
  dimnames(refs$valor) <- dimnames(gap$valor) <- NULL
  refs <- bind_rows(refs, gap)
  saveRDS(refs, arquivo)
  refs
}

crps_cv <- function(d, y) {
  amostral <- vapply(vctrs::vec_data(d), inherits, logical(1), "dist_sample")
  stopifnot(all(amostral) || !any(amostral))
  fabletools::CRPS(.dist = if (all(amostral)) vctrs::vec_data(d) else d, .actual = y)
}

metricas_cv <- function(prev, pasta) {
  pontos <- prev |> left_join(origens, by = ".id") |>
    left_join(observados, by = c("date", "variavel")) |>
    mutate(erro = observado - .mean, periodo = case_when(
      year(date) <= 2019 ~ "2015-2019", year(date) <= 2021 ~ "2020-2021", TRUE ~ "2022-2026"))
  stopifnot(all(is.finite(pontos$erro)))
  resumir <- function(x, grupos) x |>
    summarise(RMSE = sqrt(mean(erro^2)), MAE = mean(abs(erro)), vies = -mean(erro),
      CRPS = crps_cv(valor, observado), casos = n(), .by = all_of(grupos))
  acumuladas <- bind_rows(pontos |> filter(h <= 12) |> mutate(horizonte = "H1-H12"),
                         pontos |> mutate(horizonte = "H1-H24")) |>
    resumir(c("variavel", "horizonte", ".model"))
  exatas <- pontos |> filter(h %in% c(12, 24)) |> resumir(c("variavel", "h", ".model"))
  periodos <- pontos |> resumir(c("variavel", "periodo", ".model"))
  stopifnot(all(acumuladas$casos[acumuladas$horizonte == "H1-H12"] == 538L),
    all(acumuladas$casos[acumuladas$horizonte == "H1-H24"] == 1028L),
    all(exatas$casos[exatas$h == 12] == 43L), all(exatas$casos[exatas$h == 24] == 39L),
    all(is.finite(acumuladas$CRPS)), all(is.finite(periodos$CRPS)))
  write_csv(acumuladas, file.path(pasta, "metricas_h12_h24.csv"))
  write_csv(exatas, file.path(pasta, "metricas_horizontes_exatos.csv"))
  write_csv(periodos, file.path(pasta, "metricas_por_periodo.csv"))
  write_csv(select(pontos, -valor), file.path(pasta, "pontuacoes.csv"))
  list(previsoes = prev, metricas = acumuladas, horizontes_exatos = exatas, periodos = periodos)
}

rodar_cv <- function(formato, piloto = FALSE, janelas = seq_len(nrow(origens)),
                     estrutural = FALSE, nome_modelo = NULL) {
  cfg <- configuracoes |> filter(.data$formato == .env$formato)
  alvos <- c(alvos_base, if (cfg$incluir_hiato) "hiato")
  variaveis <- c(externas, alvos)
  modelo <- if (cfg$incluir_hiato) "BVAR reduzido + hiato" else "BVAR reduzido lambda 0,10"
  if (!is.null(nome_modelo)) modelo <- nome_modelo
  pasta <- file.path(raiz_resultados, "cv", if (estrutural) paste0("estrutural_", formato) else formato)
  dir.create(file.path(pasta, "checkpoints"), recursive = TRUE, showWarnings = FALSE)
  refs <- preparar_benchmarks() |> filter(variavel %in% alvos)
  resultados <- diagnosticos <- vector("list", nrow(origens))
  for (i in janelas) {
    origem <- origens$origem[i]
    treino <- dados_cv |> filter(date <= origem) |> select(date, all_of(variaveis))
    teste <- dados_cv |> filter(date > origem, date <= origem + 24L)
    H <- nrow(teste)
    if (!H) next
    arquivo <- file.path(pasta, "checkpoints", sprintf("janela_%03d.rds", i))
    if (file.exists(arquivo)) {
      salvo <- readRDS(arquivo)
      stopifnot(identical(salvo$origem, origem), identical(salvo$configuracao, cfg))
    } else {
      gravar_status(pasta, "executando", janela = i, total = nrow(origens),
                    origem = as.character(origem), fase = "estimacao")
      message("INICIO ", formato, " | ", i, "/", nrow(origens), " | ", origem)
      inicio <- Sys.time()
      ajuste <- estimar_bvar(treino, cfg, estrutural = estrutural, semente = 123L + i)
      condicoes <- matrix(NA_real_, H, length(variaveis), dimnames = list(NULL, variaveis))
      condicoes[, externas] <- as.matrix(select(as_tibble(teste), all_of(externas)))
      gravar_status(pasta, "executando", janela = i, total = nrow(origens), fase = "previsao")
      F <- prever_bvar(ajuste$post, treino, condicoes, cfg$p)$sorteios
      por_alvo <- lapply(seq_along(alvos), function(k) {
        sorteios <- matrix(F[k + 2L, , ], nrow = H)
        x <- tibble(variavel = alvos[k], .id = origens$.id[i], .model = modelo,
          date = teste$date, h = seq_len(H), valor = dist_sample(split(sorteios, row(sorteios))))
        dimnames(x$valor) <- NULL
        x |> mutate(.mean = mean(valor))
      })
      salvo <- list(origem = origem, configuracao = cfg, previsoes = bind_rows(por_alvo),
        diagnostico = ajuste$diagnostico |> mutate(.id = origens$.id[i], origem = origem,
          segundos = as.numeric(difftime(Sys.time(), inicio, units = "secs"))))
      saveRDS(salvo, paste0(arquivo, ".tmp"), compress = FALSE)
      stopifnot(file.rename(paste0(arquivo, ".tmp"), arquivo))
      message("FIM ", formato, " | ", i, " | segundos ", round(salvo$diagnostico$segundos, 1))
      rm(ajuste, F, por_alvo)
      gc(verbose = FALSE)
    }
    resultados[[i]] <- salvo$previsoes
    diagnosticos[[i]] <- salvo$diagnostico
    gravar_status(pasta, if (piloto) "piloto_concluido" else "executando", janela = i, total = nrow(origens))
    if (piloto) return(invisible(salvo$diagnostico))
  }
  if (length(janelas) < nrow(origens)) return(invisible(NULL))
  bvar <- bind_rows(resultados)
  dimnames(bvar$valor) <- dimnames(refs$valor) <- NULL
  final <- metricas_cv(bind_rows(refs, bvar), pasta)
  final$diagnosticos <- bind_rows(diagnosticos)
  stopifnot(nrow(final$diagnosticos) == 47L)
  write_csv(final$diagnosticos, file.path(pasta, "diagnosticos.csv"))
  saveRDS(final, file.path(pasta, "resultados_finais.rds"))
  gravar_status(pasta, "concluido", janelas = 47L, modelos = unique(final$previsoes$.model))
  invisible(final)
}
