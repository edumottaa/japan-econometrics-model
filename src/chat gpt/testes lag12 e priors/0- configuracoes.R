source("src/5- models/Comparacao hiato e priors/0- funcoes bvar.R", encoding = "UTF-8")
raiz_anterior <- "data/processed/comparacao_hiato_priors_20261024"
raiz_resultados <- "data/processed/comparacao_lag12_priors_20261025"
saida_lag12 <- "src/Outputs/Comparacao lag12 e priors"
configuracoes <- tribble(
  ~formato,             ~p, ~lambda, ~mu, ~delta,
  "p6_referencia",      6L,     .20,   1,      1,
  "p12_referencia",    12L,     .20,   1,      1,
  "p12_lambda10",      12L,     .10,   1,      1,
  "p12_mu05",          12L,     .10,  .5,      1,
  "p12_delta5",        12L,     .10,   1,      5,
  "p12_mu05_delta5",   12L,     .10,  .5,      5,
  "p12_lambda05_md",   12L,     .05,  .5,      5,
  "p12_lambda20_md",   12L,     .20,  .5,      5) |>
  mutate(incluir_hiato = FALSE,
    nome = paste0("p", p, " | lambda=", lambda, " mu=", mu, " delta=", delta))
dados_cv <- readRDS(file.path(raiz_anterior, "base_cv.rds")) |>
  select(date, all_of(c(externas, alvos_base)))
cv_data <- dados_cv |> stretch_tsibble(.init = 180L, .step = 3L)
origens <- cv_data |> as_tibble() |> summarise(origem = max(date), .by = .id)
observados <- dados_cv |> as_tibble() |> select(date, all_of(alvos_base)) |>
  pivot_longer(-date, names_to = "variavel", values_to = "observado")
