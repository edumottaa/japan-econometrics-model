Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(TZ = "America/Sao_Paulo")
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
source("src/chat gpt/premissas/0- funcoes forecasts.R", encoding = "UTF-8")

# Hipotese: informacao sobre a inflacao em agosto/2028, com erro de 0,5 p.p.
centro_inflacao <- 2
desvio_inflacao <- .5
for (p in c(6L, 12L)) {
  ref <- referencia(p)
  caminhos <- ref$resultado$caminhos |>
    mutate(juro_curto = if_else(date == yearmonth("2026 Dec"), 1.50, juro_curto))
  preparar_caminhos <- function(dados) caminhos
  pasta <- file.path(raiz_premissas, paste0("lag", p), "juro dezembro 1.50")
  dezembro <- forecast_ajuste(ref$ajuste, paste0(p, " lags | BoJ dezembro 1,50"), pasta)
  stopifnot(all(filter(dezembro$caminhos, date == yearmonth("2026 Dec"))$juro_curto == 1.50),
    all(is.na(filter(dezembro$caminhos, date > yearmonth("2026 Dec"))$juro_curto)))
  plotar_formato(preparar_grafico(dezembro), pasta)

  # Pesos sobre trajetorias futuras; os mesmos indices para todas as variaveis.
  x <- as_tibble(ref$resultado$previsoes)
  indices <- diagnosticos <- pesos_salvos <- list()
  for (c in unique(x$cenario)) {
    cpi <- x |> filter(cenario == c, variavel == "cpi_core") |> arrange(date)
    amostras <- parameters(cpi$valor)$x
    terminal <- nrow(cpi)
    inflacao <- 100 * (amostras[[terminal]] / amostras[[terminal - 12L]] - 1)
    log_pesos <- dnorm(inflacao, centro_inflacao, desvio_inflacao, log = TRUE)
    pesos <- exp(log_pesos - max(log_pesos))
    pesos <- pesos / sum(pesos)
    ess <- 1 / sum(pesos^2)
    stopifnot(is.finite(ess), ess >= 50)
    indices[[c]] <- indices_pesos(pesos)
    diagnosticos[[c]] <- tibble(cenario = c, ess_pesos = ess, centro = centro_inflacao,
      desvio = desvio_inflacao, data_condicao = as.Date(max(cpi$date)),
      media_antes = mean(inflacao), media_ponderada = sum(pesos * inflacao))
    pesos_salvos[[c]] <- tibble(cenario = c, sorteio = seq_along(pesos), peso = pesos)
  }
  suave <- reponderar_trajetorias(ref$resultado, indices, paste0(p, " lags | inflacao suave em 2028"))
  suave$diagnosticos <- bind_rows(diagnosticos)
  suave$hipotese <- list(tipo = "informacao ruidosa sobre resultado futuro",
    centro = centro_inflacao, desvio = desvio_inflacao, data = "2028 Aug")
  pasta <- file.path(raiz_premissas, paste0("lag", p), "inflacao suave")
  salvar_previsoes(suave, pasta)
  write_csv(bind_rows(pesos_salvos), file.path(pasta, "pesos_trajetorias.csv"))
  print(suave$diagnosticos)
}
