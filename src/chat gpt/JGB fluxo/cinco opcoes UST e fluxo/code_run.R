# ORDEM: forecasts 1-4, CVs 1-4, forecast 5, CV 5, forecasts 6-8 e CVs 6-8.
# Este e o arquivo para voce rodar. Nao foi executado na preparacao.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco opcoes UST e fluxo/00_configuracao.R',encoding='UTF-8')
iniciar_testes()
source(file.path(pasta_testes,'cv_igual_bvar_original.R'),encoding='UTF-8')

RODAR_FORECAST <- TRUE
RODAR_CV <- TRUE
MOSTRAR_GRAFICOS <- TRUE
# Para retomar so o CV: RODAR_FORECAST <- FALSE.
# Cache so e reutilizado se configuracao, codigo e insumos forem identicos.
# 1. Concluir forecasts e CVs das quatro primeiras opcoes.
status_1_a_4 <- rodar_lote(testes=1:4,rodar_forecast=RODAR_FORECAST,rodar_cv=RODAR_CV)

# 2. Somente depois: forecast e CV da opcao 5.
# Preservar no status geral tambem as etapas concluidas das opcoes 1-4.
tryCatch(
  rodar_lote(testes=5L,rodar_forecast=RODAR_FORECAST,rodar_cv=RODAR_CV),
  finally={
    arquivo_status <- file.path(pasta_testes,'resultados','status.csv')
    if(file.exists(arquivo_status)) {
      status_5 <- readr::read_csv(arquivo_status,show_col_types=FALSE) |>
        dplyr::filter(teste==5L)
      readr::write_csv(dplyr::bind_rows(status_1_a_4,status_5),arquivo_status)
    }
  }
)

# 3. Depois de concluir a opcao 5: forecasts 6-8 primeiro, CVs 6-8 depois.
status_1_a_5 <- readr::read_csv(
  file.path(pasta_testes,'resultados','status.csv'),show_col_types=FALSE)
tryCatch(
  rodar_lote(testes=6:8,rodar_forecast=RODAR_FORECAST,rodar_cv=RODAR_CV),
  finally={
    arquivo_status <- file.path(pasta_testes,'resultados','status.csv')
    if(file.exists(arquivo_status)) {
      status_6_a_8 <- readr::read_csv(arquivo_status,show_col_types=FALSE) |>
        dplyr::filter(teste %in% 6:8)
      readr::write_csv(dplyr::bind_rows(status_1_a_5,status_6_a_8),arquivo_status)
    }
  }
)
