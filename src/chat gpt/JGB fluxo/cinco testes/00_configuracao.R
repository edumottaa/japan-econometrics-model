# Configuracao comum. Este arquivo so define opcoes; nao estima nem projeta.
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
pasta_testes <- file.path(raiz,'src/chat gpt/JGB fluxo/cinco testes')
fonte_modelo <- file.path(raiz,'data/processed/bvar_producao_tendencia_jgb30_5_20261112')
fonte_forecast <- file.path(raiz,'src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao')
fonte_dados <- file.path(raiz,'src/chat gpt/spreads JGB modelo satelite/dados')

ORIGEM <- NULL                 # NULL = ultimo mes do BVAR. Historico: '2024 Aug'.
INICIO <- '2018 Jul'           # Amostra COMUM; fluxo lag 2 existe desde jul/2018.
HORIZONTE <- NULL              # NULL = horizonte salvo; historico usa 24 meses.
P <- 6L                       # Mesmo numero de defasagens do principal.
RHO <- .995                   # Mesmos pesos exponenciais.
S <- 2500L
HIPER_S <- 5000L
HIPER_BURN <- 2500L
CORES <- 4L
SORTEIOS_FORECAST <- 250L      # Centros condicionais; nao bandas preditivas.
BANDA_JURO <- c(1.75,2.25)     # Juro curto no FINAL do horizonte, em TODAS as confs.
SATELITE_VARIANCIA_MULT <- 1   # Teste 1 suave: escala da covariancia do satelite.
# Maior = condicionamento mais fraco; menor = mais forte. Nunca fixar em zero.
SATELITE_SIMULACOES <- 2000L   # Covariancia futura com os erros ARIMA selecionados.
MOSTRAR_RESIDUOS <- TRUE        # Fluxo e spread: tempo, ACF e histograma no R.
ESS_MINIMO <- 50
REESTIMAR <- FALSE             # Cache validado por dados, configuracao e codigo.
SEMENTE <- 123L
MODO_CV_ATIVO <- FALSE          # Uso interno; nao alterar para o forecast futuro.
CV_INICIO <- '2024 Oct'
CV_HORIZONTE <- 12L
CV_PASSO <- 1L
CV_S <- 500L                  # Rodada inicial; aumentar depois de avaliar coerencia.
CV_HIPER_S <- 1000L
CV_HIPER_BURN <- 500L
CV_SORTEIOS_FORECAST <- 100L
EXIGIR_FLUXO_EX_POST_COMPLETO <- FALSE
# Teste 4 fixa SOMENTE o fluxo realizado disponivel; demais meses ficam endogenos.
# Para 24 meses completamente ex post, use ORIGEM historica em TODOS os executores
# e EXIGIR_FLUXO_EX_POST_COMPLETO <- TRUE. Nunca completa realizado com estimativa.
# CV historico nao usa a meta terminal de 2028: banda somente no forecast futuro.

variaveis <- c('ust2y','brent','ip_mundial','atividade','cpi_core',
              'juro_curto','inclinacao','usdjpy')
