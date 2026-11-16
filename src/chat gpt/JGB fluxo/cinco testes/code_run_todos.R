# Ordem: seis forecasts completos, depois CV dos seis testes. Execucao manual no R.
# Testes 1/6 compartilham treinos: suave e rigido sao avaliados no mesmo CV.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco testes/00_configuracao.R',encoding='UTF-8')
source(file.path(pasta_testes,'05_executar.R'),encoding='UTF-8')
source(file.path(pasta_testes,'07_cv.R'),encoding='UTF-8')

RODAR_FORECAST <- TRUE
RODAR_CV <- TRUE
MOSTRAR_RESIDUOS <- TRUE # Mostra e salva tempo, ACF e histograma dos dois estagios.
testes_forecast <- c(1L,6L,2L,3L,4L,5L) # Primeiro as duas formas de condicionar.
testes_cv <- 1:5 # O bloco 1 calcula tambem o resultado do teste 6, sem novo treino.
nomes_testes <- c('Satelite suave no BVAR original salvo','Fluxo exogeno',
  'Fluxo domestico ex ante','Fluxo domestico ex post','Macro -> fluxo -> spread',
  'Satelite rigido no BVAR original salvo')

# 1. Somente depois de concluir os seis forecasts passa para o CV.
if(RODAR_FORECAST) {
  for(i in seq_along(testes_forecast)) {
    teste <- testes_forecast[i]
    message('\nFORECAST ',i,'/6 | teste ',teste,' | ',nomes_testes[teste])
    executar_teste(teste)
  }
  message('\nSeis forecasts concluidos. Comparacao em resultados/comparacao.')
}

# 2. Mesmos executores e configuracao comum; reutiliza caches compativeis.
# Se houver erro, interrompe no teste que falhou; nao publica sucesso parcial.
# Para retomar apenas o CV, altere RODAR_FORECAST para FALSE e rode novamente.
if(RODAR_CV) {
  for(teste in testes_cv) {
    message('\nCV: ',if(teste==1L) 'testes 1 e 6, treino compartilhado' else paste('teste',teste),
      ' | ',nomes_testes[teste])
    executar_cv_teste(teste)
  }
  message('\nCV dos seis testes concluido. Metricas em resultados/comparacao/CV spread.csv.')
}
