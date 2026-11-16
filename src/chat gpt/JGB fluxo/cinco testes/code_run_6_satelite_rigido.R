# 6. Mesmo satelite do teste 1, fixando sua trajetoria do spread EXATAMENTE.
# Preserva posterior original do BVAR; juro terminal na mesma banda 1,75%-2,25%.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco testes/00_configuracao.R',encoding='UTF-8')
source(file.path(pasta_testes,'05_executar.R'),encoding='UTF-8')
RODAR_FORECAST <- TRUE
RODAR_CV <- FALSE # O mesmo CV calcula suave e rigido, compartilhando o treino.
MOSTRAR_RESIDUOS <- TRUE # Mesmos modelos selecionados do condicionamento suave.
if(RODAR_FORECAST) executar_teste(6L)
if(RODAR_CV) {
  source(file.path(pasta_testes,'07_cv.R'),encoding='UTF-8')
  executar_cv_teste(6L)
}
