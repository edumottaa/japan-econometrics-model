# Carrega funcoes existentes; nao executa forecast nem CV.
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
pasta_original_ust <- file.path(raiz,'src/chat gpt/JGB fluxo/cinco opcoes UST e fluxo')
source(file.path(pasta_original_ust,'00_configuracao.R'),encoding='UTF-8')
iniciar_testes()
source(file.path(pasta_original_ust,'cv_igual_bvar_original.R'),encoding='UTF-8')
chave_original_ust <- chave_execucao()
chave_cv_antiga_ust <- chave_cv_original(chave_original_ust)
estimar_cv_original_ust <- estimar_reduzido_cv
chave_cv_func_original_ust <- chave_cv_original
janela_func_original_ust <- executar_janela_original
pasta_testes <- file.path(raiz,'src/chat gpt/JGB fluxo/satelite UST30 desde 2014')
raiz_cv_original <- file.path(pasta_testes,'resultados','CV original')
MOSTRAR_GRAFICOS <- FALSE
REESTIMAR <- FALSE
nomes_testes[3] <- 'BVAR + UST30 suave quando aprovado'
INICIO_SAT_UST <- yearmonth('2014 Jan')
# Mesmas nove origens comuns das quatro opcoes; treino do satelite desde 2014.
# Todas as configuracoes sao reestimadas/selecionadas em cada treino do CV.
source(file.path(pasta_testes,'01_modelo_satelite.R'),encoding='UTF-8')
source(file.path(pasta_testes,'02_bvar_forecast_cv.R'),encoding='UTF-8')
chave_execucao <- function() list(base=chave_original_ust,
  codigo_ust=tools::md5sum(file.path(pasta_testes,c('00_configuracao.R',
    '01_modelo_satelite.R','02_bvar_forecast_cv.R'))),inicio='2014-01',
  selecao='AICc entre significativos a 10% e Ljung-Box 12/24 a 5%; reavaliada por origem')
chave_cv_original <- function(chave=chave_execucao()) {
  z <- chave_cv_antiga_ust
  z$base <- chave
  z$codigo <- tools::md5sum(file.path(pasta_original_ust,
    c('cv_igual_bvar_original.R','amostragem_cv_original.R')))
  z
}
# O BVAR original nao muda: reaproveita ajustes reduzidos compativeis ja salvos.
# Cada nova previsao condicionada e seus escores ficam nesta pasta separada.
estimar_reduzido_cv <- function(d,tipo,chave) {
  arq <- file.path(pasta_original_ust,'resultados','CV original','cache',
    paste0(tipo,'_',format(as.Date(d$origem),'%Y-%m'),'.rds'))
  if(tipo=='base' && file.exists(arq)) {
    z <- readRDS(arq)
    if(identical(z$chave,chave_cv_antiga_ust)) return(z$valor)
  }
  estimar_cv_original_ust(d,tipo,chave)
}
