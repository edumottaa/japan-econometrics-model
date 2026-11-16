# CV do PIB: BVAR original e opcoes 1-4, com atividade prevista em cada origem.
# Reutiliza os CVs BVAR salvos: nao reestima nenhum BVAR.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/PIB atividade BVAR ex ante/00_configuracao.R',encoding='UTF-8')
iniciar_pib_cv()
REESTIMAR_PIB <- FALSE
MOSTRAR_GRAFICOS_PIB <- TRUE
executar_cv_pib()
