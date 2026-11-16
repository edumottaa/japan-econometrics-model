library(bsvarSIGNs)

# Caminhos utilizados nas etapas de modelagem e forecast
raiz <- getwd()
fonte_modelo <- file.path(raiz, "data/processed/bvar_producao_tendencia_jgb30_5_20261112")
fonte_forecast <- file.path(raiz, "src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao")
fonte_dados <- file.path(raiz, "data/raw/satelite_fluxo")
pasta_testes <- file.path(raiz, "src/Outputs/Satelite fluxo suave")
ARQUIVO_CURVAS <- file.path(fonte_dados, "curvas_bloomberg.csv")

# 1. Parametros do BVAR original preservados
P <- 6L                         # Seis defasagens mensais
RHO <- 0.995                    # Peso exponencial dos dados
S <- 2500L                      # Sorteios de parametros do BVAR
HIPER_S <- 5000L                # Iteracoes para calibrar a volatilidade COVID
HIPER_BURN <- 2500L             # Iteracoes iniciais descartadas nessa calibracao
CORES <- 4L
SEMENTE <- 123L
ESS_MINIMO <- 50

# 2. Forecast condicionado pelo satelite
SORTEIOS_FORECAST <- 250L       # Subconjunto usado para os centros condicionais
BANDA_JURO <- c(1.75, 2.25)     # Banda do juro no ultimo mes da projecao
SATELITE_VARIANCIA_MULT <- 1
POS_YCC <- "2024 Apr"          # Primeiro mes inteiro depois do YCC

# 3. Variaveis do BVAR
variaveis <- c("ust2y", "brent", "ip_mundial", "atividade", "cpi_core",
               "juro_curto", "inclinacao", "usdjpy")

# 4. Execucao e resultados salvos
REESTIMAR <- FALSE
MOSTRAR_GRAFICOS <- FALSE

# Funcoes auxiliares para carregar as etapas e reutilizar os arquivos salvos.

# Funcoes preservadas da configuracao selecionada. Source apenas define funcoes.

validar_mensal <- function(x,colunas,rotulo) {
  if(!nrow(x) || anyDuplicated(x$date) ||
     !identical(x$date,seq(min(x$date),max(x$date),by=1L)) ||
     !all(is.finite(as.matrix(select(x,all_of(colunas))))))
    stop('Serie mensal incompleta/duplicada: ',rotulo,'. Nao preenche lacunas silenciosamente.')
  invisible(TRUE)
}


cache_compativel <- function(arquivo,chave,ignorar_reestimar=FALSE) {
  if((!REESTIMAR || ignorar_reestimar) && file.exists(arquivo)) {
    objeto <- readRDS(arquivo)
    if(identical(objeto$chave,chave)) return(objeto$valor)
  }
  NULL
}

guardar_cache <- function(arquivo,chave,valor) {
  dir.create(dirname(arquivo),recursive=TRUE,showWarnings=FALSE)
  saveRDS(list(chave=chave,valor=valor),arquivo)
  valor
}

# Controles auxiliares para preservar os resultados ja calculados.
# Nao alteram a especificacao econometrica e nao executam estimacoes.
pasta_legada <- file.path(raiz, 'src/chat gpt/JGB fluxo/cinco opcoes UST e fluxo')
# Compatibilidade com o indice da opcao 3 ja salva.
nomes_testes <- c(NA_character_, NA_character_, '3: Satelite suave')
domesticas_cv <- variaveis[4:8]
raiz_cv_original <- file.path(pasta_testes, 'resultados', 'CV original')

scripts_satelite <- c(
 'src/1- data_collection/Satelite fluxo/0- configuracao.R',
 'src/5- models/Satelite fluxo/0- configuracao.R',
 'src/2- data_cleaning/1- data_cleaning sSpread.R',
 'src/3- feature_engineering/Satelite fluxo/1- feature_engineering sSpread.R',
 'src/4- plot data/Satelite fluxo/1- graficos.R',
 'src/4- plot data/Satelite fluxo/2- graficos forecast e CV.R',
 'src/5- models/Satelite fluxo/1- modelo satelite.R',
 'src/5- models/Satelite fluxo/2- BVAR reduzido CV.R',
 'src/6- evaluation/Satelite fluxo/1- CV BVAR suave.R',
 'src/6- evaluation/Satelite fluxo/2- executar janela suave.R',
 'src/7- forecast/Satelite fluxo/1- condicionamento suave.R',
 'src/7- forecast/Satelite fluxo/2- forecast selecionado.R')
fontes_satelite <- function() c(file.path(fonte_modelo,c('dados.rds','modelo.rds')),
 file.path(fonte_forecast,c('caminhos_novos.rds','forecast_bvar.rds','trajetorias modelos.rds')),
 file.path(fonte_dados,c('DGS30_2026-11-04.csv','ois2_historico.csv',
  'fluxos_jsda_superlongos.csv','curvas_bloomberg.csv')))
carregar_satelite <- function() {
  stopifnot(as.character(packageVersion('bsvarSIGNs'))=='3.0')
  for(a in scripts_satelite[!grepl('/0- configuracao.R$',scripts_satelite)]) source(file.path(raiz,a),local=.GlobalEnv,encoding='UTF-8')
  invisible(TRUE)
}
chave_execucao <- function() {
  fontes <- fontes_satelite()
  if(!all(file.exists(fontes))) stop('Insumo ausente: ',paste(fontes[!file.exists(fontes)],collapse='; '))
  list(hash=tools::md5sum(fontes),codigo=tools::md5sum(file.path(raiz,scripts_satelite)),
    P=P,RHO=RHO,S=S,HIPER_S=HIPER_S,HIPER_BURN=HIPER_BURN,
    SORTEIOS_FORECAST=SORTEIOS_FORECAST,BANDA_JURO=BANDA_JURO,
    POS_YCC=POS_YCC,SATELITE_VARIANCIA_MULT=SATELITE_VARIANCIA_MULT,
    configuracao='OIS pos-YCC + fluxo lag2; futuro media pos-YCC; suave')
}
chave_cv_original <- function(chave=chave_execucao()) list(base=chave,
  procedimento='BVAR reduzido original; satelite com fluxo suave',h=12L,
  init=CV_ORIGINAL_INIT,passo=CV_ORIGINAL_PASSO,S=CV_ORIGINAL_S,
  hiper_S=CV_ORIGINAL_HIPER_S,burn=CV_ORIGINAL_BURN,benchmark=c('Naive','Drift'))

# Migracao explicita dos resultados da opcao 3; nenhuma estimacao e executada.
fontes_legadas_compativeis <- function(chave) {
  while(is.null(chave$hash) && !is.null(chave$base)) chave <- chave$base
  if(is.null(chave$hash)) return(FALSE)
  fonte <- fontes_satelite(); atual <- tools::md5sum(fonte)
  mesmos <- vapply(seq_along(fonte),function(i) {
    pos <- which(basename(names(chave$hash))==basename(fonte[i]))
    length(pos)==1L && identical(unname(chave$hash[pos]),unname(atual[i]))
  },logical(1))
  all(mesmos) && isTRUE(chave$P==P) && isTRUE(chave$RHO==RHO) && isTRUE(chave$S==S)
}
reaproveitar_satelite_salvo <- function() {
  chave <- chave_execucao(); copiados <- character()
  pares <- list(
    c(file.path(pasta_legada,'resultados/opcao_3/forecast'),
      file.path(pasta_testes,'resultados/opcao_3/forecast')),
    c(file.path(pasta_legada,'resultados/CV original/opcao_3'),
      file.path(raiz_cv_original,'opcao_3')))
  for(j in seq_along(pares)) {
    de <- pares[[j]][1]; para <- pares[[j]][2]
    arq <- file.path(de,'resultado.rds')
    if(!file.exists(arq)) next
    x <- readRDS(arq)
    if(!fontes_legadas_compativeis(x$chave)) next
    if(j==2L && !isTRUE(x$valor$meta$completo)) next
    dir.create(para,recursive=TRUE,showWarnings=FALSE)
    for(a in list.files(de,full.names=TRUE)) {
      if(dir.exists(a)) next
      destino <- file.path(para,basename(a))
      if(file.exists(destino)) next
      if(grepl('\\.rds$',a)) {
        z <- readRDS(a)
        if(!is.list(z) || is.null(z$chave) || !fontes_legadas_compativeis(z$chave)) next
        z$chave <- if(j==1L) chave else chave_cv_original(chave)
        z$origem_importada <- a
        saveRDS(z,destino)
      } else stopifnot(file.copy(a,destino))
      copiados <- c(copiados,destino)
    }
  }
  invisible(copiados)
}

