# SOMENTE CVs 2, 3 e 4. Este executor nao inicia forecasts prospectivos.
# Nao altera os scripts dos modelos nem invalida os caches ja salvos.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco opcoes UST e fluxo/00_configuracao.R',encoding='UTF-8')
iniciar_testes()
source(file.path(pasta_testes,'cv_igual_bvar_original.R'),encoding='UTF-8')
REESTIMAR <- FALSE
SO_CV_OPCAO_1 <- FALSE
CVS_PARA_RODAR <- c(2L,3L,4L)
# O executor confere resultado.rds e a chave do CV antes de pular a opcao 1.
# REESTIMAR <- FALSE preserva tambem as origens salvas dos outros CVs.

retomar_pendencias <- function(executar=TRUE,somente_cv1=SO_CV_OPCAO_1) {
  chave <- chave_execucao()
  chave_cv <- chave_cv_original(chave)
  # Mesma ordem solicitada: 1-4, depois 5, depois 6-8; forecast antes de CV.
  fila <- tibble(
    etapa=c(rep('Forecast',4),rep('CV',4),'Forecast','CV',rep('Forecast',3),rep('CV',3)),
    teste=c(1:4,1:4,5L,5L,6:8,6:8),
    status='Pendente',erro=NA_character_,
    origens_salvas=NA_integer_,origens_esperadas=NA_integer_)
  bases_cv <- ler_bases_cv_original()
  ler_salvo <- function(arquivo,assinatura=chave) {
    tryCatch(cache_compativel(arquivo,assinatura),error=function(e) {
      warning('Arquivo nao pode ser reutilizado: ',arquivo,' | ',conditionMessage(e))
      NULL
    })
  }

  # 1. Conferir conclusao pelo RDS e sua chave, nao apenas pelo antigo status.csv.
  for(k in seq_len(nrow(fila))) {
    pasta <- if(fila$etapa[k]=='Forecast')
      file.path(pasta_testes,'resultados',paste0('opcao_',fila$teste[k]),'forecast') else
      file.path(raiz_cv_original,paste0('opcao_',fila$teste[k]))
    salvo <- ler_salvo(file.path(pasta,'resultado.rds'),
      if(fila$etapa[k]=='Forecast') chave else chave_cv)
    if(fila$etapa[k]=='Forecast') {
      completo <- !is.null(salvo) && nrow(salvo$tabela)>0L
    } else {
      origens <- origens_cv_original(fila$teste[k],bases_cv)$origem
      completo <- !is.null(salvo) && isTRUE(salvo$meta$completo) &&
        identical(salvo$meta$origens_esperadas,as.Date(origens))
      arquivos <- file.path(pasta,paste0('origem_',format(as.Date(origens),'%Y-%m'),'.rds'))
      fila$origens_salvas[k] <- sum(vapply(arquivos,function(a)
        !is.null(ler_salvo(a,chave_cv)),logical(1)))
      fila$origens_esperadas[k] <- length(origens)
    }
    if(isTRUE(completo)) fila$status[k] <- 'Concluido'
  }
  # Restringir a execucao aos CVs pedidos. Forecasts e demais opcoes ficam fora.
  stopifnot(length(CVS_PARA_RODAR)>0L,all(CVS_PARA_RODAR %in% seq_along(nomes_testes)))
  etapas <- if(somente_cv1) which(fila$etapa=='CV' & fila$teste==1L) else
    which(fila$etapa=='CV' & fila$teste %in% CVS_PARA_RODAR)
  print(fila[etapas,],n=Inf,width=Inf)
  message('Etapas concluidas serao puladas; no CV parcial, as origens salvas serao reutilizadas.')
  if(somente_cv1) message('SOMENTE CV 1: atividade, CPI, juro curto, spread 30Y-5Y e USD/JPY. Demais etapas nao serao executadas.')
  if(!executar) return(invisible(fila[etapas,])) # So mostra o plano; nao roda modelos.

  protegidos <- c(file.path(fonte_modelo,c('dados.rds','modelo.rds')),
    file.path(fonte_forecast,c('caminhos_novos.rds','forecast_bvar.rds','trajetorias modelos.rds')))
  antes <- tools::md5sum(protegidos)
  on.exit(stopifnot(identical(antes,tools::md5sum(protegidos))),add=TRUE)
  salvar_status <- function() {
    write_csv(select(fila,etapa,teste,status,erro),file.path(pasta_testes,'resultados','status.csv'))
    write_csv(fila,file.path(pasta_testes,'resultados','plano retomada.csv'))
  }
  salvar_status()

  # 2. Executar somente pendencias, reutilizando os mesmos executores e modelos.
  for(k in etapas) {
    numero <- fila$teste[k]
    if(fila$status[k]=='Concluido') {
      message('PULANDO ',fila$etapa[k],' ',numero,': resultado completo ja salvo.')
      next
    }
    message('\nRETOMANDO ',fila$etapa[k],' ',numero,' | ',nomes_testes[numero])
    fila$status[k] <- 'Em andamento'
    salvar_status()
    erro <- tryCatch({
      if(fila$etapa[k]=='Forecast') executar_forecast(numero,chave)
      else executar_cv(numero,chave) # O proprio CV pula cada origem com cache valido.
      NA_character_
    },error=conditionMessage)
    fila$erro[k] <- erro
    fila$status[k] <- if(is.na(erro)) 'Concluido' else 'Falhou'
    if(is.na(erro) && fila$etapa[k]=='CV')
      fila$origens_salvas[k] <- fila$origens_esperadas[k]
    salvar_status()
    if(!is.na(erro)) stop('Retomada interrompida na opcao ',numero,' (',fila$etapa[k],'): ',erro,
      '\nResultados anteriores preservados. Apos resolver, rode este mesmo arquivo novamente.')
    if(fila$etapa[k]=='Forecast') comparar_forecasts(chave)
    else if(!somente_cv1) comparar_cv(chave)
  }
  if(somente_cv1) {
    comparar_cv(chave)
    resultado <- ler_salvo(file.path(raiz_cv_original,'opcao_1','resultado.rds'),chave_cv)
    print(resultado$metricas,n=Inf,width=Inf)
    message('\nCV 1 concluido. Tabelas: resultados/CV original/opcao_1. Graficos das cinco variaveis: resultados/CV original/comparacao.')
    message('Execucao encerrada para analisar a opcao 1. Para continuar as demais, altere SO_CV_OPCAO_1 para FALSE.')
  } else {
    # Mostrar a comparacao mesmo quando todos os CVs selecionados ja estao salvos.
      comparar_cv(chave)
    message('\nCVs selecionados concluidos ou recuperados do cache. Nenhum forecast prospectivo executado.')
    message('Resultados das cinco variaveis: resultados/CV original/comparacao.')
  }
  invisible(fila[etapas,])
}

# Rode o arquivo inteiro para continuar. FALSE mostra somente o que esta faltando.
retomar_pendencias(executar=TRUE)
