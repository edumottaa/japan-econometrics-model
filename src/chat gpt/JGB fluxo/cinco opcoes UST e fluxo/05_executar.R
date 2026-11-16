# Orquestracao chamada pelos code_run. Nada e executado ao dar source neste arquivo.
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

obter_satelite_teste <- function(d,chave) {
  arquivo <- file.path(pasta_testes,'resultados','cache',paste0('satelite_',
    if(d$modo_cv) 'CV_' else 'forecast_',format(as.Date(d$origem),'%Y-%m'),'.rds'))
  valor <- cache_compativel(arquivo,chave)
  if(!is.null(valor)) return(valor)
  guardar_cache(arquivo,chave,estimar_satelite_teste(d))
}

obter_referencia <- function(d,chave,cond,cenario,mesma_amostra_fluxo=FALSE) {
  tipo <- if(mesma_amostra_fluxo) 'controle_fluxo' else 'original'
  arquivo <- file.path(pasta_testes,'resultados','cache',paste0(tipo,'_',
    if(d$modo_cv) 'CV_' else 'forecast_',format(as.Date(d$origem),'%Y-%m'),
    '_',gsub('[^[:alnum:]]','_',cenario),'.rds'))
  valor <- cache_compativel(arquivo,chave)
  if(!is.null(valor)) return(valor)
  fit <- if(mesma_amostra_fluxo) estimar_configuracao(d,'controle_fluxo',chave) else obter_original(d,chave)
  guardar_cache(arquivo,chave,projetar_configuracao(fit,cond,d))
}

calcular_opcao <- function(numero,d,chave) {
  stopifnot(numero %in% seq_along(nomes_testes))
  sat <- if(numero %in% c(3L,4L)) obter_satelite_teste(d,chave) else NULL
  tipo <- c('ust30','diferencial','original','original','domestico',
    'fluxo_exogeno','fluxo_ust30','fluxo_diferencial')[numero]
  fit <- if(tipo=='original') obter_original(d,chave) else estimar_configuracao(d,tipo,chave)
  resultados <- list(); desenhos <- list(); insumos <- list()
  aviso_controle <- character()
  for(cen in unique(d$caminhos$cenario)) {
    caminho <- d$caminhos |> filter(cenario==cen) |> arrange(date)
    cond0 <- as.matrix(select(caminho,all_of(variaveis)))
    cond <- cond0; xfut <- NULL; suave <- NULL; alvo <- NULL
    if(numero %in% 1:2) {
      x <- if(numero==1L) d$futuro_aux$ust30 else d$futuro_aux$ust30-cond0[,1]
      stopifnot(all(is.finite(x)))
      xfut <- x-fit$media_x
      insumos[[cen]] <- tibble(date=d$datas,cenario=cen,ust30=d$futuro_aux$ust30,
        ust2y=cond0[,1],x_bruto=x,x_centrado=xfut,fonte=d$fonte_aux)
    }
    if(numero %in% 6:8) {
      # Mesma premissa de fluxo da rodada: media pos-YCC conhecida na origem.
      # Diferencial = UST30 menos o UST2Y do respectivo cenario.
      fluxo <- rep(d$media_fluxo,length(d$datas))
      x <- switch(as.character(numero),
        '6'=cbind(fluxo=fluxo),
        '7'=cbind(fluxo=fluxo,ust30=d$futuro_aux$ust30),
        '8'=cbind(fluxo=fluxo,diferencial=d$futuro_aux$ust30-cond0[,1]))
      stopifnot(identical(colnames(x),names(fit$media_x)),all(is.finite(x)))
      xfut <- sweep(x,2,fit$media_x,'-')
      centrado <- as_tibble(xfut) |> rename_with(~paste0(.x,'_centrado'))
      insumos[[cen]] <- bind_cols(tibble(date=d$datas,cenario=cen),as_tibble(x),centrado) |>
        mutate(fonte=paste('Fluxo = media pos-YCC |',d$fonte_aux))
    }
    if(numero %in% c(3L,4L)) {
      alvo <- montar_satelite_suave(sat,cond0,d)
      if(numero==3L) suave <- alvo else cond[,7] <- alvo$media
      insumos[[cen]] <- tibble(date=d$datas,cenario=cen,ois2=d$futuro_aux$ois2,
        fluxo=d$media_fluxo,alvo_spread_pb=100*alvo$media,
        desvio_spread_pb=100*sqrt(diag(alvo$covariancia)),fonte=d$fonte_aux)
    }
    if(numero==5L) {
      # A variavel e domestica na estimacao. A premissa solicitada fixa sua
      # trajetoria futura na media pos-YCC, por condicionamento endogeno exato.
      # Nao e fluxo futuro livre nem um choque estrutural de fluxo identificado.
      cond <- cbind(cond,fluxo=rep(d$media_fluxo,length(d$datas)))
      insumos[[cen]] <- tibble(date=d$datas,cenario=cen,fluxo=d$media_fluxo,
        fonte='Fluxo domestico condicionado a media pos-YCC conhecida na origem')
    }
    referencia <- obter_referencia(d,chave,cond0,cen)
    teste <- projetar_configuracao(fit,cond,d,xfuturo=xfut,satelite_suave=suave)
    if(numero==4L) stopifnot(max(abs(teste$centro$inclinacao-alvo$media))<1e-7)
    if(numero==5L) stopifnot(max(abs(teste$centro$fluxo-d$media_fluxo))<1e-7)
    resultados[[paste0(cen,'_ref')]] <- referencia$centro |>
      mutate(cenario=cen,modelo='Referencia: BVAR original')
    resultados[[paste0(cen,'_teste')]] <- teste$centro |>
      mutate(cenario=cen,modelo=nomes_testes[numero])
    # O fluxo comeca em 2018. Um controle adicional separa o efeito da amostra.
    # Se seu posterior for insuficiente, nao substitui por resultado invalido:
    # publica a limitacao, com o efeito na mesma amostra ausente.
    controle_fluxo <- NULL
    if(numero %in% 5:8 && !length(aviso_controle)) {
      controle_fluxo <- tryCatch(obter_referencia(d,chave,cond0,cen,TRUE),
        error=function(e) {aviso_controle <<- conditionMessage(e); NULL})
      if(!is.null(controle_fluxo)) resultados[[paste0(cen,'_controle')]] <- controle_fluxo$centro |>
        mutate(cenario=cen,modelo='Controle: mesma amostra JSDA, sem fluxo')
    }
    desenhos[[cen]] <- list(teste=teste$centros_modelo,referencia=referencia$centros_modelo,
      controle_fluxo=if(is.null(controle_fluxo)) NULL else controle_fluxo$centros_modelo,
      variaveis=fit$vars,condicoes=cond,xfuturo=xfut,sinal_suave=suave)
    message(if(d$modo_cv) 'CV ' else 'FORECAST ',numero,' | ',cen,' concluido.')
  }
  tabela <- bind_rows(resultados) |>
    pivot_longer(any_of(c(variaveis,'fluxo')),names_to='variavel',values_to='valor') |>
    filter(is.finite(valor))
  ref <- tabela |> filter(modelo=='Referencia: BVAR original') |>
    select(date,cenario,variavel,referencia=valor)
  controle <- tabela |> filter(modelo=='Controle: mesma amostra JSDA, sem fluxo') |>
    select(date,cenario,variavel,controle_mesma_amostra=valor)
  tabela <- tabela |> left_join(ref,by=c('date','cenario','variavel')) |>
    left_join(controle,by=c('date','cenario','variavel')) |>
    mutate(teste=numero,efeito_pb=if_else(variavel %in% c('inclinacao','ust2y','juro_curto'),
      100*(valor-referencia),NA_real_),
      efeito_mesma_amostra_pb=if_else(variavel %in% c('inclinacao','ust2y','juro_curto'),
        100*(valor-controle_mesma_amostra),NA_real_))
  if(length(aviso_controle)) warning('Opcao ',numero,': controle de mesma amostra nao disponivel. ',
    'Comparacao com o original inclui mudanca de amostra. ',aviso_controle,immediate.=TRUE)
  list(tabela=tabela,desenhos=desenhos,insumos=bind_rows(insumos),satelite=sat,
    meta=list(teste=numero,nome=nomes_testes[numero],origem=d$origem,datas=d$datas,
      inicio=fit$inicio,n=nrow(fit$y),banda=d$banda,modo_cv=d$modo_cv,
      media_fluxo=d$media_fluxo,n_media_fluxo=d$n_media_fluxo,
      fonte_aux=d$fonte_aux,controle_fluxo_indisponivel=aviso_controle))
}

executar_forecast <- function(numero,chave=chave_execucao()) {
  destino <- file.path(pasta_testes,'resultados',paste0('opcao_',numero),'forecast')
  arquivo <- file.path(destino,'resultado.rds')
  salvo <- cache_compativel(arquivo,chave)
  if(!is.null(salvo)) {message('Forecast ',numero,': cache compativel.'); return(invisible(salvo))}
  d <- ler_insumos()
  valor <- calcular_opcao(numero,d,chave)
  dir.create(destino,recursive=TRUE,showWarnings=FALSE)
  write_csv(valor$tabela |> mutate(date=as.Date(date)),file.path(destino,'forecasts.csv'))
  write_csv(valor$tabela |> filter(date==max(date)) |> mutate(date=as.Date(date)),
    file.path(destino,'valores finais.csv'))
  write_csv(valor$insumos |> mutate(date=as.Date(date)),file.path(destino,'premissas.csv'))
  if(!is.null(valor$satelite)) exportar_satelite(valor$satelite,destino)
  writeLines(c(paste('Opcao',numero,nomes_testes[numero]),
    paste('Origem BVAR:',d$origem,'| inicio:',valor$meta$inicio),
    paste('Media do fluxo pos-YCC ate a origem:',d$media_fluxo,'| n:',d$n_media_fluxo),
    'Novas exogenas: contemporaneas, estimadas nas cinco equacoes domesticas; zero nas externas.',
    'Opcao 5: fluxo domestico no historico; futuro condicionado a media pos-YCC.',
    'Opcoes 6-8: fluxo exogeno em nivel, sem log; futuro = media pos-YCC do treino.',
    'Condicionamento conjunto; a referencia recalculada pode diferir do forecast antigo salvo.',
    'Meses sem UST2Y na trajetoria original: centro UST2Y salvo, igualmente em todos os testes.',
    'Juro curto terminal entre 1,75% e 2,25%. Valores publicados sao centros condicionais.',
    'Comparacao 5-8 x original inclui mudanca de amostra; consultar controle de mesma amostra.',
    valor$meta$controle_fluxo_indisponivel),file.path(destino,'metadados.txt'))
  guardar_cache(arquivo,chave,valor)
  invisible(valor)
}

rodar_lote <- function(testes=seq_along(nomes_testes),rodar_forecast=TRUE,rodar_cv=TRUE) {
  stopifnot(length(testes)>0L,all(testes %in% seq_along(nomes_testes)))
  chave <- chave_execucao()
  protegidos <- c(file.path(fonte_modelo,c('dados.rds','modelo.rds')),
    file.path(fonte_forecast,c('caminhos_novos.rds','forecast_bvar.rds','trajetorias modelos.rds')))
  antes <- tools::md5sum(protegidos)
  on.exit(stopifnot(identical(antes,tools::md5sum(protegidos))),add=TRUE)
  status <- list()
  if(rodar_forecast) {
    for(i in testes) {
      message('\nFORECAST ',i,'/',length(nomes_testes),' | ',nomes_testes[i])
      erro <- tryCatch({executar_forecast(i,chave); NA_character_},error=conditionMessage)
      status[[length(status)+1L]] <- tibble(etapa='Forecast',teste=i,
        status=if(is.na(erro)) 'Concluido' else 'Falhou',erro=erro)
      write_csv(bind_rows(status),file.path(pasta_testes,'resultados','status.csv'))
    }
    comparar_forecasts(chave)
    if(any(bind_rows(status)$status=='Falhou'))
      stop('Algum forecast falhou. Veja resultados/status.csv. CV nao iniciado; caches validos foram preservados.')
    message('\nTodos os forecasts solicitados concluidos. So agora comeca o CV.')
  }
  if(rodar_cv) {
    for(i in testes) {
      message('\nCV ',i,'/',length(nomes_testes),' | ',nomes_testes[i],' | horizonte 12 meses')
      erro <- tryCatch({executar_cv(i,chave); NA_character_},error=conditionMessage)
      status[[length(status)+1L]] <- tibble(etapa='CV',teste=i,
        status=if(is.na(erro)) 'Concluido' else 'Incompleto',erro=erro)
      write_csv(bind_rows(status),file.path(pasta_testes,'resultados','status.csv'))
    }
    comparar_cv(chave)
    if(any(bind_rows(status)$status=='Incompleto'))
      stop('CV incompleto. Veja status.csv; a comparacao identifica as janelas disponiveis e comuns.')
  }
  invisible(bind_rows(status))
}
