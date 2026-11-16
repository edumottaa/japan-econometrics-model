# Usa o posterior estrutural original. Fluxo nao altera a estimacao do BVAR.
executar_forecast_satelite <- function(chave=chave_execucao()) {
  destino <- file.path(pasta_testes,'resultados/opcao_3/forecast')
  arquivo <- file.path(destino,'resultado.rds')
  valor <- cache_compativel(arquivo,chave)
  if(is.null(valor)) {
    d <- ler_insumos(); sat <- estimar_satelite_teste(d); fit <- carregar_bvar_original(d)
    resultados <- desenhos <- insumos <- list()
    for(cen in unique(d$caminhos$cenario)) {
      caminho <- d$caminhos |> filter(cenario==cen) |> arrange(date)
      cond <- as.matrix(select(caminho,all_of(variaveis)))
      sinal <- montar_satelite_suave(sat,cond,d)
      ref <- projetar_configuracao(fit,cond,d)
      teste <- projetar_configuracao(fit,cond,d,satelite_suave=sinal)
      resultados[[paste0(cen,'_ref')]] <- ref$centro |>
        mutate(cenario=cen,modelo='Referencia: BVAR original')
      resultados[[cen]] <- teste$centro |> mutate(cenario=cen,modelo=nomes_testes[3])
      desenhos[[cen]] <- list(teste=teste$centros_modelo,referencia=ref$centros_modelo,
        variaveis=fit$vars,condicoes=cond,sinal_suave=sinal)
      insumos[[cen]] <- tibble(date=d$datas,cenario=cen,ois2=d$futuro_aux$ois2,
        fluxo=d$media_fluxo,alvo_spread_pb=100*sinal$media,
        desvio_spread_pb=100*sqrt(diag(sinal$covariancia)),fonte=d$fonte_aux)
    }
    tabela <- bind_rows(resultados) |> pivot_longer(all_of(variaveis),names_to='variavel',values_to='valor')
    ref <- tabela |> filter(modelo=='Referencia: BVAR original') |>
      select(date,cenario,variavel,referencia=valor)
    tabela <- tabela |> left_join(ref,by=c('date','cenario','variavel')) |>
      mutate(teste=3L,efeito_pb=if_else(variavel %in% c('inclinacao','ust2y','juro_curto'),
        100*(valor-referencia),NA_real_))
    valor <- list(tabela=tabela,desenhos=desenhos,insumos=bind_rows(insumos),satelite=sat,
      meta=list(teste=3L,nome=nomes_testes[3],origem=d$origem,datas=d$datas,
        inicio=fit$inicio,n=nrow(fit$y),banda=d$banda,modo_cv=FALSE,
        media_fluxo=d$media_fluxo,n_media_fluxo=d$n_media_fluxo,fonte_aux=d$fonte_aux))
    guardar_cache(arquivo,chave,valor)
  }
  dir.create(destino,recursive=TRUE,showWarnings=FALSE)
  write_csv(valor$tabela |> mutate(date=as.Date(date)),file.path(destino,'forecasts.csv'))
  write_csv(valor$insumos |> mutate(date=as.Date(date)),file.path(destino,'premissas.csv'))
  write_csv(valor$tabela |> filter(date==max(date)) |> mutate(date=as.Date(date)),
    file.path(destino,'valores finais.csv'))
  # Interface para JGB2Y/spreads/curva: valores fisicos, mesmos nomes do forecast original.
  resumo <- valor$tabela |> filter(modelo==nomes_testes[3]) |>
    transmute(date=as.Date(date),cenario,variavel,media=valor)
  write_csv(resumo,file.path(pasta_testes,'resumo_forecast.csv'))
  write_csv(resumo,file.path(pasta_testes,'resumo.csv'))
  saveRDS(list(origem=valor$meta$origem,previsoes=resumo,configuracao=valor$meta,
    nota='Centros condicionais; sem bandas preditivas BVAR nesta exportacao.'),
    file.path(pasta_testes,'forecast_satelite_suave.rds'))
  exportar_satelite(valor$satelite,destino)
  invisible(valor)
}

# Preparacao dos caminhos condicionais
# Funcoes preservadas da configuracao selecionada. Source apenas define funcoes.

ler_insumos <- function(origem=NULL,modo_cv=FALSE) {
  historico <- historico_satelite
  validar_mensal(historico,variaveis,'BVAR original')
  if(is.null(origem)) origem <- max(historico$date) else origem <- yearmonth(origem)
  stopifnot(origem %in% historico$date)
  jsda <- fluxo_estrangeiro
  validar_mensal(jsda,'fluxo','Compras liquidas estrangeiras JSDA')
  ust_diario <- ust30
  ois_diario <- ois2
  aux <- aux_satelite
  banco <- historico |> filter(date<=origem) |>
    left_join(select(aux,date,ust30,ois2,fluxo),by='date')
  # Exogenas tem a amostra completa; fluxo domestico comeca na primeira observacao JSDA.
  validar_mensal(banco,c(variaveis,'ust30'),'BVAR + UST30')
  banco_fluxo <- banco |> filter(date>=min(jsda$date))
  validar_mensal(banco_fluxo,c(variaveis,'fluxo'),'BVAR + fluxo domestico')
  media_pos <- jsda |> filter(date>=yearmonth(POS_YCC),date<=origem)
  if(!nrow(media_pos)) stop('Sem observacoes pos-YCC disponiveis ate a origem.')
  media_fluxo <- mean(media_pos$fluxo)

  if(modo_cv) {
    datas <- seq(origem+1L,origem+CV_HORIZONTE,by=1L)
    stopifnot(max(datas)<=max(historico$date))
    caminhos <- historico |> filter(date %in% datas) |> select(date,all_of(variaveis)) |>
      mutate(across(all_of(variaveis[4:8]),~NA_real_),cenario='Ex post: externos realizados')
    futuro_aux <- tibble(date=datas) |> left_join(select(aux,date,ust30,ois2),by='date')
    fonte_aux <- 'Realizado futuro (CV condicional ex post); fluxo = media do treino pos-YCC'
    banda <- NULL # A meta prospectiva de 2028 nao e conhecida nas origens historicas.
  } else {
    stopifnot(origem==max(historico$date))
    caminhos <- readRDS(file.path(fonte_forecast,'caminhos_novos.rds')) |> as_tibble() |>
      mutate(date=yearmonth(date)) |> filter(date>origem) |> arrange(cenario,date)
    # O caminho salvo deixa outubro sem UST2Y. Para UST30-UST2Y ser uma exogena
    # definida, completa APENAS essas lacunas com o centro UST2Y ja salvo, por cenario.
    # A mesma trajetoria completa e condicionada em todos os testes e na referencia.
    trajetorias <- readRDS(file.path(fonte_forecast,'trajetorias modelos.rds'))
    datas_salvas <- sort(unique(caminhos$date))
    centros_ust2 <- imap_dfr(trajetorias,function(a,cen) {
      stopifnot(dim(a)[1]==8L,dim(a)[2]==length(datas_salvas))
      tibble(date=datas_salvas,cenario=cen,
        ust2_salvo=rowMeans(matrix(a[1,,],nrow=length(datas_salvas))))
    })
    stopifnot(!anyDuplicated(select(centros_ust2,date,cenario)))
    caminhos <- caminhos |> left_join(centros_ust2,by=c('date','cenario')) |>
      mutate(ust2_completado=!is.finite(ust2y),ust2y=coalesce(ust2y,ust2_salvo)) |>
      select(-ust2_salvo)
    stopifnot(all(is.finite(caminhos$ust2y)))
    datas <- sort(unique(caminhos$date))
    curvas <- read_csv(ARQUIVO_CURVAS,show_col_types=FALSE)
    snapshot <- unique(as.Date(curvas$data_snapshot))
    stopifnot(length(snapshot)==1L,!is.na(snapshot))
    # Ate o mes do snapshot: medias mensais observadas. A partir do mes seguinte:
    # FWCV tenor constante, interpolado entre os horizontes exportados.
    # Preserva o ajuste de media mensal usado nos scripts anteriores; nao confunde
    # Forward Rate de um mes com o rendimento de um titulo de 30 anos.
    futuro_aux <- tibble(date=datas)
    for(v in c('ust30','ois2')) {
      pontos <- curvas |> filter(variavel==v) |> arrange(horizonte_meses)
      stopifnot(!anyDuplicated(pontos$horizonte_meses),min(pontos$horizonte_meses)==0L)
      h <- max(as.integer(datas-yearmonth(snapshot)))
      f <- approx(pontos$horizonte_meses,pontos$taxa_pct,xout=0:h,rule=1)$y
      if(any(!is.finite(f))) stop('A curva ',v,' nao cobre todo o horizonte. Atualize ARQUIVO_CURVAS.')
      diario <- if(v=='ust30') ust_diario else ois_diario
      spot <- diario[[v]][diario$date==snapshot]
      stopifnot(length(spot)==1L,is.finite(spot))
      termo <- tibble(date=yearmonth(snapshot)+seq_len(h),
        valor=spot+(head(f,-1)+tail(f,-1))/2-f[1])
      obs <- aux[[v]][match(datas,aux$date)]
      futuro_aux[[v]] <- ifelse(datas<=yearmonth(snapshot),obs,
        termo$valor[match(datas,termo$date)])
    }
    fonte_aux <- paste('FWCV + nowcasts; snapshot',snapshot,
      '| media mensal aproximada, ajuste de nivel preservado do projeto')
    banda <- BANDA_JURO
  }
  stopifnot(length(datas)>0L,identical(datas,seq(origem+1L,max(datas),by=1L)))
  validar_mensal(futuro_aux,c('ust30','ois2'),'UST30/OIS2 futuro')
  for(cen in unique(caminhos$cenario))
    stopifnot(identical(filter(caminhos,cenario==cen)$date,datas))
  list(banco=banco,banco_fluxo=banco_fluxo,historico=historico,jsda=jsda,aux=aux,
    origem=origem,datas=datas,caminhos=caminhos,futuro_aux=futuro_aux,
    media_fluxo=media_fluxo,n_media_fluxo=nrow(media_pos),banda=banda,
    modo_cv=modo_cv,fonte_aux=fonte_aux)
}


# Calculo da distribuicao futura do BVAR e da banda do juro.

media_truncada <- function(mu,sd,limites) {
  a <- (limites[1]-mu)/sd; b <- (limites[2]-mu)/sd
  direita <- mu<=mean(limites)
  maior <- pnorm(if(direita) a else b,lower.tail=!direita,log.p=TRUE)
  menor <- pnorm(if(direita) b else a,lower.tail=!direita,log.p=TRUE)
  u <- (seq_len(200)-.5)/200
  log_prob <- maior+log1p(-u*(-expm1(menor-maior)))
  mu+sd*mean(qnorm(log_prob,lower.tail=!direita,log.p=TRUE))
}

projetar_configuracao <- function(fit,cond,d,xfuturo=NULL,satelite_suave=NULL) {
  post <- fit$modelo; y <- fit$y
  N <- ncol(y); h <- nrow(cond); p <- post$last_draw$p
  stopifnot(ncol(cond)==N)
  if(!d$modo_cv) stopifnot(length(d$banda)==2L,
    d$banda[1]==1.75,d$banda[2]==2.25)
  indices <- unique(round(seq(1,dim(post$posterior$A)[3],
    length.out=min(SORTEIOS_FORECAST,dim(post$posterior$A)[3]))))
  centros <- array(NA_real_,c(h,N,length(indices)))
  for(k in seq_along(indices)) {
    s <- indices[k]; B <- post$posterior$A[,,s]
    A <- lapply(seq_len(p),function(l) B[,(l-1L)*N+seq_len(N),drop=FALSE])
    constantes <- outer(rep(1,h),B[,N*p+1L])+outer(seq_len(h)/12,B[,N*p+2L])
    if(!is.null(xfuturo)) {
      # Uma ou duas exogenas, na mesma ordem usada em cbind(tendencia,x).
      x <- as.matrix(xfuturo)
      stopifnot(nrow(x)==h,all(is.finite(x)),ncol(B)==N*p+2L+ncol(x))
      constantes <- constantes+x%*%t(B[,N*p+2L+seq_len(ncol(x)),drop=FALSE])
    }
    historia <- y; media <- matrix(NA_real_,h,N)
    for(t in seq_len(h)) {
      media[t,] <- constantes[t,]+Reduce('+',lapply(seq_len(p),function(l)
        drop(A[[l]]%*%historia[nrow(historia)-l+1L,])))
      historia <- rbind(historia,media[t,])
    }
    phi <- list(diag(N))
    if(h>1L) for(j in 1:(h-1L)) phi[[j+1L]] <- Reduce('+',
      lapply(seq_len(min(j,p)),function(l) A[[l]]%*%phi[[j-l+1L]]))
    G <- matrix(0,h*N,h*N)
    for(t in seq_len(h)) for(j in seq_len(t))
      G[(t-1L)*N+seq_len(N),(j-1L)*N+seq_len(N)] <- phi[[t-j+1L]]
    escalas <- rep(1,h)
    covid <- post$last_draw$prior$covid
    if(!is.null(covid) && covid>0) {
      hiper <- post$posterior$hyper[,s]
      escalas <- 1+(hiper[N+6]-1)*hiper[N+7]^(nrow(y)-p+seq_len(h)-covid-2)
    }
    V <- G%*%kronecker(diag(escalas^2),post$posterior$Sigma[,,s])%*%t(G)
    mu <- as.vector(t(media)); fixos <- as.vector(t(cond)); ix <- which(is.finite(fixos))
    if(length(ix)) {
      ganho <- V[,ix,drop=FALSE]%*%chol2inv(chol(V[ix,ix,drop=FALSE]))
      mu <- mu+drop(ganho%*%(fixos[ix]-mu[ix]))
      V <- V-ganho%*%V[ix,,drop=FALSE]; V <- (V+t(V))/2
    }
    if(!is.null(satelite_suave)) {
      usar <- which(is.finite(satelite_suave$media) & !is.finite(cond[,7]))
      if(length(usar)) {
        suave <- aplicar_sinal_suave(mu,V,(usar-1L)*N+7L,
          satelite_suave$media[usar],satelite_suave$covariancia[usar,usar,drop=FALSE])
        mu <- suave$media; V <- suave$covariancia
      }
    }
    alvo <- (h-1L)*N+6L
    if(!is.null(d$banda) && V[alvo,alvo]>1e-12) {
      terminal <- media_truncada(mu[alvo],sqrt(V[alvo,alvo]),d$banda)
      mu <- mu+V[,alvo]/V[alvo,alvo]*(terminal-mu[alvo])
    }
    centros[,,k] <- matrix(mu,h,N,byrow=TRUE)
    stopifnot(all(is.finite(centros[,,k])))
    if(!is.null(d$banda)) stopifnot(
      centros[h,6,k]>=d$banda[1]-1e-7,centros[h,6,k]<=d$banda[2]+1e-7)
    if(length(ix)) stopifnot(max(abs(mu[ix]-fixos[ix]))<1e-7)
  }
  # Guardar tambem unidades do modelo, para auditoria e futura avaliacao.
  bruto <- centros
  for(j in match(c('brent','ip_mundial','atividade','cpi_core','usdjpy'),fit$vars))
    centros[,j,] <- exp(centros[,j,]/100)
  centro <- as_tibble(apply(centros,c(1,2),mean),.name_repair='minimal')
  names(centro) <- fit$vars
  centro <- centro |> mutate(date=d$datas,.before=1)
  list(centro=centro,centros_modelo=bruto,indices=indices)
}
