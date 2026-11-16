# 1. Configuracao do CV: mesmos valores ja utilizados
CV_HORIZONTE <- 12L
CV_ORIGINAL_H <- 12L
CV_ORIGINAL_INIT <- 180L
CV_ORIGINAL_PASSO <- 3L
CV_ORIGINAL_S <- 2500L
CV_ORIGINAL_HIPER_S <- 5000L
CV_ORIGINAL_BURN <- 2500L

# Funcoes preservadas da configuracao selecionada. Source apenas define funcoes.

forecast_reduzido_cv <- function(fit,d,id,xfuturo=NULL,spread_fixo=NULL,sinal_suave=NULL,fluxo_fixo=FALSE) {
  post <- fit$modelo; y <- fit$y; N <- ncol(y); H <- length(d$datas); p <- P
  tendencia_futura <- matrix(seq_len(H)/12,ncol=1)
  exogenas <- if(is.null(xfuturo)) tendencia_futura else cbind(tendencia_futura,xfuturo)
  condicoes <- cbind(as.matrix(select(d$caminhos,all_of(variaveis[1:3]))),matrix(NA_real_,H,N-3))
  if(!is.null(spread_fixo)) condicoes[,7] <- spread_fixo
  if(fluxo_fixo) condicoes[,9] <- d$media_fluxo
  set.seed(200000L+id+1000L)
  previsao_cv <- bsvarSIGNs::forecast(post,horizon=H,conditional_forecast=condicoes,
    exogenous_forecast=exogenas)
  # 3. Mesma correcao recursiva do script original; inclui os alvos adicionais
  # apenas nas opcoes que explicitamente condicionam spread ou fluxo.
  F <- previsao_cv$forecasts; medias <- previsao_cv$forecast_mean
  choques_cv <- F-medias
  fixos <- which(is.finite(condicoes[1,]))
  stopifnot(all(vapply(seq_len(H),function(j)
    identical(which(is.finite(condicoes[j,])),fixos),logical(1))))
  for(s in seq_len(dim(F)[3])) {
    estado_cv <- c(t(y[nrow(y)-(0:(p-1)),,drop=FALSE]),1,exogenas[1,])
    sigma <- post$posterior$Sigma[,,s]
    ganho <- sigma[,fixos,drop=FALSE]%*%solve(sigma[fixos,fixos,drop=FALSE])
    for(j in seq_len(H)) {
      media <- drop(post$posterior$A[,,s]%*%estado_cv)
      ajuste <- media[fixos]-medias[fixos,j,s]
      F[,j,s] <- media+choques_cv[,j,s]-drop(ganho%*%ajuste)
      F[fixos,j,s] <- condicoes[j,fixos]
      if(j<H) estado_cv <- c(F[,j,s],head(estado_cv,N*(p-1)),1,exogenas[j+1,])
    }
    if(!is.null(sinal_suave)) {
      # Sinal suave como observacao ruidosa da trajetoria, com covariancia
      # acumulada do satelite. Sorteios incluem inovacoes, alem de parametros.
      selecao <- diag(N)[fixos,,drop=FALSE]
      Tcond <- diag(N)-ganho%*%selecao
      A <- lapply(seq_len(p),function(l)
        Tcond%*%post$posterior$A[,(l-1)*N+seq_len(N),s,drop=FALSE][,,1])
      phi <- list(diag(N))
      if(H>1L) for(j in 1:(H-1L)) phi[[j+1L]] <- Reduce('+',lapply(seq_len(min(j,p)),
        function(l) A[[l]]%*%phi[[j-l+1L]]))
      G <- matrix(0,N*H,N*H)
      for(j in seq_len(H)) for(k in seq_len(j))
        G[(j-1L)*N+seq_len(N),(k-1L)*N+seq_len(N)] <- phi[[j-k+1L]]
      Q <- sigma-ganho%*%sigma[fixos,,drop=FALSE]; Q <- (Q+t(Q))/2
      escalas <- rep(1,H); covid <- post$last_draw$prior$covid
      if(!is.null(covid)) {
        hiper <- post$posterior$hyper[,s]
        escalas <- 1+(hiper[N+6]-1)*hiper[N+7]^(nrow(y)-p+seq_len(H)-covid-2)
      }
      V <- G%*%kronecker(diag(escalas^2),Q)%*%t(G)
      ix <- (seq_len(H)-1L)*N+7L; R <- sinal_suave$covariancia
      K <- V[,ix,drop=FALSE]%*%chol2inv(chol(V[ix,ix,drop=FALSE]+R))
      erro_sinal <- drop(t(chol(R))%*%rnorm(H))
      v <- as.vector(F[,,s])
      v <- v+drop(K%*%(sinal_suave$media-v[ix]-erro_sinal))
      F[,,s] <- matrix(v,N,H)
    }
  }
  stopifnot(all(is.finite(F)),
    max(abs(sweep(F[fixos,,,drop=FALSE],c(1,2),t(condicoes[,fixos,drop=FALSE]),'-')))<1e-7)
  F
}

distribuicoes_cv <- function(F,d,id,modelo) {
  map_dfr(domesticas_cv,function(alvo) {
    sorteios <- matrix(F[match(alvo,variaveis),,],nrow=length(d$datas))
    tibble(.id=id,.model=modelo,date=d$datas,origem=d$origem,h=seq_along(d$datas),
      variavel=alvo,valor=distributional::dist_sample(split(sorteios,row(sorteios)))) |>
      mutate(.mean=mean(valor))
  })
}

benchmarks_cv_original <- function(d,id) {
  treino <- d$banco |> select(date,all_of(domesticas_cv)) |> as_tsibble(index=date)
  map_dfr(domesticas_cv,function(alvo) {
    ajuste <- treino |> transmute(date,valor=.data[[alvo]]) |>
      model(Naive=NAIVE(valor),Drift=RW(valor~drift()))
    ajuste |> forecast(h=length(d$datas)) |> as_tibble() |>
      transmute(.id=id,.model,date,origem=d$origem,h=as.integer(date-d$origem),
        variavel=alvo,valor=unname(valor),.mean)
  })
}

escores_cv_original <- function(previsoes,bases) {
  observados <- bases$historico |> select(date,all_of(domesticas_cv)) |>
    pivot_longer(-date,names_to='variavel',values_to='observado')
  escores <- previsoes |> left_join(observados,by=c('date','variavel')) |>
    mutate(erro=.mean-observado,periodo=case_when(year(date)<=2019~'2015-2019',
      year(date)<=2021~'2020-2021',TRUE~'2022-2026'))
  escores$crps <- NA_real_
  # Mesma identidade eficiente usada em graficos e benchmarks CV modelo novo.R.
  for(m in unique(escores$.model)) {
    ix <- which(escores$.model==m)
    pars <- distributional::parameters(escores$valor[ix])
    if(m %in% c('Naive','Drift')) {
      z <- (escores$observado[ix]-pars$mu)/pars$sigma
      escores$crps[ix] <- ifelse(pars$sigma>0,
        pars$sigma*(z*(2*pnorm(z)-1)+2*dnorm(z)-1/sqrt(pi)),
        abs(escores$observado[ix]-pars$mu))
    } else escores$crps[ix] <- map2_dbl(pars$x,escores$observado[ix],function(x,y) {
      n <- length(x)
      mean(abs(x-y))-sum((2*seq_len(n)-n-1)*sort(x))/n^2
    })
  }
  stopifnot(all(is.finite(escores$erro)),all(is.finite(escores$crps)))
  escores
}

metricas_cv_original <- function(escores) {
  resumir <- function(x,grupos) x |> group_by(across(all_of(grupos))) |>
    summarise(origens=n_distinct(origem),casos=n(),RMSE=sqrt(mean(erro^2)),
      MAE=mean(abs(erro)),CRPS=mean(crps),.groups='drop')
  bind_rows(resumir(escores,c('teste','variavel','.model')) |> mutate(periodo='Geral'),
    resumir(escores,c('teste','variavel','periodo','.model'))) |>
    arrange(variavel,periodo,RMSE)
}

executar_cv <- function(numero,chave=chave_execucao()) {
  chave <- chave_cv_original(chave); bases <- ler_bases_cv_original()
  grade <- origens_cv_original(numero,bases)
  destino <- file.path(raiz_cv_original,paste0('opcao_',numero))
  dir.create(destino,recursive=TRUE,showWarnings=FALSE)
  salvo <- cache_compativel(file.path(destino,'resultado.rds'),chave)
  if(!is.null(salvo) && isTRUE(salvo$meta$completo)) return(invisible(salvo))
  previsoes <- status <- list()
  for(k in seq_len(nrow(grade))) {
    o <- grade$origem[k]; id <- grade$.id[k]
    arquivo <- file.path(destino,paste0('origem_',format(as.Date(o),'%Y-%m'),'.rds'))
    valor <- cache_compativel(arquivo,chave)
    erro <- NA_character_
    if(is.null(valor)) erro <- tryCatch({
      d <- janela_cv_original(o,bases)
      valor <- executar_janela_original(numero,d,id,chave)
      guardar_cache(arquivo,chave,valor)
      NA_character_
    },error=conditionMessage)
    if(is.na(erro)) previsoes[[k]] <- valor
    status[[k]] <- tibble(origem=as.Date(o),status=if(is.na(erro)) 'Concluido' else 'Falhou',erro=erro)
    write_csv(bind_rows(status),file.path(destino,'status por origem.csv'))
    message('CV original | opcao ',numero,' | ',k,'/',nrow(grade),' | ',o)
  }
  prev <- bind_rows(previsoes) |> mutate(teste=numero)
  if(!nrow(prev)) stop('Nenhuma janela concluida no CV original da opcao ',numero)
  escores <- escores_cv_original(prev,bases)
  meta <- list(teste=as.integer(numero),origens_esperadas=as.Date(grade$origem),
    completo=all(bind_rows(status)$status=='Concluido'),status=bind_rows(status),
    procedimento='BVAR reduzido, sem sinais/narrativas; janelas expansivas trimestrais',
    horizonte=CV_ORIGINAL_H)
  guardar_cache(file.path(destino,'resultado.rds'),chave,
    list(meta=meta,previsoes=prev,escores=escores,metricas=metricas_cv_original(escores)))
  write_csv(select(escores,-valor),file.path(destino,'previsoes e escores.csv'))
  write_csv(metricas_cv_original(escores),file.path(destino,'metricas por periodo.csv'))
  if(!meta$completo) stop('CV original incompleto da opcao ',numero,': veja status por origem.csv.')
  invisible(escores)
}


# Janelas de treino e teste
# Funcoes preservadas da configuracao selecionada. Source apenas define funcoes.

ler_bases_cv_original <- function() {
  historico <- historico_satelite |> select(date,all_of(variaveis))
  jsda <- fluxo_estrangeiro
  aux <- aux_satelite
  # 1. Mesma grade de janelas do script original, inclusive janelas finais parciais.
  cv_data <- historico |> as_tsibble(index=date) |>
    stretch_tsibble(.init=CV_ORIGINAL_INIT,.step=CV_ORIGINAL_PASSO)
  origens <- cv_data |> as_tibble() |> group_by(.id) |>
    summarise(origem=max(date),.groups='drop') |> filter(origem<max(historico$date))
  list(historico=historico,aux=aux,jsda=jsda,cv_data=cv_data,origens=origens)
}

origens_cv_original <- function(numero,bases=ler_bases_cv_original()) {
  grade <- bases$origens
  # 1/2 nao usam fluxo: toda a grade original. 3-8 precisam da premissa pos-YCC.
  # Nao se usa a media observada depois de uma origem antiga para prever o passado.
  if(numero %in% 3:8) grade <- grade |> filter(origem>=yearmonth(POS_YCC))
  grade
}

janela_cv_original <- function(origem,bases) {
  banco <- bases$historico |> filter(date<=origem) |>
    left_join(select(bases$aux,date,ust30,ois2,fluxo),by='date')
  datas <- bases$historico$date[bases$historico$date>origem &
    bases$historico$date<=origem+CV_ORIGINAL_H]
  media <- bases$jsda |> filter(date>=yearmonth(POS_YCC),date<=origem)
  caminhos <- bases$historico |> filter(date %in% datas) |>
    mutate(across(all_of(domesticas_cv),~NA_real_),cenario='Ex post')
  list(id=bases$origens$.id[match(origem,bases$origens$origem)],
    banco=banco,banco_fluxo=filter(banco,date>=min(bases$jsda$date)),
    historico=bases$historico,aux=bases$aux,jsda=bases$jsda,origem=origem,
    datas=datas,caminhos=caminhos,modo_cv=TRUE,banda=NULL,
    futuro_aux=tibble(date=datas) |> left_join(select(bases$aux,date,ust30,ois2),by='date'),
    media_fluxo=if(nrow(media)) mean(media$fluxo) else NA_real_,n_media_fluxo=nrow(media))
}

