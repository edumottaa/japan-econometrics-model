# Condicionamento conjunto da trajetoria. Mesma regra para todos os modelos.
# Centros condicionais por sorteio de parametros; NAO sao bandas preditivas.
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
