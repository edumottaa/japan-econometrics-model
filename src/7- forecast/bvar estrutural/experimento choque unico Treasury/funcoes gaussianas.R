# VAR reduzido por MQO e condicionamento gaussiano da trajetoria inteira.
# O condicionamento usa tambem informacao dos meses seguintes, nao apenas um mes por vez.
ajustar_var <- function(y,max_lag=6L) {
  y <- as.matrix(y); stopifnot(all(is.finite(y)))
  n <- ncol(y); centro <- colMeans(y); z <- sweep(y,2,centro,'-')
  linhas <- (max_lag+1L):nrow(y)
  ajustes <- lapply(seq_len(max_lag),function(p) {
    X <- cbind(1,do.call(cbind,lapply(seq_len(p),function(l) z[linhas-l,,drop=FALSE])))
    fit <- lm.fit(X,z[linhas,,drop=FALSE])
    if(fit$rank<ncol(X)) return(NULL)
    E <- as.matrix(fit$residuals); B <- as.matrix(fit$coefficients)
    sigma <- crossprod(E)/(nrow(X)-ncol(X))
    bic <- nrow(X)*as.numeric(determinant(crossprod(E)/nrow(X),logarithm=TRUE)$modulus)+
      ncol(X)*n*log(nrow(X))
    list(p=p,B=B,sigma=sigma,residuos=E,BIC=bic,linhas=linhas)
  })
  bics <- vapply(ajustes,function(a) if(is.null(a)) Inf else a$BIC,numeric(1))
  stopifnot(any(is.finite(bics)))
  m <- ajustes[[which.min(bics)]]
  m$A <- lapply(seq_len(m$p),function(l) t(m$B[1L+(l-1L)*n+seq_len(n),,drop=FALSE]))
  m$constante <- m$B[1,]; m$centro <- centro; m$y <- y; m$variaveis <- colnames(y)
  m$teste_lags <- data.frame(p=seq_len(max_lag),BIC=bics)
  F <- do.call(cbind,m$A)
  if(m$p>1L) F <- rbind(F,cbind(diag(n*(m$p-1L)),matrix(0,n*(m$p-1L),n)))
  m$raiz_maxima <- max(Mod(eigen(F,only.values=TRUE)$values))
  m
}

prever_var_condicionado <- function(m,condicoes) {
  condicoes <- as.matrix(condicoes); h <- nrow(condicoes); n <- length(m$centro)
  stopifnot(ncol(condicoes)==n)
  hist <- sweep(m$y,2,m$centro,'-'); medias <- matrix(NA_real_,h,n)
  for(t in seq_len(h)) {
    constante <- if(is.null(m$constantes_futuras)) m$constante else m$constantes_futuras[t,]
    medias[t,] <- constante+Reduce('+',lapply(seq_len(m$p),function(l)
      drop(m$A[[l]]%*%hist[nrow(hist)-l+1L,])))
    hist <- rbind(hist,medias[t,])
  }
  phi <- list(diag(n))
  if(h>1) for(k in 1:(h-1L)) phi[[k+1L]] <- Reduce('+',lapply(seq_len(min(k,m$p)),
    function(l) m$A[[l]]%*%phi[[k-l+1L]]))
  G <- matrix(0,h*n,h*n)
  for(t in seq_len(h)) for(k in seq_len(t))
    G[(t-1L)*n+seq_len(n),(k-1L)*n+seq_len(n)] <- phi[[t-k+1L]]
  V <- G%*%kronecker(diag(h),m$sigma)%*%t(G)
  mu <- as.vector(t(medias))
  fixos <- as.vector(t(sweep(condicoes,2,m$centro,'-'))); idx <- which(!is.na(fixos))
  if(length(idx)) {
    inversa <- chol2inv(chol(V[idx,idx,drop=FALSE]))
    K <- V[,idx,drop=FALSE]%*%inversa
    mu <- mu+drop(K%*%(fixos[idx]-mu[idx]))
    V <- V-K%*%V[idx,,drop=FALSE]
  }
  media <- sweep(matrix(mu,h,n,byrow=TRUE),2,m$centro,'+')
  colnames(media) <- m$variaveis; V <- (V+t(V))/2
  erro <- if(length(idx)) max(abs(as.vector(t(media))[idx]-as.vector(t(condicoes))[idx])) else 0
  stopifnot(all(is.finite(media)),erro<1e-8,min(eigen(V,symmetric=TRUE,only.values=TRUE)$values)>-1e-6)
  list(media=media,covariancia=V,erro_condicoes=erro)
}

decompor_var <- function(m,previsao,alvo,datas) {
  k <- match(alvo,m$variaveis); n <- length(m$variaveis)
  hist <- sweep(m$y,2,m$centro,'-'); out <- vector('list',nrow(previsao$media))
  for(t in seq_len(nrow(previsao$media))) {
    parcelas <- vapply(seq_len(n),function(j) sum(vapply(seq_len(m$p),function(l)
      m$A[[l]][k,j]*hist[nrow(hist)-l+1L,j],numeric(1))),numeric(1))
    referencia <- m$centro[k]+m$constante[k]
    ajuste <- previsao$media[t,k]-referencia-sum(parcelas)
    out[[t]] <- data.frame(date=datas[t],driver=c('referencia',m$variaveis,'ajuste_condicionado'),
      contribuicao=c(referencia,parcelas,ajuste))
    hist <- rbind(hist,previsao$media[t,]-m$centro)
  }
  d <- do.call(rbind,out)
  erro <- max(abs(as.numeric(tapply(d$contribuicao,d$date,sum))-previsao$media[,k]))
  stopifnot(erro<1e-8)
  d
}

diagnosticar_var <- function(m) data.frame(variavel=m$variaveis,
  media_residuo=colMeans(m$residuos),desvio_residuo=apply(m$residuos,2,sd),
  Ljung_Box_12_p=vapply(seq_along(m$variaveis),function(j)
    Box.test(m$residuos[,j],lag=12,type='Ljung-Box',fitdf=m$p)$p.value,numeric(1)),
  p=m$p,raiz_maxima=m$raiz_maxima)
