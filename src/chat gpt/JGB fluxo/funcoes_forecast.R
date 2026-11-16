# Mesmo condicionamento para as variantes: trajetoria inteira e banda terminal.
# Todos os calculos usam matrizes pequenas do VAR; a complexidade fica neste apoio.
source('src/7- forecast/bvar estrutural/experimento oferta estrutural central/funcoes gaussianas.R',
       encoding='UTF-8')
centro_banda <- function(mu,sd,limites) {
  a <- (limites[1]-mu)/sd; b <- (limites[2]-mu)/sd
  direita <- mu<=mean(limites)
  maior <- pnorm(if(direita) a else b,lower.tail=!direita,log.p=TRUE)
  menor <- pnorm(if(direita) b else a,lower.tail=!direita,log.p=TRUE)
  u <- (seq_len(200)-.5)/200
  log_prob <- maior+log1p(-u*(-expm1(menor-maior)))
  mu+sd*mean(qnorm(log_prob,lower.tail=!direita,log.p=TRUE))
}
projetar_bvar <- function(post,y,condicoes,xfuturo=NULL,spread_fixo=NULL,
                         banda=c(1.75,2.25),sorteios=250L) {
  h <- nrow(condicoes); N <- ncol(y); p <- as.integer(post$last_draw$p)
  idx <- unique(round(seq(1,dim(post$posterior$A)[3],length.out=min(sorteios,dim(post$posterior$A)[3]))))
  caminhos <- array(NA_real_,c(h,N,length(idx)))
  cond <- condicoes
  if(!is.null(spread_fixo)) {
    usar <- is.finite(spread_fixo)
    cond[usar,7] <- spread_fixo[usar]/100
  }
  for(k in seq_along(idx)) {
    s <- idx[k]; A <- post$posterior$A[,,s]
    constantes <- outer(rep(1,h),A[,N*p+1])+outer(seq_len(h)/12,A[,N*p+2])
    if(!is.null(xfuturo)) constantes <- constantes+outer(xfuturo,A[,N*p+3])
    m <- list(p=p,A=lapply(seq_len(p),function(l) A[,N*(l-1)+seq_len(N),drop=FALSE]),
      constantes_futuras=constantes,sigma=post$posterior$Sigma[,,s],
      y=y,centro=rep(0,N),variaveis=colnames(y))
    # Propagar tambem o decaimento COVID futuro, como no forecast do pacote.
    f <- prever_var_condicionado(m,matrix(NA_real_,h,N))
    covid <- post$last_draw$prior$covid
    if(!is.null(covid) && covid>0) {
      hiper <- post$posterior$hyper[,s]
      escala <- 1+(hiper[N+6]-1)*hiper[N+7]^(nrow(y)-p+seq_len(h)-covid-2)
      L <- t(chol(f$covariancia))
      f$covariancia <- tcrossprod(sweep(L,2,rep(escala,each=N),'*'))
    }
    mu <- as.vector(t(f$media))
    fixos <- as.vector(t(cond)); idx_fixo <- which(is.finite(fixos))
    if(length(idx_fixo)) {
      ganho <- f$covariancia[,idx_fixo,drop=FALSE]%*%
        chol2inv(chol(f$covariancia[idx_fixo,idx_fixo,drop=FALSE]))
      mu <- mu+drop(ganho%*%(fixos[idx_fixo]-mu[idx_fixo]))
      f$covariancia <- f$covariancia-ganho%*%f$covariancia[idx_fixo,,drop=FALSE]
      f$covariancia <- (f$covariancia+t(f$covariancia))/2
    }
    alvo <- (h-1L)*N+6L
    v <- f$covariancia[alvo,alvo]
    if(v>1e-12) {
      terminal <- centro_banda(mu[alvo],sqrt(v),banda)
      mu <- mu+f$covariancia[,alvo]/v*(terminal-mu[alvo])
    }
    caminhos[,,k] <- matrix(mu,h,N,byrow=TRUE)
    stopifnot(max(abs(caminhos[,,k][is.finite(cond)]-cond[is.finite(cond)]))<1e-7)
  }
  stopifnot(all(is.finite(caminhos)),
            all(caminhos[h,6,]>=banda[1]-1e-7),all(caminhos[h,6,]<=banda[2]+1e-7))
  # Centros condicionais, sem sortear inovacoes futuras. Nao sao bandas preditivas.
  for(j in c(2,3,4,5,8)) caminhos[,j,] <- exp(caminhos[,j,]/100)
  resultado <- as_tibble(apply(caminhos,c(1,2),mean),.name_repair='minimal')
  names(resultado) <- colnames(y)
  resultado
}
