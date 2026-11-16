# Extensao local do amostrador bsvarSIGNs 3.0, sem modificar o pacote instalado.
# Fontes: github.com/bsvars/bsvarSIGNs, src/sample_SOE.cpp e bsvars_sign.cpp.
# Condicionar a normal matricial a A[-spread,fluxo]=0; beta do spread continua livre.
# A densidade das restricoes entra nos pesos, como no bloco SOE do pacote.
estimar_fluxo_restrito <- function(spec,S=2500L,max_tentativas=250000L,
                                  indices_hyper=seq_len(S),retornar_bruto=FALSE) {
  ns <- asNamespace('bsvarSIGNs')
  nativo <- function(nome,...) .Call(get(paste0('_bsvarSIGNs_',nome),ns),...)
  prior <- spec$prior$get_prior()
  stopifnot(length(prior$covid)==1L,is.finite(prior$covid),prior$covid>0)
  prior$B <- t(prior$A)
  for(n in c('Ysoc','Xsoc','Ysur','Xsur')) prior[[n]] <- t(prior[[n]])
  Y <- t(spec$data_matrices$Y); X <- t(spec$data_matrices$X)
  N <- ncol(Y); K <- ncol(X); p <- as.integer(spec$p); Nf <- as.integer(spec$num_foreign_vars)
  id <- spec$identification$get_identification()
  sinais <- id$sign_irf; sinais[is.na(sinais)] <- 0
  estrutural <- id$sign_structural; estrutural[is.na(estrutural)] <- 0
  Z <- get('get_Z',ns)(id$sign_irf)
  narrativa <- do.call(rbind,lapply(id$sign_narrative,function(n)
    c(c(S=1,A=2,B=3)[n$type],n$sign,n$var,n$shock,n$start-p,n$periods-1)))
  if(is.null(narrativa)) narrativa <- matrix(c(0,1,1,1,1,1),nrow=1)
  R1 <- c(unlist(lapply(0:(p-1),function(l) l*N+seq_len(Nf))), (N*p+1):K)
  R2 <- setdiff(seq_len(K),R1)
  eq_zero <- setdiff(seq_len(N),7L)
  post <- list(A=array(NA_real_,c(N,K,S)),B=array(NA_real_,c(N,N,S)),
    Q=array(NA_real_,c(N,N,S)),Sigma=array(NA_real_,c(N,N,S)),
    Theta0=array(NA_real_,c(N,N,S)),hyper=matrix(NA_real_,nrow(prior$hyper),S))
  log_pesos <- numeric(S); tentativas <- integer(S)
  for(s in seq_len(S)) {
    hyper <- prior$hyper[,1L+(indices_hyper[s]-1L)%%ncol(prior$hyper)]
    psi <- hyper[4:(N+3)]
    V0 <- diag(diag(prior$V)*c(rep(hyper[3]^2/psi,p),rep(1,K-N*p)))
    escala <- rep(1,nrow(Y)); covid <- as.integer(prior$covid)
    if(covid>0) {
      escala[covid+0:2] <- hyper[(N+4):(N+6)]
      depois <- seq.int(covid+3,nrow(Y))
      escala[depois] <- 1+(hyper[N+6]-1)*hyper[N+7]^seq_along(depois)
    }
    Ys <- Y/escala; Xs <- X/escala
    Yp <- rbind(prior$Ysoc/hyper[1],prior$Ysur/hyper[2],Ys)
    Xp <- rbind(prior$Xsoc/hyper[1],prior$Xsur/hyper[2],Xs)
    niw <- nativo('niw_cpp',Yp,Xp,prior$B,V0,diag(psi),as.integer(prior$nu))
    M <- niw[[1]]; V <- niw[[2]]
    V22i <- solve(V[R2,R2]); Vf <- matrix(0,K,K)
    Vf[R1,R1] <- V[R1,R1]-V[R1,R2]%*%V22i%*%V[R2,R1]
    Mf <- matrix(0,K,Nf)
    Mf[R1,] <- M[R1,1:Nf]-V[R1,R2]%*%V22i%*%M[R2,1:Nf]
    aceito <- FALSE
    for(t in seq_len(max_tentativas)) {
      Sigma <- solve(rWishart(1,as.numeric(niw[[4]]),solve(niw[[3]]))[,,1])
      sorteio <- nativo('sample_restricted_B_cpp',M,V,Sigma,p,as.integer(N),Nf,as.integer(K))
      A <- sorteio[[1]] # K x N
      C <- solve(Sigma[1:Nf,1:Nf],Sigma[1:Nf,(Nf+1):N])
      W <- cbind(diag(Nf),C)
      H <- t(W)%*%Sigma[1:Nf,1:Nf]%*%W
      E <- matrix(0,N,N)
      E[(Nf+1):N,(Nf+1):N] <- Sigma[(Nf+1):N,(Nf+1):N]-t(C)%*%Sigma[1:Nf,(Nf+1):N]
      media <- cbind(Mf,M[,(Nf+1):N]+(Mf-M[,1:Nf])%*%C)
      Cr <- H[eq_zero,eq_zero]*Vf[K,K]+E[eq_zero,eq_zero]*V[K,K]
      Cr <- (Cr+t(Cr))/2
      Cor <- vapply(eq_zero,function(j)
        as.vector(outer(Vf[,K],H[,j])+outer(V[,K],E[,j])),numeric(K*N))
      # Projecao gaussiana condicional: nao apenas zerar coeficientes sorteados.
      A <- A-matrix(Cor%*%solve(Cr,A[K,eq_zero]),K,N)
      A[K,eq_zero] <- 0
      media_r <- media[K,eq_zero]
      log_restricao <- -.5*(length(eq_zero)*log(2*pi)+
        2*sum(log(diag(chol(Cr))))+drop(t(media_r)%*%solve(Cr,media_r)))
      L <- t(chol(Sigma))
      q <- nativo('sample_Q',p,Ys,Xs,A,solve(L),L,prior,sinais,narrativa,
                   estrutural,Z,Nf,1L)
      if(is.finite(as.numeric(q[[3]]))) {
        post$A[,,s] <- t(A); post$Q[,,s] <- q[[1]]
        post$B[,,s] <- t(q[[1]])%*%solve(L)
        post$Sigma[,,s] <- Sigma; post$Theta0[,,s] <- L%*%q[[1]]
        post$hyper[,s] <- hyper
        log_pesos[s] <- as.numeric(sorteio[[2]])+log_restricao+as.numeric(q[[3]])
        tentativas[s] <- t; aceito <- TRUE; break
      }
    }
    if(!aceito) stop('Sem sorteio identificado no indice ',s,'. Aumente max_tentativas; nao aceite o ultimo candidato.')
    if(s%%5==0) message('BVAR fluxo: ',s,'/',S,' | ',format(Sys.time(),'%H:%M:%S'))
  }
  bruto <- list(post=post,log_pesos=log_pesos,tentativas=tentativas)
  if(retornar_bruto) return(bruto)
  finalizar_fluxo(list(bruto),spec)
}

# Os pesos sao normalizados APENAS depois de reunir todos os trabalhadores.
finalizar_fluxo <- function(brutos,spec) {
  S <- sum(vapply(brutos,function(x) length(x$log_pesos),integer(1)))
  post <- brutos[[1]]$post
  for(n in names(post)) {
    d <- dim(post[[n]])
    if(length(d)==3) post[[n]] <- array(unlist(lapply(brutos,function(x) x$post[[n]]),use.names=FALSE),c(d[1:2],S))
    else post[[n]] <- do.call(cbind,lapply(brutos,function(x) x$post[[n]]))
  }
  log_pesos <- unlist(lapply(brutos,function(x) x$log_pesos))
  tentativas <- unlist(lapply(brutos,function(x) x$tentativas))
  w <- exp(log_pesos-max(log_pesos)); ess <- sum(w)^2/sum(w^2)
  indices <- sample(seq_len(S),S,replace=TRUE,prob=w)
  for(n in names(post)) {
    if(length(dim(post[[n]]))==3) post[[n]] <- post[[n]][,,indices,drop=FALSE]
    else post[[n]] <- post[[n]][,indices,drop=FALSE]
  }
  post$ess <- ess
  objeto <- bsvarSIGNs::specify_posterior_bsvarSIGN$new(spec,post)
  attr(objeto,'diagnostico_restricoes') <- list(ess=ess,tentativas=tentativas,log_pesos=log_pesos)
  objeto
}

estimar_fluxo_paralelo <- function(spec,S,cores=4L,log='') {
  cl <- parallel::makePSOCKcluster(cores,outfile=log)
  on.exit(parallel::stopCluster(cl))
  parallel::clusterEvalQ(cl,library(bsvarSIGNs))
  parallel::clusterExport(cl,'estimar_fluxo_restrito',envir=environment())
  grupos <- split(seq_len(S),rep(seq_len(cores),length.out=S))
  brutos <- parallel::clusterMap(cl,function(indices,semente,espec) {
    set.seed(semente)
    estimar_fluxo_restrito(espec,S=length(indices),indices_hyper=indices,retornar_bruto=TRUE)
  },grupos,1000L+seq_along(grupos),MoreArgs=list(espec=spec))
  set.seed(123)
  finalizar_fluxo(brutos,spec)
}

# Voltar a unidades originais, como na finalizacao do BVAR principal.
normalizar_fluxo <- function(post,pesos) {
  Y <- sweep(post$last_draw$data_matrices$Y,2,sqrt(pesos),'/')
  X <- sweep(post$last_draw$data_matrices$X,2,sqrt(pesos),'/')
  N <- nrow(Y); T <- ncol(Y); S <- dim(post$posterior$A)[3]
  covid <- post$last_draw$prior$covid
  shocks <- sigma <- array(NA_real_,c(N,T,S))
  for(s in seq_len(S)) {
    escala <- rep(1,T); h <- post$posterior$hyper[,s]
    escala[covid+0:2] <- h[(N+4):(N+6)]
    depois <- seq.int(covid+3,T)
    escala[depois] <- 1+(h[N+6]-1)*h[N+7]^seq_along(depois)
    escala <- escala/sqrt(pesos)
    sigma[,,s] <- matrix(escala,N,T,byrow=TRUE)
    shocks[,,s] <- post$posterior$B[,,s]%*%sweep(Y-post$posterior$A[,,s]%*%X,2,escala,'/')
  }
  post$posterior$shocks <- shocks; post$posterior$sigma <- sigma
  post$last_draw$data_matrices$Y <- Y; post$last_draw$data_matrices$X <- X
  post$last_draw$prior$Y <- Y; post$last_draw$prior$X <- X
  attr(post,'pesos_exponenciais') <- list(rho=.995,peso=pesos)
  post
}
