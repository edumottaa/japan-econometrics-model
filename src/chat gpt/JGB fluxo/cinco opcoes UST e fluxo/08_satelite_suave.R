# Opcoes 3/4: preservar a posterior do BVAR atual e condicionar ao satelite.
carregar_bvar_original <- function(d) {
  banco_original <- d$historico |> arrange(date)
  if(d$origem!=max(banco_original$date))
    stop('Opcoes 3/4 usam o BVAR salvo na origem atual. Para origens historicas use o CV, que reestima so com dados de treino.')
  post <- readRDS(file.path(fonte_modelo,'modelo.rds'))
  y <- as.matrix(select(banco_original,all_of(variaveis)))
  p <- post$last_draw$p
  stopifnot(p==P,nrow(post$posterior$A)==8L,
    max(abs(t(y[-seq_len(p),,drop=FALSE])-post$last_draw$data_matrices$Y))<1e-8,
    is.finite(post$posterior$ess),post$posterior$ess>=ESS_MINIMO)
  list(modelo=post,y=y,vars=variaveis,tipo='original salvo',
    inicio=min(banco_original$date))
}

montar_satelite_suave <- function(sat,cond,d) {
  h <- nrow(cond)
  delta <- sat$satelite$delta_pb[match(d$datas,sat$satelite$date)]
  cov_delta <- sat$cov_delta_pb2
  stopifnot(identical(dim(cov_delta),c(h,h)),all(is.finite(cov_delta)),
    is.finite(SATELITE_VARIANCIA_MULT),SATELITE_VARIANCIA_MULT>0,
    length(delta)==h,all(is.finite(delta)))
  # Acumular a covariancia futura da equacao selecionada, respeitando as ancoras.
  # Nao pressupoe erro branco e nao trata os horizontes como independentes.
  L <- matrix(0,h,h); alvo_pb <- numeric(h)
  anterior <- 100*tail(d$banco$inclinacao,1)
  for(t in seq_len(h)) {
    if(is.finite(cond[t,7])) {
      alvo_pb[t] <- 100*cond[t,7]
    } else {
      alvo_pb[t] <- anterior+delta[t]
      if(t>1L) L[t,] <- L[t-1L,]
      L[t,t] <- 1
    }
    anterior <- alvo_pb[t]
  }
  list(media=alvo_pb/100,
    covariancia=SATELITE_VARIANCIA_MULT*L%*%cov_delta%*%t(L)/10000,
    nota='Cenario suave; covariancia acumulada das inovacoes da equacao do spread, condicional aos insumos. Nao inclui incerteza da media do fluxo ou dos parametros; nao presume informacao independente do BVAR.')
}

aplicar_sinal_suave <- function(mu,V,indices,alvo,R) {
  # Sinal probabilistico: alvo = selecao(Y) + erro com covariancia R.
  # O alvo NAO e copiado para o forecast; combina-se com a distribuicao do BVAR.
  stopifnot(length(indices)==length(alvo),nrow(R)==length(indices),
    ncol(R)==length(indices),all(is.finite(R)))
  ganho <- V[,indices,drop=FALSE]%*%
    chol2inv(chol(V[indices,indices,drop=FALSE]+R))
  novo_mu <- mu+drop(ganho%*%(alvo-mu[indices]))
  novo_V <- V-ganho%*%V[indices,,drop=FALSE]
  list(media=novo_mu,covariancia=(novo_V+t(novo_V))/2)
}
