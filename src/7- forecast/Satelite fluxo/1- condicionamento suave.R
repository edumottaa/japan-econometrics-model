# Condicionamento suave do BVAR pelo satelite: tilting
# Repondera os sorteios do BVAR para a media do spread igualar o alvo do satelite.
# Peso de cada sorteio proporcional a exp(lambda*spread)
# Robertson, Tallman e Whiteman (2005); Kruger, Clark e Ravazzolo (2017)
tilting <- function(x,alvo) {
  x <- sweep(x,2,alvo); escala <- apply(x,2,sd)   # distancia de cada sorteio ao alvo
  pesos <- function(l) { z <- drop(x%*%(l/escala)); w <- exp(z-max(z)); w/sum(w) }
  entropia <- function(l) { z <- drop(x%*%(l/escala)); max(z)+log(mean(exp(z-max(z)))) }
  lambda <- optim(rep(0,ncol(x)),entropia,function(l) drop(pesos(l)%*%x)/escala,
    method='BFGS',control=list(maxit=1000,reltol=1e-14))$par
  pesos(lambda)
}
