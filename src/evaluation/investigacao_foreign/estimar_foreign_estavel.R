# Previsao reduzida: orientacao dos choques livre, com exogeneidade preservada.
# Mais candidatos tornam a reponderacao menos sensivel a um sorteio de peso extremo.
estimar_foreign_estavel <- function(Y,p=6,seed=123,propostas=25000,sorteios=2500){
 set.seed(seed)
 spec<-bsvarSIGNs::specify_bsvarSIGN$new(data=Y[,5:10],foreign=Y[,1:4],p=p,
  sign_structural=matrix(NA_real_,10,10),
  hyper_mu=FALSE,hyper_delta=FALSE,hyper_lambda=FALSE,hyper_psi=FALSE,mc.cores=1)
 # Esta funcao e para forecasting reduzido. Identificacao economica deve ser especificada separadamente.
 post<-bsvars::estimate(spec,S=propostas,show_progress=FALSE)
 idx<-seq_len(sorteios)
 # A reamostragem de importancia do pacote ja ocorreu; manter a quantidade final comum.
 # Manter o objeto do pacote, inclusive hyper e matrizes do treino para forecast().
 post$posterior<-lapply(post$posterior,function(z){
  dz<-dim(z)
  if(length(dz)==3L && dz[3]==propostas) return(z[,,idx,drop=FALSE])
  if(length(dz)==2L && dz[2]==propostas) return(z[,idx,drop=FALSE])
  if(is.null(dz) && length(z)==propostas) return(z[idx])
  z
 })
 post
}
