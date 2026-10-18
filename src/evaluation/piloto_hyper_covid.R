.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6",.libPaths()))
suppressPackageStartupMessages({library(fpp3);library(bsvarSIGNs);library(bsvars);library(coda);library(readr)})
root<-"C:/Users/dudup/OneDrive/japan-econometrics-model"
out<-file.path(root,"data/processed/piloto_hyper_covid_20261018_p6")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
dados<-readRDS(file.path(root,"data/processed/cv_comparacao_20261017_193604/data_frozen.rds")) |>
 filter(date<=yearmonth("2021 Jun")) |> select(-topix)
p<-6L
covid<-match(as.Date("2020-03-01"),as.Date(dados$date))-p
externas<-as.matrix(select(as_tibble(dados),ust2y,ip_mundial,brent,vix))
internas<-as.matrix(select(as_tibble(dados),atividade,cpi_core,juro_curto,inclinacao,usdjpy))
nomes<-c("mu","delta","lambda","covid_s0","covid_s1","covid_s2","covid_rho")
linhas<-c(1:3,13:16)
resultados<-vector("list",2)
for(i in 1:2){
 set.seed(c(123,456)[i]);inicio<-Sys.time()
 cat(format(Sys.time()),"INICIO cadeia",i,"\n");flush.console()
 hyper<-resumo<-NULL; resultados[[i]]<-tryCatch({
  spec<-specify_bsvarSIGN$new(data=internas,foreign=externas,p=p,
   sign_structural=matrix(NA_real_,9,9),
   hyper_mu=TRUE,hyper_delta=TRUE,hyper_lambda=TRUE,hyper_psi=FALSE,
   hyper_covid=covid,mc.cores=1)
  spec$estimate_hyper(S=5000,burn_in=2500)
  hyper<-t(spec$prior$hyper[linhas,,drop=FALSE]);colnames(hyper)<-nomes
  resumo<-tibble(cadeia=i,hiperparametro=nomes,
   media=colMeans(hyper),desvio=apply(hyper,2,sd),
   distintos=apply(hyper,2,function(x)length(unique(x))),
   ESS=as.numeric(effectiveSize(mcmc(hyper))))
  print(resumo);flush.console()
  saveRDS(list(hyper=hyper,resumo=resumo),file.path(out,paste0("hyper_cadeia_",i,".rds")))
  post<-bsvars::estimate(spec,S=2500,show_progress=FALSE)
  ponderacao<-tibble(cadeia=i,ESS_importancia=as.numeric(post$posterior$ess),
   conjuntos_distintos=ncol(unique(matrix(post$posterior$A,ncol=2500),MARGIN=2)),
   valores_finitos=all(is.finite(post$posterior$A))&&all(is.finite(post$posterior$Sigma)))
  print(ponderacao);flush.console()
  list(hyper=hyper,resumo=resumo,ponderacao=ponderacao,erro=NULL)
 },error=function(e)list(hyper=hyper,resumo=resumo,erro=conditionMessage(e)))
 resultados[[i]]$segundos<-as.numeric(difftime(Sys.time(),inicio,units="secs"))
 saveRDS(resultados[[i]],file.path(out,paste0("cadeia_",i,".rds")))
 cat(format(Sys.time()),"FIM cadeia",i,"erro",if(is.null(resultados[[i]]$erro))"nenhum" else resultados[[i]]$erro,"\n");flush.console()
}
write_csv(bind_rows(lapply(resultados,`[[`,"resumo")),file.path(out,"hiperparametros.csv"))
write_csv(bind_rows(lapply(resultados,`[[`,"ponderacao")),file.path(out,"ponderacao.csv"))
if(all(vapply(resultados,function(x)is.null(x$erro),logical(1)))){
 rh<-tryCatch(gelman.diag(mcmc.list(lapply(resultados,function(x)mcmc(x$hyper))),
  autoburnin=FALSE,multivariate=FALSE)$psrf,error=function(e)NULL)
 if(!is.null(rh))write_csv(tibble(hiperparametro=rownames(rh),Rhat=rh[,1],limite_superior=rh[,2]),file.path(out,"rhat.csv"))
}
saveRDS(resultados,file.path(out,"resultados.rds"))
cat("PILOTO CONCLUIDO\n")