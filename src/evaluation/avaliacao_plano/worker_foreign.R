args<-commandArgs(trailingOnly=TRUE)
source("C:/Users/dudup/OneDrive/japan-econometrics-model/src/evaluation/avaliacao_plano/common.R")
source(file.path(root,"src/evaluation/investigacao_foreign/estimar_foreign_estavel.R"))
out<-args[1];p<-as.integer(args[2]);dir.create(out,showWarnings=FALSE,recursive=TRUE)
for(nm in c("windows","logs","status"))dir.create(file.path(out,nm),showWarnings=FALSE)
old<-file.path(root,"data/processed/cv_comparacao_20261017_193604")
d<-readRDS(file.path(out,"data_frozen.rds"));cfg<-readRDS(file.path(out,"config.rds"));check_package_protocol(cfg)
trace("importance_sampling",where=asNamespace("bsvarSIGNs"),tracer=quote(assign(".stage_weights",posterior$posterior$w,envir=.GlobalEnv)),print=FALSE)
for(i in seq_along(cfg$origins)){
 file<-file.path(out,"windows",sprintf("foreign_p%d_w%03d.rds",p,i));if(file.exists(file))next
 write_status(file.path(out,"status",paste0("foreign_p",p,".json")),state="running",fold=i,lag=p,pid=Sys.getpid())
 n<-cfg$origins[i];train<-d |> slice_head(n=n);test12<-d |> slice((n+1):min(n+12,nrow(d)));test24<-d |> slice((n+1):min(n+24,nrow(d)))
 Y<-as.matrix(select(as_tibble(train),all_of(vars)));start<-Sys.time();log_line("foreign",p,"INICIO",i)
 warnings_seen<-character()
 res<-tryCatch(withCallingHandlers({
  small<-estimar_foreign_estavel(Y,p=p,seed=cfg$seed+i,propostas=25000,sorteios=2500)
  w<-.stage_weights;di<-diagnostico_signs(small,w,p,i)
  set.seed(100000+i);F12<-bsvarSIGNs::forecast(small,horizon=12)$forecasts
  set.seed(300000+i);F24<-bsvarSIGNs::forecast(small,horizon=24)$forecasts
  di$proposals<-25000L
  if(di$quality=="ok" && di$ess_importance<500)di$quality<-"low_importance_ess"
  sc12<-score_full_draws(F12,train,test12,i,paste0("bsvarSIGNs_estavel_p",p),di$quality)
  sc24<-score_full_draws(F24,train,test24,i,paste0("bsvarSIGNs_estavel_p",p),di$quality)
  list(diagnostic=di,scores12=sc12,scores24=sc24,forecasts12=F12[5:10,,,drop=FALSE],forecasts24=F24[5:10,,,drop=FALSE],forecast_variables=alvos,raw_weights=w,error=NULL)
 },warning=function(w){warnings_seen<<-c(warnings_seen,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e)list(error=conditionMessage(e),diagnostic=tibble(family="bsvarSIGNs",p=p,fold=i,quality="execution_failure")))
 res$warnings<-unique(warnings_seen);res$elapsed_seconds<-as.numeric(difftime(Sys.time(),start,units="secs"));res$seed<-cfg$seed+i;res$origin<-max(train$date)
 saveRDS(res,file,compress=TRUE)
 log_line("foreign",p,"FIM",i,"segundos",round(res$elapsed_seconds,1),"qualidade",res$diagnostic$quality,"erro",if(is.null(res$error))"nenhum" else res$error)
 rm(res);if(exists("small"))rm(small);if(exists("F12"))rm(F12,F24);gc(verbose=FALSE)
}
write_status(file.path(out,"status",paste0("foreign_p",p,".json")),state="complete",completed=47,lag=p,pid=Sys.getpid())