args<-commandArgs(trailingOnly=TRUE)
source("C:/Users/dudup/OneDrive/japan-econometrics-model/src/evaluation/avaliacao_plano/common.R")
out<-args[1];dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(out,"config.rds")))check_package_protocol(readRDS(file.path(out,"config.rds")))
for(nm in c("windows","logs","status","figures"))dir.create(file.path(out,nm),showWarnings=FALSE)
old<-file.path(root,"data/processed/cv_comparacao_20261017_193604")
d<-readRDS(file.path(old,"data_frozen.rds"));cfg<-readRDS(file.path(old,"config.rds"));cfg$h<-24L;cfg$reestimate_step_months<-3L;cfg$forecast_frequency<-"monthly";cfg$targets<-alvos
cfg$signs_proposals<-25000L;cfg$signs_retained<-2500L;cfg$signs_normalization<-"none; arbitrary shock orientation is irrelevant to reduced-form forecasting"
cfg$logscore<-"negative log density; Gaussian KDE bw.nrd for simulated distributions; analytic normal benchmarks"
cfg$forecast_engine<-"package";cfg$crps_engine<-"fabletools"
cfg$created_full_utc<-format(Sys.time(),tz="UTC",usetz=TRUE)
saveRDS(cfg,file.path(out,"config.rds"));saveRDS(d,file.path(out,"data_frozen.rds"));jsonlite::write_json(cfg,file.path(out,"config.json"),pretty=TRUE,auto_unbox=TRUE)
rows<-paths<-vector("list",length(cfg$origins))
for(i in seq_along(cfg$origins)){
 n<-cfg$origins[i];train<-d |> slice_head(n=n);test<-d |> slice((n+1):min(n+24,nrow(d)));nh<-nrow(test);ws<-list()
 for(v in alvos){
  fc<-train |> select(date,valor=all_of(v)) |> model(ar=ARIMA(valor~pdq(d=1,q=0)+PDQ(D=0,Q=0)),naive=NAIVE(valor),drift=RW(valor~drift())) |> forecast(h=24) |> filter(date<=max(d$date))
  for(nm in unique(fc$.model)){
   a<-fc |> filter(.model==nm)
   ws[[paste(v,nm)]]<-score_full_normal(nm,i,v,train,test,mean(a$valor),sqrt(distributional::variance(a$valor)))
  }
 }
 fv<-train |> model(var=VAR(vars(atividade,cpi_core,juro_curto))) |> forecast(h=24)
 par<-distributional::parameters(fv$.distribution)
 for(v in c("atividade","cpi_core","juro_curto")){
  k<-match(v,c("atividade","cpi_core","juro_curto"))
  ws[[paste(v,"var")]]<-score_full_normal("var",i,v,train,test,map_dbl(par$mu[1:nh],~.x[k]),sqrt(map_dbl(par$sigma[1:nh],~.x[k,k])))
 }
 paths[[i]]<-train |> model(aicc=VAR(vars(ip_mundial,brent,vix,ust2y))) |> forecast(h=24) |> as_tibble() |> arrange(date) |> pull(.mean)
 rows[[i]]<-bind_rows(ws);log_line("PREPARACAO H24",i,"/47")
}
bs<-bind_rows(rows);stopifnot(all(is.finite(bs$log_score)),!anyDuplicated(bs[c("model","fold","variable","h")]))
saveRDS(bs,file.path(out,"benchmarks.rds"));saveRDS(paths,file.path(out,"external_paths.rds"));write_csv(bs,file.path(out,"benchmarks.csv"))
write_status(file.path(out,"status/prepare.json"),state="complete",completed=47)