.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6",.libPaths()))
suppressPackageStartupMessages(library(fpp3))
suppressPackageStartupMessages(library(bsvarSIGNs))
setwd("C:/Users/dudup/OneDrive/NT Macro/work/psi_diagnostic")
post<-readRDS("holdout_original_post.rds")
d<-readRDS("holdout_diagnostic_data.rds")
old<-readRDS("holdout_original_diagnostic.rds")
h<-d$h;p<-d$p;n<-ncol(d$train);m<-nrow(d$train);S<-dim(post$posterior$A)[3]
x0<-c(t(d$train[m:(m-p+1),]),1)
actual<-d$test[,"cpi_core"]
cat("TEST OF NATIVE LAG UPDATE WITHOUT FUTURE SHOCKS\n")
pzero<-post$clone(deep=TRUE)
pzero$posterior$Sigma<-post$posterior$Sigma*1e-18
last<-ncol(pzero$last_draw$data_matrices$X)
pzero$last_draw$data_matrices$X[,last]<-x0
set.seed(789)
zero_native<-bsvarSIGNs::forecast(pzero,horizon=h)$forecasts[6,,]
frozen<-matrix(NA_real_,h,S)
for(s in seq_len(S)){
 x<-x0; A<-post$posterior$A[,,s]
 for(j in seq_len(h)){
  yp<-drop(A%*%x); frozen[j,s]<-yp[6]
  x<-c(yp,x[(n+1):length(x)])
 }
}
cat("MAX NATIVE MINUS FROZEN-LAG RECURSION",max(abs(zero_native-frozen)),"\n")
cat("RMSE FROZEN",sqrt(mean((actual-rowMeans(frozen))^2)),"\n")
cat("RMSE CORRECT RECURSION",sqrt(mean((actual-rowMeans(old$conditional))^2)),"\n")
cat("STARTING-STATE LAG ERROR\n")
print(data.frame(variable=colnames(d$train),native_first_lag=post$last_draw$data_matrices$X[seq_len(n),last],correct_first_lag=d$train[m,]))

cat("STABLE SYNTHETIC AR2: SAME STATE, NO PARAMETER UNCERTAINTY\n")
syn<-pzero$clone(deep=TRUE)
syn$posterior$A<-array(0,dim(post$posterior$A))
for(s in seq_len(S)){
 syn$posterior$A[,seq_len(n),s]<-diag(1.5,n)
 syn$posterior$A[,n+seq_len(n),s]<-diag(-.6,n)
}
fsyn<-bsvarSIGNs::forecast(syn,horizon=h)$forecasts[6,,]
ar2correct<-numeric(h); a<-d$train[m,6]; b<-d$train[m-1,6]
for(j in seq_len(h)){new<-1.5*a-.6*b;ar2correct[j]<-new;b<-a;a<-new}
cat("TRUE ROOT MODULUS",sqrt(.6),"\n")
cat("H24 NATIVE",mean(fsyn[h,]),"H24 CORRECT",ar2correct[h],"\n")

cat("CORRECT FULL PREDICTIVE SIMULATION: SAME A AND SIGMA DRAWS\n")
rows<-list()
all_samples<-list()
for(seed in c(123,456,789)){
 set.seed(seed)
 forecasts<-array(NA_real_,c(n,h,S))
 for(s in seq_len(S)){
  x<-x0;A<-post$posterior$A[,,s];L<-t(chol(post$posterior$Sigma[,,s]))
  for(j in seq_len(h)){
   yp<-drop(A%*%x+L%*%rnorm(n));forecasts[,j,s]<-yp
   x<-c(yp,x[seq_len(n*(p-1))],1)
  }
 }
 samples<-forecasts[6,,]
 fc<-rowMeans(samples)
 rows[[as.character(seed)]]<-data.frame(seed=seed,RMSE=sqrt(mean((actual-fc)^2)),MAE=mean(abs(actual-fc)),CRPS=mean(vapply(seq_len(h),function(j)fabletools::CRPS(distributional::dist_sample(list(samples[j,])),actual[j]),numeric(1))),min=min(samples),max=max(samples))
 all_samples[[as.character(seed)]]<-forecasts
}
results<-do.call(rbind,rows);print(results)
saveRDS(list(results=results,forecasts=all_samples,synthetic=list(native=rowMeans(fsyn),correct=ar2correct),frozen=frozen,zero_native=zero_native),"holdout_corrected_diagnostic.rds")
write.csv(results,"holdout_corrected_seeds.csv",row.names=FALSE)
cat("BENCHMARK REPRODUCTION\n")
base<-as_tibble(rbind(d$train,d$test))|>mutate(date=d$dates)|>as_tsibble(index=date)
train<-head(base,m)
fit<-train|>model(ar=ARIMA(cpi_core~pdq(d=1,q=0)+PDQ(D=0,Q=0)),naive=NAIVE(cpi_core),drift=RW(cpi_core~drift()))
print(fit|>fabletools::forecast(h=h)|>accuracy(base,measures=list(RMSE=RMSE,MAE=MAE,CRPS=CRPS)))
fitv<-train|>model(var=VAR(vars(atividade,cpi_core,juro_curto)))
fv<-fabletools::forecast(fitv,h=h)
pr<-distributional::parameters(fv$.distribution)
fvc<-tibble(.model="var",date=tail(d$dates,h),cpi_core=distributional::dist_normal(purrr::map_dbl(pr$mu,2),sqrt(purrr::map_dbl(pr$sigma,~.x[2,2]))))|>as_tsibble(key=.model,index=date)|>as_fable(response="cpi_core",distribution="cpi_core")
print(fvc|>accuracy(base,measures=list(RMSE=RMSE,MAE=MAE,CRPS=CRPS)))
cat("DONE\n")
