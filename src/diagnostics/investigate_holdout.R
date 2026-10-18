.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
suppressPackageStartupMessages(library(fpp3))
suppressPackageStartupMessages(library(bsvarSIGNs))
library(tsibble)
library(bsvars)
library(fabletools)
library(distributional)

setwd("C:/Users/dudup/OneDrive/NT Macro/work/psi_diagnostic")
options(width = 130)
cat("VERSIONS\n"); print(sapply(c("bsvarSIGNs","bsvars","fabletools","distributional"), function(x) as.character(packageVersion(x))))
cat("FORECAST METHOD\n"); print(getS3method("forecast", "PosteriorBSVARSIGN"))
cat("PRIOR INITIALIZER\n"); print(specify_prior_bsvarSIGN$public_methods$initialize)
cat("ESTIMATE METHOD\n"); print(getS3method("estimate", "BSVARSIGN"))
d <- readRDS("data_bundle.rds")
y <- cbind(d$matrix_externa,d$matrix_interna)
dates <- yearmonth(d$dates)
cat("DATA\n"); print(dim(y));print(colnames(y)); print(range(dates)); print(colSums(!is.finite(y)))
h <- 24L; p <- 12L; n <- ncol(y); m <- nrow(y)-h
train <- y[seq_len(m),,drop=FALSE]; test <- y[m+seq_len(h),,drop=FALSE]
cat("HOLDOUT\n"); print(c(train_start=as.character(dates[1]),train_end=as.character(dates[m]),test_start=as.character(dates[m+1]),test_end=as.character(tail(dates,1))))
cat("RANGES\n"); print(rbind(min=apply(train,2,min),max=apply(train,2,max),sd=apply(train,2,sd)))
spec <- specify_bsvarSIGN$new(data=train,p=p)
cat("HYPER\n"); print(spec$prior$hyper[,1]);cat("PRIOR A FIRST DIAGONAL\n"); print(diag(spec$prior$A[,seq_len(n)]))
cat("BASELINE START\n"); flush.console()
set.seed(123)
timing <- system.time(post <- estimate(spec,S=200,show_progress=FALSE))
saveRDS(post,"holdout_original_post.rds")
cat("BASELINE POST SAVED\n");print(timing);flush.console()
fore <- forecast(post,horizon=h)
saveRDS(fore,"holdout_original_fore.rds")
saveRDS(list(train=train,test=test,dates=dates,m=m,h=h,p=p),"holdout_diagnostic_data.rds")
samples <- fore$forecasts[match("cpi_core",colnames(train)),,]
actual <- test[,"cpi_core"]
point <- rowMeans(samples)
metrics <- c(RMSE=sqrt(mean((actual-point)^2)),MAE=mean(abs(actual-point)),CRPS=mean(scoringRules_available<-vapply(seq_len(h), function(j) CRPS(dist_sample(list(samples[j,])),actual[j]), numeric(1))))
cat("BASELINE METRICS\n"); print(metrics)
cat("SAMPLE RANGES\n"); print(range(samples)); print(quantile(samples,c(0,.01,.5,.99,1)))
monthly <- data.frame(date=as.character(dates[m+seq_len(h)]), actual=actual, mean=point,median=apply(samples,1,median),q05=apply(samples,1,quantile,.05),q95=apply(samples,1,quantile,.95),min=apply(samples,1,min),max=apply(samples,1,max))
print(monthly);write.csv(monthly,"holdout_original_monthly.csv",row.names=FALSE)
cat("DATA COEFFICIENT DIMENSIONS\n"); print(lapply(post$posterior,dim));print(dim(spec$data_matrices$X))
cat("STATE VS LAST DATA\n"); print(tail(spec$data_matrices$X[,ncol(spec$data_matrices$X)],12));print(tail(train,2))
cat("ROOTS\n");flush.console()
roots <- vapply(seq_len(dim(post$posterior$A)[3]),function(s) {
 A<-post$posterior$A[,,s]
 F<-rbind(A[,seq_len(n*p)],cbind(diag(n*(p-1)),matrix(0,n*(p-1),n)))
 max(Mod(eigen(F,only.values=TRUE)$values))
},numeric(1))
cat("ROOT DISTRIBUTION\n");print(quantile(roots,c(0,.25,.5,.75,.9,.95,.99,1)));cat("UNSTABLE FRACTION",mean(roots>1),"\n")
path_error <- sqrt(colMeans((samples-actual)^2))
worst <- order(path_error,decreasing=TRUE)[seq_len(10)]
print(data.frame(draw=worst,root=roots[worst],rmse=path_error[worst],final=samples[h,worst]))
conditional <- matrix(NA_real_,h,length(roots))
x0 <- c(t(train[m:(m-p+1),]),1)
for(s in seq_along(roots)) {
 x<-x0; A<-post$posterior$A[,,s]
 for(j in seq_len(h)) {
  pred <- drop(A%*%x);conditional[j,s]<-pred[6]
  x<-c(pred,x[seq_len(n*(p-1))],1)
 }
}
cat("CONDITIONAL FORECAST METRICS\n");print(c(RMSE=sqrt(mean((actual-rowMeans(conditional))^2)),MAE=mean(abs(actual-rowMeans(conditional)))))
cat("FIRST HORIZON NATIVE VS CONDITIONAL\n");print(c(native=mean(samples[1,]),conditional=mean(conditional[1,]),actual=actual[1]))
saveRDS(list(roots=roots,samples=samples,conditional=conditional,metrics=metrics,monthly=monthly,timing=timing),"holdout_original_diagnostic.rds")
cat("DONE\n")
