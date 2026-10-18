fix <- "C:/Users/dudup/OneDrive/japan-econometrics-model/software/bsvarSIGNs-3.0-forecast-fix"
.libPaths(c(file.path(fix,"library"),"C:/Users/dudup/AppData/Local/R/win-library/4.6",.libPaths()))
suppressPackageStartupMessages(library(bsvarSIGNs))
a <- readRDS(file.path(fix,"posterior_original.rds")); b <- readRDS(file.path(fix,"posterior_fixed.rds"))
comparison <- all.equal(a,b,tolerance=1e-10)
stopifnot(isTRUE(comparison))
set.seed(991); y <- matrix(rnorm(200),100,2); y[99:100,] <- 1
spec <- specify_bsvarSIGN$new(y,p=2,hyper_mu=FALSE,hyper_delta=FALSE,hyper_lambda=FALSE,hyper_psi=FALSE,sign_structural=matrix(NA_real_,2,2))
post <- bsvars::estimate(spec,S=10L,show_progress=FALSE)
post$posterior$A[] <- 0
for (s in 1:10) {
 post$posterior$A[,1:2,s] <- diag(1.5,2)
 post$posterior$A[,3:4,s] <- diag(-.6,2)
 post$posterior$Sigma[,,s] <- diag(1e-24,2)
}
f <- bsvarSIGNs::forecast(post,24)
reference <- as.numeric(stats::filter(rep(0,24),c(1.5,-.6),method="recursive",init=c(1,1)))
result <- data.frame(h=1:24,patched=f$forecast_mean[1,,1],reference=reference)
stopifnot(max(abs(result$patched-result$reference))<1e-8)
write.csv(result,file.path(fix,"AR2_corrigido.csv"),row.names=FALSE)
writeLines(c("Estimation equal within 1e-10: TRUE",paste("AR2 H24:",result$patched[24]),paste("Max AR2 error:",max(abs(result$patched-result$reference)))),file.path(fix,"verification.txt"))
print(tail(result,1)); print(comparison)