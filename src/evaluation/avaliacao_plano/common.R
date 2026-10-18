source("C:/Users/dudup/OneDrive/japan-econometrics-model/src/evaluation/comparacao_cv_3_6/common.R")
alvos <- vars[5:10]
# O log score e -log densidade: menor e melhor.
# Mesma KDE gaussiana/bw.nrd do scoringRules, calculada em log para preservar caudas.
logscore_sample_stable <- function(x,y) {
 bw <- stats::bw.nrd(x)
 if(!is.finite(bw) || bw<=0) stop("Amostra preditiva degenerada: bandwidth invalido")
 z <- dnorm(y,mean=x,sd=bw,log=TRUE); m<-max(z)
 -(m+log(mean(exp(z-m))))
}
score_full_draws <- function(F,train,test,fold,model,quality="ok") {
 sc <- score_draws(F,train,test,fold,model,quality)
 map_dfr(alvos,function(v){
  a<-matrix(F[match(v,vars),seq_len(nrow(test)),,drop=FALSE],nrow=nrow(test))
  sc |> filter(variable==v) |> mutate(origin_value=tail(train[[v]],1),
    predictive_sd=apply(a,1,sd),log_score=vapply(seq_len(nrow(test)),function(j)logscore_sample_stable(a[j,],test[[v]][j]),numeric(1)))
 })
}
score_full_normal <- function(model,fold,variable,train,test,mu,sd){
 score_normal(model,fold,variable,max(train$date),test$date,test[[variable]],mu,sd) |>
 mutate(origin_value=tail(train[[.env$variable]],1),predictive_sd=.env$sd,log_score=-dnorm(test[[.env$variable]],.env$mu,.env$sd,log=TRUE))
}
# Verificar o calculo estavel contra o pacote em um caso sem subfluxo numerico.
set.seed(721); .check<-rnorm(2500)
stopifnot(abs(logscore_sample_stable(.check,.3)-scoringRules::logs_sample(.3,.check))<1e-8,
 is.finite(logscore_sample_stable(.check,100)))
rm(.check)
# Protocolo atual: funcoes nativas dos pacotes, conforme solicitado.
# Os helpers historicos carregados acima ficam apenas para reproducao/diagnostico.
crps_emp <- function(x,y) fabletools::CRPS(distributional::dist_sample(list(x)),.actual=y)
crps_norm <- function(mu,sd,y) vapply(seq_along(y),function(j)
 fabletools::CRPS(distributional::dist_normal(mu[j],sd[j]),.actual=y[j]),numeric(1))
check_package_protocol <- function(cfg){
 if(!identical(cfg$forecast_engine,"package") || !identical(cfg$crps_engine,"fabletools"))
  stop("Esta pasta usa o protocolo anterior. Prepare uma pasta nova para funcoes nativas; nao misture checkpoints.")
}
