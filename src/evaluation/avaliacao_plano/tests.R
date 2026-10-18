# Comparacoes pareadas por alvo/horizonte. Origens separadas por tres meses.
hac_lag <- function(n,h) min(n-2L,max(ceiling(h/3)-1L,floor(4*(n/100)^(2/9))))
dm_loss_hac <- function(loss_model,loss_benchmark,h,lag_override=NULL){
 d<-loss_model-loss_benchmark;n<-length(d);lag<-if(is.null(lag_override))hac_lag(n,h) else lag_override
 ans<-tibble(n=n,mean_loss_difference=mean(d),hac_lag=lag,statistic=NA_real_,p_value=NA_real_,status="ok")
 if(n<10 || any(!is.finite(d)) || sd(d)<1e-14){ans$status<-"insufficient_or_degenerate";return(ans)}
 se<-sqrt(as.numeric(sandwich::NeweyWest(lm(d~1),lag=lag,prewhite=FALSE,adjust=FALSE)))
 heff<-ceiling(h/3);correction<-sqrt((n+1-2*heff+heff*(heff-1)/n)/n)
 if(!is.finite(se) || se<=0 || !is.finite(correction)){ans$status<-"invalid_variance";return(ans)}
 ans$statistic<-correction*mean(d)/se;ans$p_value<-2*pt(-abs(ans$statistic),df=n-1);ans
}
direction_test <- function(actual_change,forecast_change,h){
 sy<-ifelse(abs(actual_change)<1e-10,0,sign(actual_change));sf<-ifelse(abs(forecast_change)<1e-10,0,sign(forecast_change))
 ok<-is.finite(sy)&is.finite(sf);sy<-sy[ok];sf<-sf[ok]
 ans<-tibble(n_all=length(sy),directional_accuracy=mean(sy==sf),actual_zero=sum(sy==0),forecast_zero=sum(sf==0),
  n_pt=sum(sy!=0&sf!=0),PT_statistic=NA_real_,PT_p_value=NA_real_,PT_HAC_statistic=NA_real_,PT_HAC_p_value=NA_real_,status="ok")
 keep<-sy!=0&sf!=0;a<-as.numeric(sy[keep]>0);f<-as.numeric(sf[keep]>0);n<-length(a)
 if(n<10 || length(unique(a))<2 || length(unique(f))<2){ans$status<-"insufficient_or_degenerate_directions";return(ans)}
 pa<-mean(a);pf<-mean(f);hit<-as.numeric(a==f);ph<-mean(hit);p0<-pa*pf+(1-pa)*(1-pf)
 v0<-p0*(1-p0)/n
 vs<-((2*pf-1)^2*pa*(1-pa)+(2*pa-1)^2*pf*(1-pf))/n+4*pa*pf*(1-pa)*(1-pf)/n^2
 if(v0>vs){ans$PT_statistic<-(ph-p0)/sqrt(v0-vs);ans$PT_p_value<-pnorm(ans$PT_statistic,lower.tail=FALSE)}
 # Robustez delta/HAC do excesso de acerto sobre a independencia das frequencias marginais.
 # Manter separada do PT classico; nao apresentar como o teste dinamico PT2009.
 influence<-hit-ph-(2*pf-1)*(a-pa)-(2*pa-1)*(f-pf)
 se<-sqrt(as.numeric(sandwich::NeweyWest(lm(influence~1),lag=hac_lag(n,h),prewhite=FALSE,adjust=FALSE)))
 if(is.finite(se)&&se>0){ans$PT_HAC_statistic<-(ph-p0)/se;ans$PT_HAC_p_value<-pnorm(ans$PT_HAC_statistic,lower.tail=FALSE)}
 ans
}
# Conferir a implementacao generica de DM com a funcao publicada do forecast no caso h1/lag0.
set.seed(811);.e1<-rnorm(100);.e2<-rnorm(100,sd=1.2)
.a<-dm_loss_hac(.e1^2,.e2^2,h=1,lag_override=0);.b<-forecast::dm.test(.e1,.e2,h=1,power=2,varestimator="bartlett")
stopifnot(abs(.a$statistic-as.numeric(.b$statistic))<1e-8,abs(.a$p_value-.b$p.value)<1e-8,
 is.na(direction_test(seq(-10,10),rep(0,21),1)$PT_p_value))
rm(.e1,.e2,.a,.b)