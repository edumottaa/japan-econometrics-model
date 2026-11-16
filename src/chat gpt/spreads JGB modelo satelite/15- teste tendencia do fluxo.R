Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/resultados'
dados <- readRDS(file.path(trabalho,'dados.rds'))$dados; K <- 3L

# A compra liquida estrangeira tem tendencia? Se sim, o beta do fluxo no spread pode so estar
# casando a tendencia do fluxo com a abertura do spread (regressao espuria).
# 1. Testes de raiz unitaria/estacionariedade do fluxo (amostra toda e desde 2022).
# 2. Estagio 2 (OIS x YCC, 14-) com o fluxo em formas diferentes:
#    nivel | nivel + tendencia linear | desvio da media movel de 12m | variacao | desvio da tendencia linear
dados <- dados |> mutate(t=row_number(),ycc=as.numeric(mes<=yearmonth('2024 Mar')),ois_x_ycc=d_ois2*ycc,
  liq_l2=lag(liq_estr,2),
  liq_mm12=slider::slide_dbl(liq_estr,mean,.before=11,.complete=TRUE),
  liq_desvio_mm12_l2=lag(liq_estr-liq_mm12,2),
  d_liq_l2=lag(difference(liq_estr),2))
tend <- lm(liq_estr ~ t,data=as_tibble(dados))
dados <- dados |> mutate(liq_destend_l2=lag(liq_estr-predict(tend,newdata=pick(t)),2))
cat('Tendencia linear do fluxo: ',round(coef(tend)[2]*12,3),'¥ tri por ano (p=',
  signif(summary(tend)$coefficients[2,4],2),')\n')
cat('Media do fluxo: 2018-2021',round(mean(dados$liq_estr[dados$mes<yearmonth('2022 Jan')],na.rm=TRUE),2),
  '| 2022-2023',round(mean(dados$liq_estr[dados$mes>=yearmonth('2022 Jan') & dados$mes<yearmonth('2024 Jan')]),2),
  '| 2024-2026',round(mean(dados$liq_estr[dados$mes>=yearmonth('2024 Jan')]),2),'\n\n')

testes <- map_dfr(list(`Completa`=yearmonth('2018 Jun'),`Desde 2022`=yearmonth('2022 Jan')),function(ini) {
  d <- dados |> filter(mes>=ini)
  bind_cols(d |> features(liq_estr,list(unitroot_kpss,unitroot_pp,unitroot_ndiffs)),
    tibble(kpss_tendencia_estat=urca::ur.kpss(na.omit(d$liq_estr),type='tau',lags='short')@teststat,
      kpss_tendencia_crit5=urca::ur.kpss(na.omit(d$liq_estr),type='tau',lags='short')@cval[1,'5pct'])) },.id='amostra')
print(testes |> mutate(across(where(is.double),~signif(.x,3))),width=Inf)

ajusta <- function(regs) dados |> mutate(d_s30_5=if_else(row_number()<=14L,NA_real_,d_s30_5)) |>
  model(a=ARIMA(!!as.formula(paste('d_s30_5 ~',paste(regs,collapse=' + '),'+ pdq(0,0,0) + PDQ(0,0,0) + 1'))))
# Mesma amostra em todos (primeiros 14 meses fora por causa da media movel de 12m + lag 2).
formas <- list(
  `Nivel (atual)`=c('d_ois2','ois_x_ycc','liq_l2'),
  `Nivel + tendencia linear`=c('d_ois2','ois_x_ycc','liq_l2','t'),
  `So tendencia linear (sem fluxo)`=c('d_ois2','ois_x_ycc','t'),
  `Desvio da tendencia linear`=c('d_ois2','ois_x_ycc','liq_destend_l2'),
  `Desvio da media movel 12m`=c('d_ois2','ois_x_ycc','liq_desvio_mm12_l2'),
  `Variacao do fluxo`=c('d_ois2','ois_x_ycc','d_liq_l2'),
  `Sem fluxo`=c('d_ois2','ois_x_ycc'))
res <- imap_dfr(formas,function(r,nome) { m <- ajusta(r); t <- tidy(m)
  foco <- t |> filter(str_detect(term,'liq|^t$'))
  tibble(forma=nome,AICc=glance(m)$AICc,
    coefs=paste(sprintf('%s=%.3g (p=%.2g)',foco$term,foco$estimate,foco$p.value),collapse='; '),
    ljung_box_p=augment(m) |> features(.innov,ljung_box,lag=12) |> pull(lb_pvalue)) }) |>
  mutate(ganho_vs_sem_fluxo=AICc[forma=='Sem fluxo']-AICc,.after=AICc)
cat('\n'); for(i in seq_len(nrow(res))) cat(sprintf('%-34s AICc %.1f | ganho vs sem fluxo %+.1f | LB p=%.2f\n   %s\n',
  res$forma[i],res$AICc[i],res$ganho_vs_sem_fluxo[i],res$ljung_box_p[i],res$coefs[i]))
write_csv(res,file.path(trabalho,'teste tendencia do fluxo.csv'))
