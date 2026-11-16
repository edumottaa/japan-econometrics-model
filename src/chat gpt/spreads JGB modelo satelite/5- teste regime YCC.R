Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/resultados'
selecao <- readRDS(file.path(trabalho,'selecao.rds'))
modelos <- readRDS(file.path(trabalho,'modelos ARIMA.rds'))
K <- selecao$K; H <- 12L; rho <- 0.98
termos <- function(x,lags) paste(ifelse(lags==0,x,paste0('lag(',x,',',lags,')')),collapse=' + ')
# Ate mar/2024 o BoJ controlava o 10Y (YCC). Separar os betas de UST30 e OIS2Y por regime.
dados <- readRDS(file.path(trabalho,'dados.rds'))$dados |>
  mutate(d_ust30_pre=d_ust30*(1-pos_ycc),d_ust30_pos=d_ust30*pos_ycc,
    d_ois2_pre=d_ois2*(1-pos_ycc),d_ois2_pos=d_ois2*pos_ycc)

# 1. Especificacoes com as defasagens e ordens ARIMA do modelo final.
especificar <- function(alvo,dividir) {
  e <- selecao$escolhas |> filter(alvo==!!alvo)
  x <- map2_chr(e$regressor,e$spec,function(r,s) if(dividir && r %in% c('d_ust30','d_ois2'))
    paste(termos(paste0(r,'_pre'),selecao$specs[[s]]),'+',termos(paste0(r,'_pos'),selecao$specs[[s]])) else
    termos(r,selecao$specs[[s]]))
  t <- tidy(modelos[[alvo]])$term
  list(rhs=paste(x,collapse=' + '),
    erro=paste0(' + pdq(',sum(str_detect(t,'^ar[0-9]+$')),',0,',sum(str_detect(t,'^ma[0-9]+$')),') + PDQ(0,0,0) + 1'))
}
ajustar <- function(d,alvo,dividir) {
  s <- especificar(alvo,dividir)
  d |> mutate(!!alvo:=if_else(row_number()<=K,NA_real_,.data[[alvo]])) |>
    model(arima=ARIMA(!!as.formula(paste(alvo,'~',s$rhs,s$erro))))
}
# Peso exponencial (mesmo criterio do 01_jgb2.R): MQO ponderado, sem erros ARIMA.
ajustar_ponderada <- function(d,alvo) {
  s <- especificar(alvo,FALSE)
  d <- as_tibble(d) |> mutate(!!alvo:=if_else(row_number()<=K,NA_real_,.data[[alvo]]),
    peso=rho^rev(seq_len(n())-1L))
  lm(as.formula(paste(alvo,'~',s$rhs)),data=d,weights=peso)
}

# 2. Coeficientes por regime e teste de razao de verossimilhanca (2 restricoes).
resultado <- map_dfr(names(modelos),function(alvo) {
  completo <- ajustar(dados,alvo,FALSE); dividido <- ajustar(dados,alvo,TRUE)
  pos <- ajustar(dados |> filter(pos_ycc==1),alvo,FALSE)
  pond <- ajustar_ponderada(dados,alvo)
  hac <- sqrt(diag(sandwich::NeweyWest(pond)))
  lr <- 2*(glance(dividido)$log_lik-glance(completo)$log_lik)
  bind_rows(tidy(completo) |> mutate(modelo='Amostra completa'),
    tidy(dividido) |> mutate(modelo='Betas por regime (jun/18-set/26)'),
    tidy(pos) |> mutate(modelo='So pos-YCC (abr/24-set/26)'),
    tibble(term=names(coef(pond)),estimate=coef(pond),std.error=hac[names(coef(pond))],
      statistic=estimate/std.error,p.value=2*pnorm(-abs(statistic)),
      modelo=paste0('MQO ponderado rho=',rho,' (EP Newey-West)'))) |>
    filter(str_detect(term,'ust30|ois2')) |>
    mutate(alvo=alvo,lr_estatistica=lr,lr_pvalor=pchisq(lr,2,lower.tail=FALSE),
      AICc_completo=glance(completo)$AICc,AICc_regime=glance(dividido)$AICc,.before=1)
})
print(resultado |> select(alvo,modelo,term,estimate,std.error,p.value),n=Inf)
print(resultado |> distinct(alvo,lr_estatistica,lr_pvalor,AICc_completo,AICc_regime))

# 3. CV ex post so em origens com pelo menos 6 meses pos-YCC no treino.
origens <- seq(yearmonth('2024 Sep'),max(dados$mes)-H,by=1)
cv <- map_dfr(names(modelos),function(alvo) map_dfr(origens,function(o) {
  treino <- dados |> filter(mes<=o); futuro <- dados |> filter(mes>o,mes<=o+H)
  pond <- ajustar_ponderada(treino,alvo)
  # Prever o MQO com o historico para as defasagens; so as linhas futuras sao usadas.
  x_pond <- predict(pond,newdata=as_tibble(dados) |> filter(mes<=o+H))
  real <- futuro[[alvo]]
  completo <- forecast(ajustar(treino,alvo,FALSE),new_data=futuro)$.mean
  regime <- forecast(ajustar(treino,alvo,TRUE),new_data=futuro)$.mean
  tibble(alvo=alvo,origem=o,h=seq_len(H),real=real,`Amostra completa`=completo,
    `Betas por regime`=regime,`MQO ponderado`=tail(x_pond,H),`Variacao zero`=0)
})) |> pivot_longer(-c(alvo,origem,h,real),names_to='modelo',values_to='previsto') |>
  group_by(alvo,modelo,origem) |> arrange(h,.by_group=TRUE) |>
  mutate(erro=cumsum(real)-cumsum(previsto)) |> ungroup()
metricas <- cv |> filter(h %in% c(1,3,6,12)) |> group_by(alvo,modelo,h) |>
  summarise(RMSE_pb=sqrt(mean(erro^2)),n_origens=n(),.groups='drop') |> arrange(alvo,h,RMSE_pb)
print(metricas,n=Inf)

# Armazenamento da saida em outputs.
write_csv(resultado,file.path(trabalho,'regime coeficientes.csv'))
write_csv(metricas,file.path(trabalho,'regime cv metricas.csv'))
cat('REGIME_CONCLUIDO\n')
