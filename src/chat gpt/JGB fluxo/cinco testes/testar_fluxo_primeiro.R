# ALVO DO PRIMEIRO ESTAGIO = FLUXO ESTRANGEIRO. Depois projetar o spread com fluxo PREVISTO.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco testes/00_configuracao.R',encoding='UTF-8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(fpp3)); suppressPackageStartupMessages(library(tidyverse))
source(file.path(pasta_testes,'01_dados.R'),encoding='UTF-8')
source(file.path(pasta_testes,'09_selecao_satelite.R'),encoding='UTF-8')
destino <- file.path(pasta_testes,'resultados','fluxo primeiro depois spread')
dir.create(destino,recursive=TRUE,showWarnings=FALSE)
d <- ler_insumos()
boj <- jsonlite::fromJSON(file.path(fonte_dados,'BOJ_STRACLUCON_2026-11-11.json'))
curto <- tibble(date=yearmonth(ym(boj$RESULTSET$VALUES$SURVEY_DATES[[1]])),
  call_jp=as.numeric(boj$RESULTSET$VALUES$VALUES[[1]]))
fed <- read_csv(file.path(fonte_dados,'FEDFUNDS_FRED_2026-11-11.csv'),show_col_types=FALSE) |>
  transmute(date=yearmonth(observation_date),fed=FEDFUNDS)
banco <- d$historico |> as_tibble() |> arrange(date) |>
  transmute(date,spread=100*inclinacao,d_spread=100*(inclinacao-lag(inclinacao)),
    cambio=usdjpy-lag(usdjpy),inflacao=100*(exp((cpi_core-lag(cpi_core,12))/100)-1)) |>
  left_join(d$aux |> select(date,fluxo,ois_pos,fluxo_l2,ust_l2),by='date') |>
  left_join(curto,by='date') |> left_join(fed,by='date') |>
  mutate(diferencial=fed-call_jp) |> preparar_lags_satelite(c('diferencial','cambio','inflacao'))
for(v in c('fluxo','d_spread')) for(j in 1:3)
  banco[[paste0(v,'_l',j)]] <- lag(banco[[v]],j)
cols <- c('spread','d_spread','fluxo','ois_pos','fluxo_l2','ust_l2',
  unlist(lapply(c('diferencial','cambio','inflacao'),function(x) paste0(x,'_l',0:12))),
  paste0('fluxo_l',1:3),paste0('d_spread_l',1:3))
banco <- banco |> filter(date>=yearmonth(INICIO),date<=d$origem,
  if_all(all_of(cols),is.finite)) |> as_tsibble(index=date)
combos <- c(list(base=character()),set_names(unlist(lapply(1:3,function(k)
  combn(c('diferencial','cambio','inflacao'),k,simplify=FALSE)),recursive=FALSE),paste0('C',1:7)))
write_csv(imap_dfr(combos,~tibble(id=.y,variaveis=paste(.x,collapse=' + '))),
  file.path(destino,'combinacoes.csv'))
write_csv(as_tibble(banco) |> mutate(date=as.Date(date)),file.path(destino,'base auditada.csv'))

fit_fluxo <- function(dados,termos) {
  f <- as.formula(paste('fluxo ~',if(length(termos)) paste(termos,collapse=' + ') else '1',
    '+ pdq(p=0:3,d=0,q=0:3) + PDQ(0,0,0)'))
  dados |> model(arima=ARIMA(!!f,stepwise=FALSE,approximation=FALSE))
}
escolher_lag_fluxo <- function(x,dados) {
  tab <- map_dfr(names(specs_satelite),function(s) {
    m <- fit_fluxo(dados,paste0(x,'_l',specs_satelite[[s]]))
    tibble(spec=s,AICc=glance(m)$AICc,n=length(specs_satelite[[s]]),
      menor=min(specs_satelite[[s]]))
  }) |> filter(is.finite(AICc)) |> mutate(delta=AICc-min(AICc)) |>
    filter(delta<2) |> arrange(n,desc(menor),AICc) |> slice_head(n=1)
  stopifnot(nrow(tab)==1L)
  paste0(x,'_l',specs_satelite[[tab$spec]])
}
fit_arima_fluxo <- function(comb,dados,escolhas) {
  termos <- unlist(escolhas[comb],use.names=FALSE)
  m <- fit_fluxo(dados,termos)
  teste <- tidy(m) |> filter(term %in% termos) |> select(term,estimate,p.value)
  list(modelo=m,termos=termos,teste=teste,
    aprovado=nrow(teste)==length(termos) && all(is.finite(teste$p.value) & teste$p.value<=.1))
}
fit_var_fluxo <- function(comb,dados) {
  ex <- if(length(comb)) paste0(comb,'_l0') else character()
  candidatos <- lapply(1:3,function(p) {
    rhs <- c(ex,paste0('fluxo_l',1:p),paste0('d_spread_l',1:p))
    m <- lm(as.formula(paste('cbind(fluxo,d_spread) ~',paste(rhs,collapse=' + '))),data=dados)
    E <- residuals(m); n <- nrow(E); k <- length(coef(m))+3L
    ll <- -n*(2*(log(2*pi)+1)+as.numeric(determinant(crossprod(E)/n,logarithm=TRUE)$modulus))/2
    list(modelo=m,p=p,rhs=rhs,AICc=-2*ll+2*k+2*k*(k+1)/(n-k-1))
  })
  a <- candidatos[[which.min(vapply(candidatos,`[[`,numeric(1),'AICc'))]]
  co <- summary(a$modelo)[[1]]$coefficients # PRIMEIRA equacao = fluxo.
  teste <- tibble(term=ex,estimate=if(length(ex)) unname(co[ex,1]) else numeric(),
    p.value=if(length(ex)) unname(co[ex,4]) else numeric())
  m <- lm(as.formula(paste('fluxo ~',paste(a$rhs,collapse=' + '))),data=dados)
  ix <- paste0('d_spread_l',1:a$p); b <- coef(m)[ix]; V <- vcov(m)[ix,ix,drop=FALSE]
  teste <- bind_rows(teste,tibble(term='spread: bloco de lags',estimate=NA_real_,
    p.value=pf(as.numeric(t(b)%*%solve(V,b))/length(ix),length(ix),df.residual(m),lower.tail=FALSE)))
  a$teste <- teste
  a$aprovado <- all(is.finite(teste$p.value) & teste$p.value<=.1)
  a
}
prever_var_fluxo <- function(a,treino,janela) {
  hist <- treino |> as_tibble() |> select(fluxo,d_spread) |> as.matrix()
  out <- matrix(NA_real_,nrow(janela),2)
  for(t in seq_len(nrow(janela))) {
    linha <- as.list(janela[t,])
    for(j in 1:a$p) {
      linha[[paste0('fluxo_l',j)]] <- unname(hist[nrow(hist)-j+1L,1])
      linha[[paste0('d_spread_l',j)]] <- unname(hist[nrow(hist)-j+1L,2])
    }
    X <- c('(Intercept)'=1,unlist(linha[a$rhs]))
    out[t,] <- drop(X[rownames(coef(a$modelo))]%*%coef(a$modelo))
    hist <- rbind(hist,out[t,])
  }
  stopifnot(all(is.finite(out))); out
}
cl <- parallel::makePSOCKcluster(3L)
parallel::clusterEvalQ(cl,{
  .libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
  suppressPackageStartupMessages(library(fpp3)); suppressPackageStartupMessages(library(tidyverse)); NULL
})
parallel::clusterExport(cl,c('specs_satelite','fit_fluxo','escolher_lag_fluxo','fit_arima_fluxo'))
ajustar_janela_fluxo <- function(dados) {
  xs <- c('diferencial','cambio','inflacao')
  escolhas <- set_names(parallel::parLapply(cl,xs,escolher_lag_fluxo,dados=dados),xs)
  ar <- parallel::parLapply(cl,combos,fit_arima_fluxo,dados=dados,escolhas=escolhas)
  va <- lapply(combos,fit_var_fluxo,dados=dados)
  ref <- dados |> model(arima=ARIMA(fluxo~0+ust_l2+pdq(0,0,2)+PDQ(0,0,0)))
  teste <- tidy(ref) |> filter(term=='ust_l2') |> select(term,estimate,p.value)
  c(set_names(ar,paste0('ARIMA_',names(combos))),set_names(va,paste0('VAR_',names(combos))),
    list(Referencia_UST30=list(modelo=ref,termos='ust_l2',teste=teste,
      aprovado=all(teste$p.value<=.1))))
}
chave <- list(banco=as_tibble(banco),codigo=tools::md5sum(file.path(pasta_testes,'testar_fluxo_primeiro.R')))
cache <- file.path(destino,'ajustes completos.rds')
salvo <- if(file.exists(cache)) readRDS(cache) else NULL
message('ALVO = FLUXO. Amostra: ',min(banco$date),' a ',max(banco$date),'; n=',nrow(banco))
modelos <- if(!is.null(salvo) && identical(salvo$chave,chave)) salvo$modelos else ajustar_janela_fluxo(banco)
saveRDS(list(chave=chave,modelos=modelos),cache)
triagem <- imap_dfr(modelos,function(a,id) tibble(modelo=id,aprovado=a$aprovado,
  reprovadas=paste(a$teste$term[!is.finite(a$teste$p.value) | a$teste$p.value>.1],collapse='; ')))
write_csv(triagem,file.path(destino,'triagem FLUXO.csv'))
write_csv(imap_dfr(modelos,~mutate(.x$teste,modelo=.y)),file.path(destino,'p valores FLUXO.csv'))
print(triagem,n=Inf)

diag <- list()
grDevices::pdf(tempfile(fileext='.pdf'))
for(id in names(modelos)) {
  a <- modelos[[id]]; pasta <- file.path(destino,'todos modelos',id)
  dir.create(pasta,recursive=TRUE,showWarnings=FALSE)
  if(!startsWith(id,'VAR')) {
    diag[[id]] <- diagnosticar_satelite(a$modelo) |> mutate(modelo=id,equacao='fluxo')
    g <- grafico_residuos_satelite(a$modelo,'fluxo')
    g[[1]] <- g[[1]]+labs(title=paste(id,'| ALVO: fluxo estrangeiro'))+
      theme(plot.title=element_text(size=11),plot.subtitle=element_text(size=9))
    ggsave(file.path(pasta,'residuos FLUXO.png'),g,width=12,height=8,dpi=140,bg='white')
  } else for(j in 1:2) {
    eq <- c('fluxo','spread')[j]; e <- residuals(a$modelo)[,j]
    q <- tibble(date=banco$date,erro=e) |> as_tsibble(index=date)
    g <- ggtime::gg_tsdisplay(q,erro,plot_type='histogram',lag_max=24)
    g[[1]] <- g[[1]]+labs(title=paste(id,'|',eq))
    ggsave(file.path(pasta,paste0('residuos ',eq,'.png')),g,width=12,height=8,dpi=140,bg='white')
    diag[[paste(id,eq)]] <- map_dfr(c(12,24),function(k) tibble(lag_teste=k,
      p_ljung_box=Box.test(e,lag=k,type='Ljung-Box')$p.value,modelo=id,equacao=eq))
  }
}
invisible(dev.off())
write_csv(bind_rows(diag),file.path(destino,'diagnosticos residuos.csv'))
origens <- seq(yearmonth('2024 Oct'),max(banco$date)-12L,by=1L)
cv <- list()
for(i in seq_along(origens)) {
  o <- origens[i]; t <- banco |> filter(date<=o); j <- banco |> filter(date>o,date<=o+12L)
  stopifnot(nrow(j)==12L)
  a <- ajustar_janela_fluxo(t)
  # SEGUNDO estagio separado. Estimado com fluxo historico; futuro so com fluxo PREVISTO.
  sat <- lm(d_spread~ois_pos+fluxo_l2,data=t)
  produzir_linha <- function(id,f,direto=rep(NA_real_,12),sig=NA) {
    stopifnot(length(f)==12L,all(is.finite(f)))
    lags <- head(c(tail(t$fluxo,2),f),12)
    delta <- predict(sat,newdata=tibble(ois_pos=j$ois_pos,fluxo_l2=lags))
    tibble(origem=as.Date(o),date=as.Date(j$date),h=1:12,modelo=id,
      fluxo_previsto=f,fluxo_realizado=j$fluxo,
      spread_satelite=tail(t$spread,1)+cumsum(delta),spread_realizado=j$spread,
      spread_var_direto=if(all(is.finite(direto))) tail(t$spread,1)+cumsum(direto) else NA_real_,
      significativo_no_treino=sig)
  }
  for(id in names(a)) {
    m <- a[[id]]
    if(startsWith(id,'VAR')) {
      previsao <- prever_var_fluxo(m,t,j)
      cv[[paste(i,id)]] <- produzir_linha(id,previsao[,1],previsao[,2],m$aprovado)
    } else {
      # Nenhum fluxo futuro realizado e enviado para o forecast ARIMA.
      novos <- j |> as_tibble() |> select(date,all_of(m$termos)) |> as_tsibble(index=date)
      f <- forecast(m$modelo,new_data=novos)$.mean
      cv[[paste(i,id)]] <- produzir_linha(id,f,sig=m$aprovado)
    }
  }
  for(nome in c('Naive','Drift','Media')) {
    f <- switch(nome,Naive=rep(tail(t$fluxo,1),12),
      Drift=tail(t$fluxo,1)+(1:12)*mean(diff(t$fluxo)),Media=rep(mean(t$fluxo),12))
    cv[[paste(i,nome)]] <- produzir_linha(nome,f)
  }
  write_csv(bind_rows(cv),file.path(destino,'CV parcial.csv'))
  message('CV FLUXO -> SPREAD 12m: ',i,'/',length(origens),' | ',format(o))
}
parallel::stopCluster(cl)
previsoes <- bind_rows(cv)
metricas <- previsoes |> group_by(modelo) |> summarise(origens=n_distinct(origem),
  rmse_fluxo_12m=sqrt(mean((fluxo_previsto[h==12]-fluxo_realizado[h==12])^2)),
  rmse_fluxo_meses_1a12=sqrt(mean((fluxo_previsto-fluxo_realizado)^2)),
  rmse_spread_satelite_12m=sqrt(mean((spread_satelite[h==12]-spread_realizado[h==12])^2)),
  rmse_spread_var_direto_12m=if(all(is.finite(spread_var_direto)))
    sqrt(mean((spread_var_direto[h==12]-spread_realizado[h==12])^2)) else NA_real_,
  fracao_treinos_significativos=mean(significativo_no_treino),.groups='drop') |>
  left_join(triagem,by='modelo') |> arrange(rmse_fluxo_12m)
write_csv(previsoes,file.path(destino,'CV previsoes.csv'))
write_csv(metricas,file.path(destino,'CV todos.csv'))
write_csv(metricas |> filter(aprovado %in% TRUE | modelo %in% c('Naive','Drift','Media')),
  file.path(destino,'CV significativos.csv'))
writeLines(c('ALVO PRINCIPAL = FLUXO ESTRANGEIRO; corte de significancia aplicado a sua equacao.',
  'ARIMA: fluxo ~ combinacoes de diferencial curto, variacao cambial e inflacao; ARMA 0:3, d=0.',
  'd=0 mantido conforme comparacao anterior de transformacoes; fluxo sem log por conter negativos.',
  'VAR: fluxo e Delta spread juntos, macros exogenas realizadas; p=1:3 por AICc.',
  'VAR: corte nos regressores macro e bloco de defasagens do spread NA EQUACAO DO FLUXO; proprias defasagens sao termos AR.',
  'Ex post APENAS para diferencial/cambio/inflacao e OIS. Fluxo futuro e previsto em TODOS os modelos.',
  'Estagio 2: Delta spread ~ constante + OIS pos-YCC + fluxo lag 2, com coeficientes reestimados em cada origem.',
  'Os lags do fluxo futuro no estagio 2 usam previsoes; somente os dois lags iniciais sao historicos.',
  'CV em 12 meses; 11 origens; reescolha de lags/ARMA/VAR por origem. Sem excluir origens por p alto.',
  'Corte de 10% nominal apos selecao e exploratorio; multiplas comparacoes. Nao garante diagnosticos adequados.',
  'Resultados anteriores de combinacoes do spread nao sao resultados da projecao do fluxo. Nenhum BVAR modificado.'),
  file.path(destino,'LEIA-ME.txt'))
print(metricas,n=Inf,width=Inf)
