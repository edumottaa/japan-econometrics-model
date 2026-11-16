# ALVO UNICO: FLUXO ESTRANGEIRO. Nao estima nem projeta spread.
# Todas as explicativas realizadas no CV; intercepto obrigatorio e fora do corte p<=10%.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco testes/00_configuracao.R',encoding='UTF-8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(fpp3)); suppressPackageStartupMessages(library(tidyverse))
source(file.path(pasta_testes,'01_dados.R'),encoding='UTF-8')
source(file.path(pasta_testes,'09_selecao_satelite.R'),encoding='UTF-8')
destino <- file.path(pasta_testes,'resultados','comparacao SOMENTE FLUXO')
dir.create(destino,recursive=TRUE,showWarnings=FALSE)
d <- ler_insumos()
boj <- jsonlite::fromJSON(file.path(fonte_dados,'BOJ_STRACLUCON_2026-11-11.json'))
curto <- tibble(date=yearmonth(ym(boj$RESULTSET$VALUES$SURVEY_DATES[[1]])),
  call_jp=as.numeric(boj$RESULTSET$VALUES$VALUES[[1]]))
fed <- read_csv(file.path(fonte_dados,'FEDFUNDS_FRED_2026-11-11.csv'),show_col_types=FALSE) |>
  transmute(date=yearmonth(observation_date),fed=FEDFUNDS)
banco <- d$historico |> as_tibble() |> arrange(date) |>
  transmute(date,cambio=usdjpy-lag(usdjpy),
    inflacao=100*(exp((cpi_core-lag(cpi_core,12))/100)-1)) |>
  left_join(d$aux |> select(date,fluxo,ust30),by='date') |>
  left_join(curto,by='date') |> left_join(fed,by='date') |>
  mutate(diferencial=fed-call_jp) |>
  preparar_lags_satelite(c('ust30','diferencial','cambio','inflacao'))
for(v in c('fluxo','ust30','diferencial','cambio','inflacao')) for(j in 1:3)
  banco[[paste0(v,'_l',j)]] <- lag(banco[[v]],j)
cols <- c('fluxo',paste0('fluxo_l',1:3),
  unlist(lapply(c('ust30','diferencial','cambio','inflacao'),function(x) paste0(x,'_l',0:12))))
banco <- banco |> filter(date>=yearmonth(INICIO),date<=d$origem,
  if_all(all_of(cols),is.finite)) |> as_tsibble(index=date)
combos <- list(UST30='ust30',Dif='diferencial',UST_FX=c('ust30','cambio'),
  UST_Inflacao=c('ust30','inflacao'),Dif_FX=c('diferencial','cambio'),
  Dif_Inflacao=c('diferencial','inflacao'),UST_FX_Inflacao=c('ust30','cambio','inflacao'),
  Dif_FX_Inflacao=c('diferencial','cambio','inflacao'))
write_csv(as_tibble(banco) |> mutate(date=as.Date(date)),file.path(destino,'base FLUXO.csv'))
write_csv(imap_dfr(combos,~tibble(id=.y,variaveis=paste(.x,collapse=' + '))),
  file.path(destino,'configuracoes.csv'))
fit_fluxo <- function(dados,termos) {
  f <- as.formula(paste('fluxo ~ 1 +',paste(termos,collapse=' + '),
    '+ pdq(p=0:3,d=0,q=0:3) + PDQ(0,0,0)'))
  dados |> model(arima=ARIMA(!!f,stepwise=FALSE,approximation=FALSE))
}
escolher_lag <- function(x,dados) {
  tab <- map_dfr(names(specs_satelite),function(s) {
    m <- fit_fluxo(dados,paste0(x,'_l',specs_satelite[[s]]))
    tibble(spec=s,AICc=glance(m)$AICc,n=length(specs_satelite[[s]]),
      menor=min(specs_satelite[[s]]))
  }) |> filter(is.finite(AICc)) |> mutate(delta=AICc-min(AICc)) |>
    filter(delta<2) |> arrange(n,desc(menor),AICc) |> slice_head(n=1)
  stopifnot(nrow(tab)==1L)
  paste0(x,'_l',specs_satelite[[tab$spec]])
}
fit_arima <- function(comb,dados,escolhas) {
  termos <- unlist(escolhas[comb],use.names=FALSE)
  m <- fit_fluxo(dados,termos)
  teste <- tidy(m) |> filter(term %in% termos) |> select(term,estimate,p.value)
  list(modelo=m,termos=termos,teste=teste,
    aprovado=nrow(teste)==length(termos) && all(is.finite(teste$p.value) & teste$p.value<=.1))
}
fit_var <- function(comb,dados) {
  vars <- c('fluxo',comb); N <- length(vars)
  candidatos <- lapply(1:3,function(p) {
    rhs <- unlist(lapply(vars,function(v) paste0(v,'_l',1:p)))
    f <- as.formula(paste('cbind(',paste(vars,collapse=','),') ~ 1 +',paste(rhs,collapse=' + ')))
    m <- lm(f,data=dados); E <- residuals(m); n <- nrow(E)
    k <- length(coef(m))+N*(N+1)/2
    ll <- -n*(N*(log(2*pi)+1)+as.numeric(determinant(crossprod(E)/n,logarithm=TRUE)$modulus))/2
    list(modelo=m,vars=vars,p=p,rhs=rhs,AICc=if(n>k+1) -2*ll+2*k+2*k*(k+1)/(n-k-1) else Inf)
  })
  a <- candidatos[[which.min(vapply(candidatos,`[[`,numeric(1),'AICc'))]]
  stopifnot(is.finite(a$AICc))
  alvo <- lm(as.formula(paste('fluxo ~ 1 +',paste(a$rhs,collapse=' + '))),data=dados)
  a$teste <- map_dfr(comb,function(v) {
    ix <- paste0(v,'_l',1:a$p); b <- coef(alvo)[ix]; V <- vcov(alvo)[ix,ix,drop=FALSE]
    F <- as.numeric(t(b)%*%solve(V,b))/length(ix)
    tibble(term=paste0(v,': bloco de lags'),estimate=NA_real_,
      p.value=pf(F,length(ix),df.residual(alvo),lower.tail=FALSE))
  })
  a$aprovado <- all(is.finite(a$teste$p.value) & a$teste$p.value<=.1)
  a
}
prever_var_condicional <- function(a,treino,janela) {
  # VAR conjunto, mas as trajetorias das explicativas sao condicionadas aos realizados.
  # O fluxo futuro nunca e condicionado ao realizado.
  vars <- a$vars; N <- length(vars); p <- a$p; h <- nrow(janela)
  y <- treino |> as_tibble() |> select(all_of(vars)) |> as.matrix()
  B <- coef(a$modelo)
  A <- lapply(1:p,function(l) t(B[paste0(vars,'_l',l),,drop=FALSE]))
  hist <- y; medias <- matrix(NA_real_,h,N)
  for(t in 1:h) {
    medias[t,] <- B['(Intercept)',]+Reduce('+',lapply(1:p,function(l)
      drop(A[[l]]%*%hist[nrow(hist)-l+1L,])))
    hist <- rbind(hist,medias[t,])
  }
  phi <- list(diag(N))
  for(k in 1:(h-1)) phi[[k+1]] <- Reduce('+',lapply(1:min(k,p),
    function(l) A[[l]]%*%phi[[k-l+1]]))
  G <- matrix(0,h*N,h*N)
  for(t in 1:h) for(k in 1:t)
    G[(t-1)*N+1:N,(k-1)*N+1:N] <- phi[[t-k+1]]
  E <- residuals(a$modelo); Sigma <- crossprod(E)/nrow(E)
  V <- G%*%kronecker(diag(h),Sigma)%*%t(G)
  cond <- matrix(NA_real_,h,N)
  cond[,-1] <- janela |> as_tibble() |> select(all_of(vars[-1])) |> as.matrix()
  fixos <- as.vector(t(cond)); ix <- which(is.finite(fixos)); mu <- as.vector(t(medias))
  ganho <- V[,ix,drop=FALSE]%*%chol2inv(chol(V[ix,ix,drop=FALSE]))
  mu <- mu+drop(ganho%*%(fixos[ix]-mu[ix]))
  stopifnot(all(is.finite(mu)),max(abs(mu[ix]-fixos[ix]))<1e-6)
  matrix(mu,h,N,byrow=TRUE)[,1]
}
cl <- parallel::makePSOCKcluster(3L)
parallel::clusterEvalQ(cl,{
  .libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
  suppressPackageStartupMessages(library(fpp3)); suppressPackageStartupMessages(library(tidyverse)); NULL
})
parallel::clusterExport(cl,c('specs_satelite','fit_fluxo','escolher_lag','fit_arima'))
ajustar_arimas <- function(dados,confs) {
  xs <- unique(unlist(confs,use.names=FALSE))
  escolhas <- set_names(parallel::parLapply(cl,xs,escolher_lag,dados=dados),xs)
  out <- parallel::parLapply(cl,confs,fit_arima,dados=dados,escolhas=escolhas)
  set_names(out,paste0('ARIMA_',names(confs)))
}
chave <- list(banco=as_tibble(banco),codigo=tools::md5sum(file.path(pasta_testes,'comparar_modelos_FLUXO.R')))
cache <- file.path(destino,'ajustes.rds')
salvo <- if(file.exists(cache)) readRDS(cache) else NULL
message('ALVO UNICO = FLUXO | todas com intercepto | conferir significancia antes do CV')
modelos <- if(!is.null(salvo) && identical(salvo$chave,chave)) salvo$modelos else {
  c(ajustar_arimas(banco,combos),set_names(lapply(combos,fit_var,dados=banco),paste0('VAR_',names(combos))))
}
saveRDS(list(chave=chave,modelos=modelos),cache)
triagem <- imap_dfr(modelos,function(a,id) tibble(modelo=id,aprovado=a$aprovado,
  reprovadas=paste(a$teste$term[!is.finite(a$teste$p.value) | a$teste$p.value>.1],collapse='; ')))
write_csv(triagem,file.path(destino,'triagem ANTES DO CV.csv'))
write_csv(imap_dfr(modelos,~mutate(.x$teste,modelo=.y)),file.path(destino,'p valores FLUXO.csv'))
print(triagem,n=Inf)
ids <- triagem$modelo[triagem$aprovado]
message('CV sera executado APENAS para: ',paste(ids,collapse=', '))
# Diagnosticos e graficos de fluxo: todas as especificacoes, sem qualquer spread.
grDevices::pdf(tempfile(fileext='.pdf'))
diag <- list()
for(id in names(modelos)) {
  a <- modelos[[id]]; pasta <- file.path(destino,'residuos',id)
  dir.create(pasta,recursive=TRUE,showWarnings=FALSE)
  if(startsWith(id,'ARIMA')) {
    diag[[id]] <- diagnosticar_satelite(a$modelo) |> mutate(modelo=id)
    g <- grafico_residuos_satelite(a$modelo,'fluxo')
  } else {
    e <- residuals(a$modelo)[,1]
    g <- ggtime::gg_tsdisplay(tibble(date=banco$date,erro=e) |> as_tsibble(index=date),
      erro,plot_type='histogram',lag_max=24)
    diag[[id]] <- map_dfr(c(12,24),function(k) tibble(lag_teste=k,
      p_ljung_box=Box.test(e,lag=k,type='Ljung-Box')$p.value,modelo=id))
  }
  g[[1]] <- g[[1]]+labs(title=paste(id,'| ALVO: FLUXO | intercepto incluido'))+
    theme(plot.title=element_text(size=10),plot.subtitle=element_text(size=8))
  ggsave(file.path(pasta,'residuos FLUXO.png'),g,width=12,height=8,dpi=140,bg='white')
}
invisible(dev.off())
write_csv(bind_rows(diag),file.path(destino,'diagnosticos residuos.csv'))
origens <- seq(yearmonth('2024 Oct'),max(banco$date)-12L,by=1L)
cv <- list()
for(i in seq_along(origens)) {
  o <- origens[i]; t <- banco |> filter(date<=o); j <- banco |> filter(date>o,date<=o+12L)
  stopifnot(nrow(j)==12L)
  nomes_ar <- sub('^ARIMA_','',ids[startsWith(ids,'ARIMA')])
  fits_ar <- if(length(nomes_ar)) ajustar_arimas(t,combos[nomes_ar]) else list()
  for(id in ids) {
    if(startsWith(id,'ARIMA')) {
      a <- fits_ar[[id]]
      novos <- j |> as_tibble() |> select(date,all_of(a$termos)) |> as_tsibble(index=date)
      f <- forecast(a$modelo,new_data=novos)$.mean
    } else {
      a <- fit_var(combos[[sub('^VAR_','',id)]],t)
      f <- prever_var_condicional(a,t,j)
    }
    cv[[paste(i,id)]] <- tibble(origem=as.Date(o),date=as.Date(j$date),h=1:12,
      modelo=id,previsto=f,observado=j$fluxo,significativo_no_treino=a$aprovado)
  }
  cv[[paste(i,'benchmarks')]] <- bind_rows(
    tibble(modelo='Naive',previsto=rep(tail(t$fluxo,1),12)),
    tibble(modelo='Drift',previsto=tail(t$fluxo,1)+(1:12)*mean(diff(t$fluxo))),
    tibble(modelo='Media',previsto=rep(mean(t$fluxo),12))) |>
    mutate(origem=as.Date(o),date=rep(as.Date(j$date),3),h=rep(1:12,3),
      observado=rep(j$fluxo,3),significativo_no_treino=NA)
  write_csv(bind_rows(cv),file.path(destino,'CV parcial.csv'))
  message('CV SOMENTE FLUXO 12m: ',i,'/',length(origens),' | ',format(o))
}
parallel::stopCluster(cl)
previsoes <- bind_rows(cv)
tabela <- previsoes |> group_by(modelo) |> summarise(origens=n_distinct(origem),
  rmse_12m=sqrt(mean((previsto[h==12]-observado[h==12])^2)),
  mae_12m=mean(abs(previsto[h==12]-observado[h==12])),
  rmse_meses_1a12=sqrt(mean((previsto-observado)^2)),
  fracao_treinos_significativos=mean(significativo_no_treino),.groups='drop') |>
  left_join(triagem,by='modelo') |> arrange(rmse_12m)
write_csv(previsoes,file.path(destino,'CV previsoes FLUXO.csv'))
write_csv(tabela,file.path(destino,'TABELA CV FLUXO.csv'))
writeLines(c('ALVO UNICO: fluxo estrangeiro em trilhoes de ienes por mes. Nenhum spread usado nesta rodada.',
  'CV de 12 meses, 11 origens; explicativas realizadas em ARIMA e VAR; fluxo futuro previsto.',
  'Todas as configuracoes incluem intercepto. Intercepto e termos AR/MA proprios fora do corte de 10%.',
  'Variaveis: UST30 (%), FedFunds-callBoJ (%), variacao mensal de 100log USDJPY, CPI core 12m (%).',
  'Confs sem diferencial usam UST30. Fluxo sem log e d=0; ordens ARMA 0:3 selecionadas por AICc.',
  'VAR: fluxo + variaveis da conf, p=1:3 por AICc; trajetorias futuras das explicativas condicionadas aos realizados.',
  'Significancia conferida PRIMEIRO na amostra completa. Somente aprovadas entram no CV.',
  'VAR: significancia por bloco de lags de cada explicativa na equacao do fluxo; ARIMA: coeficientes economicos individuais.',
  'P-valores nominais apos selecao; triagem exploratoria com multiplos testes. CV nao e avaliacao independente da triagem.',
  'Nao se excluem origens do CV quando perdem significancia. Reporta fracao de treinos que passam.',
  'Ljung-Box dos VARs e marginal descritivo, nao Portmanteau multivariado.'),file.path(destino,'LEIA-ME.txt'))
print(tabela,n=Inf,width=Inf)
