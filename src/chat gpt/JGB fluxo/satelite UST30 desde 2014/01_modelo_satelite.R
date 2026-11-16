# 1. Dados e transformacoes. Spread/OIS e Delta UST em pb; UST em nivel em %.
base_satelite_ust <- function(d) {
  d$historico |> select(date,inclinacao) |>
    left_join(select(d$aux,date,ois2,ust30),by='date') |> arrange(date) |>
    mutate(d_spread=100*(inclinacao-lag(inclinacao)),
      # Antes de abr/2024 o efeito do OIS e imposto zero, como no satelite atual.
      ois_pos=if_else(date>=yearmonth('2024 Apr'),100*(ois2-lag(ois2)),0),
      d_ust=100*(ust30-lag(ust30)),
      pos=as.integer(date>=yearmonth('2024 Apr')),
      ycc=as.integer(date>=yearmonth('2016 Sep') & date<yearmonth('2024 Apr')),
      pre=as.integer(date<yearmonth('2016 Sep')))
}
regressores_ust <- function(x,forma,k,regime) {
  x <- arrange(x,date)
  x$ust_x <- dplyr::lag(if(forma=='delta') x$d_ust else x$ust30,k)
  if(regime=='pos') x$ust_pos <- x$ust_x*x$pos
  if(regime=='tres') {
    x$ust_pre <- x$ust_x*x$pre; x$ust_ycc <- x$ust_x*x$ycc
    x$ust_pos <- x$ust_x*x$pos
  }
  x
}
grade_ust <- function() {
  bind_rows(crossing(forma=c('delta','nivel'),k=0:2,regime='constante'),
    crossing(forma='delta',k=0:2,regime=c('dummy','pos','tres'))) |>
    crossing(erros=c('branco','arima')) |>
    mutate(id=paste(forma,paste0('lag',k),regime,erros,sep='_'))
}

# 2. Estimacao. Sem coeficientes imputados; intercepto em todas as configuracoes.
ajustar_candidato_ust <- function(d,g) {
  completo <- regressores_ust(base_satelite_ust(d),g$forma,g$k,g$regime)
  termos <- c('ois_pos',if(g$regime=='tres') c('ust_pre','ust_ycc','ust_pos') else 'ust_x',
    if(g$regime=='pos') 'ust_pos',if(g$regime=='dummy') 'pos')
  treino <- completo |> filter(date>=INICIO_SAT_UST,date<=d$origem) |>
    select(date,d_spread,all_of(termos))
  validar_mensal(treino,c('d_spread',termos),'Satelite UST desde 2014')
  stopifnot(nrow(treino)>=36L)
  pdq_txt <- if(g$erros=='branco') 'pdq(0,0,0)' else 'pdq(0:2,0,0:2)'
  f <- as.formula(paste('d_spread ~ 1 +',paste(termos,collapse=' + '),
                         '+',pdq_txt,'+ PDQ(0,0,0)'))
  modelo <- treino |> as_tsibble(index=date) |>
    model(satelite=ARIMA(f,stepwise=FALSE,approximation=FALSE))
  stopifnot(!inherits(modelo$satelite[[1]],'null_mdl'))
  coef <- tidy(modelo)
  residuos <- augment(modelo) |> filter(is.finite(.innov))
  n_ar_ma <- sum(grepl('^(ar|ma)[0-9]',coef$term))
  lb <- map_dfr(c(12L,24L),function(k) tibble(lag=k,
    p_ljung_box=Box.test(residuos$.innov,lag=k,type='Ljung-Box',fitdf=n_ar_ma)$p.value))
  # 3. Diagnosticos de variancia e distribuicao; nao equivalem a autocorrelacao da media.
  e <- residuos$.innov
  z <- as.data.frame(embed(e^2,13)); names(z) <- paste0('v',seq_len(ncol(z)))
  arch <- lm(v1~.,data=z)
  arch_p <- pchisq(nrow(z)*summary(arch)$r.squared,df=12,lower.tail=FALSE)
  p_economicos <- coef$p.value[match(termos,coef$term)]
  gl <- glance(modelo)
  resumo <- tibble(id=g$id,forma=g$forma,k=g$k,regime=g$regime,erros=g$erros,
    n=nrow(treino),inicio=as.Date(min(treino$date)),fim=as.Date(max(treino$date)),
    n_pos=sum(treino$date>=yearmonth('2024 Apr')),AICc=gl$AICc,
    p_max_economico=max(p_economicos),
    significativo=all(is.finite(p_economicos) & p_economicos<=.10),
    lb12=lb$p_ljung_box[1],lb24=lb$p_ljung_box[2],
    residuos_sem_autocorrelacao=all(lb$p_ljung_box>=.05),
    arch12_p=arch_p,shapiro_p=shapiro.test(e)$p.value,
    aprovado=significativo & residuos_sem_autocorrelacao,n_termos=length(termos))
  # 4. Insumos futuros: somente UST30 e OIS da curva/realizado ex post no CV.
  aux <- bind_rows(d$aux |> filter(date<=d$origem) |> select(date,ois2,ust30),
    select(d$futuro_aux,date,ois2,ust30)) |> arrange(date) |>
    mutate(d_ust=100*(ust30-lag(ust30)),
      ois_pos=if_else(date>=yearmonth('2024 Apr'),100*(ois2-lag(ois2)),0),
      pos=as.integer(date>=yearmonth('2024 Apr')),
      ycc=as.integer(date>=yearmonth('2016 Sep') & date<yearmonth('2024 Apr')),
      pre=as.integer(date<yearmonth('2016 Sep')))
  novos <- regressores_ust(aux,g$forma,g$k,g$regime) |>
    filter(date %in% d$datas) |> select(date,all_of(termos)) |> as_tsibble(index=date)
  stopifnot(nrow(novos)==length(d$datas),
    all(is.finite(as.matrix(select(as_tibble(novos),-date)))))
  list(modelo=modelo,coeficientes=coef,residuos=residuos,diagnosticos=lb,
    treino=treino,insumos=novos,resumo=resumo,cov_delta_pb2=NULL)
}

testar_satelites_ust <- function(d,exportar=FALSE) {
  grade <- grade_ust(); ajustes <- list(); falhas <- list()
  for(i in seq_len(nrow(grade))) {
    g <- grade[i,]
    ajuste <- tryCatch(ajustar_candidato_ust(d,g),error=function(e) {
      falhas[[g$id]] <<- tibble(id=g$id,erro=conditionMessage(e)); NULL})
    if(!is.null(ajuste)) ajustes[[g$id]] <- ajuste
  }
  tabela <- bind_rows(map(ajustes,'resumo'))
  if(!nrow(tabela)) stop('Nenhum satelite estimado: ',paste(head(map_chr(falhas,'erro'),3),collapse='; '))
  tabela <- arrange(tabela,desc(aprovado),AICc)
  aptos <- filter(tabela,aprovado)
  # Entre modelos a menos de 2 AICc do melhor, prefere menos regressores.
  escolhido <- if(nrow(aptos)) aptos |> filter(AICc<=min(AICc)+2) |>
    arrange(n_termos,AICc) |> slice(1) else NULL
  if(exportar) {
    destino <- file.path(pasta_testes,'resultados','triagem')
    dir.create(destino,recursive=TRUE,showWarnings=FALSE)
    write_csv(tabela,file.path(destino,'comparacao configuracoes.csv'))
    write_csv(bind_rows(falhas),file.path(destino,'falhas.csv'))
    write_csv(imap_dfr(ajustes,~mutate(.x$coeficientes,id=.y)),file.path(destino,'coeficientes.csv'))
    saveRDS(list(tabela=tabela,escolhido=escolhido,ajustes=ajustes),file.path(destino,'triagem.rds'))
    for(id in tabela$id) {
      arq <- file.path(destino,id); dir.create(arq,recursive=TRUE,showWarnings=FALSE)
      exportar_satelite(ajustes[[id]],arq)
    }
    print(tabela,n=Inf,width=Inf)
    if(!is.null(escolhido)) print(escolhido)
  }
  list(tabela=tabela,escolhido=escolhido,ajustes=ajustes)
}

# Adaptador minimo: o BVAR continua usando as mesmas funcoes de condicionamento.
estimar_satelite_teste <- function(d) {
  testes <- testar_satelites_ust(d,exportar=FALSE)
  arq <- file.path(pasta_testes,'resultados','selecao por origem',
    paste0(format(as.Date(d$origem),'%Y-%m'),'.csv'))
  dir.create(dirname(arq),recursive=TRUE,showWarnings=FALSE)
  write_csv(testes$tabela,arq)
  if(is.null(testes$escolhido)) stop('Nenhuma configuracao aprovada nesta origem: ',d$origem,
    '. Ver significancia e residuos; nao publica condicionamento reprovado.')
  id <- testes$escolhido$id; sat <- testes$ajustes[[id]]
  prev <- forecast(sat$modelo,new_data=sat$insumos) |> as_tibble()
  sat$satelite <- prev |> transmute(date,delta_pb=.mean,
    spread_pb=100*tail(d$banco$inclinacao,1)+cumsum(.mean))
  # Simulacao preserva correlacao serial dos erros ARIMA ao acumular o nivel.
  # Parametros/curvas tratados como fixos, como no condicionamento suave anterior.
  set.seed(123L+as.integer(as.Date(d$origem)))
  sim <- generate(sat$modelo,new_data=sat$insumos,times=3000) |> as_tibble()
  M <- sim |> select(date,.rep,.sim) |> pivot_wider(names_from=.rep,values_from=.sim) |> arrange(date)
  V <- cov(t(as.matrix(select(M,-date)))); V <- (V+t(V))/2
  stopifnot(all(is.finite(V)),nrow(V)==length(d$datas))
  sat$cov_delta_pb2 <- V+diag(max(mean(diag(V))*1e-10,1e-10),nrow(V))
  sat$media_fluxo <- NA_real_; sat$configuracao <- testes$escolhido
  sat
}
