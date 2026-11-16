# Diagnosticos apenas: nao executa forecast BVAR/CV, nem altera a selecao anterior.
executar_sem_dummy_cointegracao <- function() {
  d <- ler_insumos()
  destino <- file.path(pasta_testes,'resultados','sem dummy e cointegracao')
  dir.create(destino,recursive=TRUE,showWarnings=FALSE)
  grade <- crossing(forma=c('delta','nivel'),k=0:2,regime='constante',
    erros=c('branco','arima')) |> mutate(id=paste(forma,paste0('lag',k),erros,sep='_'))
  ajustes <- list(); falhas <- list()
  for(modo in c('OIS_pos_YCC','OIS_sem_regime')) {
    # Mesmo periodo em ambos: fev/2014 em diante, pois Delta OIS requer jan/2014.
    # Ambiente local evita alterar os scripts/funcoes do experimento principal.
    ambiente <- new.env(parent=environment())
    ambiente$INICIO_SAT_UST <- yearmonth('2014 Feb')
    if(modo=='OIS_sem_regime') ambiente$base_satelite_ust <- function(d) {
      x <- base_satelite_ust(d)
      x$ois_pos <- 100*(x$ois2-dplyr::lag(x$ois2))
      x
    }
    fn <- ajustar_candidato_ust; environment(fn) <- ambiente
    for(i in seq_len(nrow(grade))) {
      g <- grade[i,]; id <- paste(modo,g$id,sep='__')
      a <- tryCatch(fn(d,g),error=function(e) {
        falhas[[id]] <<- tibble(id=id,erro=conditionMessage(e)); NULL})
      if(!is.null(a)) {
        # Corresponde ao OIS sem regime tambem nos insumos futuros exportados.
        if(modo=='OIS_sem_regime') {
          o <- bind_rows(d$aux |> filter(date<=d$origem) |> select(date,ois2),
                         select(d$futuro_aux,date,ois2)) |> arrange(date) |>
            mutate(d_ois=100*(ois2-lag(ois2))) |> filter(date %in% d$datas)
          a$insumos$ois_pos <- o$d_ois
        }
        a$resumo <- mutate(a$resumo,id=.env$id,ois=.env$modo)
        ajustes[[id]] <- a
      }
    }
  }
  comparacao <- bind_rows(map(ajustes,'resumo')) |> arrange(desc(aprovado),AICc)
  stopifnot(nrow(comparacao)>0L,length(unique(comparacao$n))==1L)
  write_csv(comparacao,file.path(destino,'comparacao sem dummy.csv'))
  write_csv(imap_dfr(ajustes,~mutate(.x$coeficientes,id=.y)),
    file.path(destino,'coeficientes sem dummy.csv'))
  write_csv(bind_rows(falhas),file.path(destino,'falhas.csv'))
  # Referencia anterior com dummy, agora exatamente nas mesmas 151 observacoes.
  ambiente_ref <- new.env(parent=environment())
  ambiente_ref$INICIO_SAT_UST <- yearmonth('2014 Feb')
  fn_ref <- ajustar_candidato_ust; environment(fn_ref) <- ambiente_ref
  ref <- fn_ref(d,tibble(forma='delta',k=0L,regime='dummy',erros='arima',
                         id='Referencia_OIS_pos_e_dummy'))
  comparaveis <- bind_rows(ref$resumo |> mutate(ois='OIS_pos_YCC'),
    comparacao |> filter(forma=='delta',k==0L,erros=='arima'))
  stopifnot(length(unique(comparaveis$n))==1L)
  write_csv(arrange(comparaveis,AICc),file.path(destino,'referencias mesma amostra.csv'))
  for(modo in unique(comparacao$ois)) {
    aptos <- filter(comparacao,ois==modo,aprovado)
    if(nrow(aptos)) {
      id <- slice_min(aptos,AICc,n=1,with_ties=FALSE)$id
      pasta <- file.path(destino,modo); dir.create(pasta,recursive=TRUE,showWarnings=FALSE)
      exportar_satelite(ajustes[[id]],pasta)
    }
  }

  # Ordem de integracao: ADF H0=raiz unitaria; KPSS H0=estacionariedade.
  # Series em nivel para cointegracao, nao as variacoes do satelite.
  niveis <- base_satelite_ust(d) |>
    filter(date>=yearmonth('2014 Jan'),date<=d$origem) |>
    transmute(date,spread=inclinacao,ust30,ois2)
  validar_mensal(niveis,c('spread','ust30','ois2'),'Niveis para cointegracao')
  testes_raiz <- objetos_raiz <- list()
  for(v in c('spread','ust30','ois2')) for(ordem in 0:2) {
    x <- niveis[[v]]; if(ordem>0L) x <- diff(x,differences=ordem)
    for(det in c('drift','trend')) {
      a <- urca::ur.df(x,type=det,lags=12,selectlags='AIC')
      tau <- if(det=='drift') 'tau2' else 'tau3'
      k <- urca::ur.kpss(x,type=if(det=='drift') 'mu' else 'tau',lags='long')
      stat <- as.numeric(a@teststat[1,tau]); crit <- a@cval[tau,'5pct']
      ks <- as.numeric(k@teststat); kc <- as.numeric(k@cval[1,'5pct'])
      id <- paste(v,ordem,det,sep='_')
      testes_raiz[[id]] <- tibble(variavel=v,diferencas=ordem,deterministico=det,
        adf=stat,adf_critico_5=crit,adf_rejeita_raiz_5=stat<crit,
        kpss=ks,kpss_critico_5=kc,kpss_rejeita_estacionariedade_5=ks>kc)
      objetos_raiz[[id]] <- list(adf=a,kpss=k)
    }
  }
  raiz <- bind_rows(testes_raiz)
  write_csv(raiz,file.path(destino,'ADF KPSS.csv'))

  # Phillips-Ouliaris Pz: H0 = ausencia de cointegracao. Rejeita na cauda superior.
  # Valores criticos proprios de cointegracao; nao usa o ADF comum dos residuos.
  sistemas <- list(spread_UST30=c('spread','ust30'),spread_OIS2=c('spread','ois2'),
                   spread_UST30_OIS2=c('spread','ust30','ois2'))
  co <- objetos_co <- list()
  for(sistema in names(sistemas)) for(det in c('constant','trend')) for(banda in c('short','long')) {
    vars <- sistemas[[sistema]]
    z <- as.matrix(select(niveis,all_of(vars)))
    a <- urca::ca.po(z,demean=det,lag=banda,type='Pz')
    id <- paste(sistema,det,banda,sep='_')
    stat <- as.numeric(a@teststat); crit <- as.numeric(a@cval[1,'5pct'])
    # Precondicao conservadora: todas aparentam I(1) sob os dois testes com constante.
    check <- map_lgl(vars,function(v) {
      r0 <- filter(raiz,variavel==v,diferencas==0,deterministico=='drift')
      r1 <- filter(raiz,variavel==v,diferencas==1,deterministico=='drift')
      !r0$adf_rejeita_raiz_5 && r0$kpss_rejeita_estacionariedade_5 &&
        r1$adf_rejeita_raiz_5 && !r1$kpss_rejeita_estacionariedade_5
    })
    co[[id]] <- tibble(sistema,deterministico=det,banda,n=nrow(z),estatistica=stat,
      critico_5=crit,rejeita_nao_cointegracao_5=stat>crit,
      I1_compativel_todas=all(check))
    objetos_co[[id]] <- a
  }
  cointegracao <- bind_rows(co)
  write_csv(cointegracao,file.path(destino,'Phillips Ouliaris.csv'))
  write_csv(mutate(niveis,date=as.Date(date)),file.path(destino,'series em nivel.csv'))
  saveRDS(list(comparacao=comparacao,ajustes=ajustes,raiz=raiz,
    referencias=comparaveis,
    testes_raiz=objetos_raiz,cointegracao=cointegracao,testes_co=objetos_co),
    file.path(destino,'resultados.rds'))
  capture.output({lapply(objetos_raiz,summary); lapply(objetos_co,summary)},
    file=file.path(destino,'testes completos.txt'))
  print(comparacao,n=Inf,width=Inf); print(raiz,n=Inf,width=Inf)
  print(cointegracao,n=Inf,width=Inf)
  invisible(list(comparacao=comparacao,raiz=raiz,cointegracao=cointegracao))
}
