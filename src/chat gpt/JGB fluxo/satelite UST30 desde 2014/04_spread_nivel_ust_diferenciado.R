# Teste separado: spread em nivel (pb), UST30 em variacao (pb), sem dummies.
# Erros sao brancos ou ARMA(p,q), p/q 0:2. d=0 preserva o alvo em nivel.
# Nao compara AICc entre spread em nivel e spread diferenciado.
executar_spread_nivel_ust_delta <- function() {
  d <- ler_insumos()
  destino <- file.path(pasta_testes,'resultados','spread nivel UST diferenciado')
  dir.create(destino,recursive=TRUE,showWarnings=FALSE)
  grade <- crossing(ois=c('delta','nivel'),k=0:2,erros=c('branco','arima')) |>
    mutate(forma='delta',regime='constante',
      id=paste('spread_nivel',paste0('OIS_',ois),paste0('USTdelta_lag',k),erros,sep='_'))
  ajustes <- list(); falhas <- list()
  for(i in seq_len(nrow(grade))) {
    g <- grade[i,]; modo <- g$ois
    ambiente <- new.env(parent=environment())
    ambiente$INICIO_SAT_UST <- yearmonth('2014 Feb')
    ambiente$base_satelite_ust <- local({
      modo_local <- modo
      function(d) {
        x <- base_satelite_ust(d)
        x$d_spread <- 100*x$inclinacao # Nome herdado da interface; resposta EM NIVEL.
        x$ois_pos <- if(modo_local=='nivel') 100*x$ois2 else
          100*(x$ois2-dplyr::lag(x$ois2))
        x
      }
    })
    fn <- ajustar_candidato_ust; environment(fn) <- ambiente
    a <- tryCatch(fn(d,g),error=function(e) {
      falhas[[g$id]] <<- tibble(id=g$id,erro=conditionMessage(e)); NULL})
    if(!is.null(a)) {
      # Insumos futuros coerentes com a transformacao realmente estimada.
      o <- bind_rows(d$aux |> filter(date<=d$origem) |> select(date,ois2),
                     select(d$futuro_aux,date,ois2)) |> arrange(date) |>
        mutate(ois_delta=100*(ois2-lag(ois2))) |> filter(date %in% d$datas)
      a$insumos$ois_pos <- if(modo=='nivel') 100*o$ois2 else o$ois_delta
      a$resumo <- mutate(a$resumo,alvo='spread em nivel (pb)',ois=.env$modo)
      ar <- a$coeficientes |> filter(grepl('^ar[0-9]+$',term)) |> pull(estimate)
      a$resumo$menor_raiz_AR <- if(length(ar)) min(Mod(polyroot(c(1,-ar)))) else Inf
      a$resumo$soma_AR <- sum(ar)
      a$unidades <- list(alvo='spread 30Y-5Y em nivel (pb)',
        ois=if(modo=='nivel') 'OIS2Y em nivel (pb)' else 'Delta OIS2Y (pb)',ust='Delta UST30 (pb)')
      ajustes[[g$id]] <- a
    }
  }
  tabela <- bind_rows(map(ajustes,'resumo'))
  if(!nrow(tabela)) stop('Todos os ajustes falharam: ',paste(map_chr(falhas,'erro'),collapse='; '))
  tabela <- arrange(tabela,desc(aprovado),AICc)
  coeficientes <- imap_dfr(ajustes,~mutate(.x$coeficientes,id=.y))
  write_csv(tabela,file.path(destino,'comparacao configuracoes.csv'))
  write_csv(coeficientes,file.path(destino,'coeficientes.csv'))
  write_csv(bind_rows(falhas),file.path(destino,'falhas.csv'))
  # Exporta residuos das melhores versoes por OIS, mesmo se reprovadas, com status explicito.
  for(modo in unique(tabela$ois)) {
    x <- filter(tabela,ois==modo) |> arrange(desc(aprovado),AICc) |> slice(1)
    pasta <- file.path(destino,paste0('OIS_',modo))
    dir.create(pasta,recursive=TRUE,showWarnings=FALSE)
    exportar_satelite(ajustes[[x$id]],pasta)
    e <- ajustes[[x$id]]$residuos |> transmute(date=as.Date(date),residuo=.innov)
    g1 <- ggplot(e,aes(date,residuo))+geom_hline(yintercept=0,colour='grey60')+
      geom_line()+geom_point(size=1)+theme_minimal()+
      labs(title=paste('Spread em nivel, Delta UST30 | OIS',modo),
        subtitle=paste('Ljung-Box: 12m p=',round(x$lb12,3),'| 24m p=',round(x$lb24,3)),
        x=NULL,y='Inovacoes (pb)')
    g3 <- ggplot(e,aes(residuo))+geom_histogram(bins=20,fill='grey50',colour='white')+
      theme_minimal()+labs(title='Distribuicao das inovacoes',x='Inovacoes (pb)',y='Frequencia')
    ggsave(file.path(pasta,'residuos 1.png'),g1,width=10,height=5,dpi=150,bg='white')
    ggsave(file.path(pasta,'residuos 3.png'),g3,width=10,height=5,dpi=150,bg='white')
    write_csv(x,file.path(pasta,'status e diagnosticos.csv'))
  }
  saveRDS(list(tabela=tabela,coeficientes=coeficientes,ajustes=ajustes),
    file.path(destino,'resultados.rds'))
  print(tabela,n=Inf,width=Inf)
  for(modo in unique(tabela$ois)) {
    id <- filter(tabela,ois==modo) |> arrange(desc(aprovado),AICc) |> slice(1) |> pull(id)
    print(filter(coeficientes,.data$id==.env$id))
  }
  invisible(list(tabela=tabela,coeficientes=coeficientes))
}
