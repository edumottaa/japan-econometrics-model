# Acrescenta apenas dummy YCC ao modelo em nivel com Delta OIS/UST e erros AR(2).
executar_spread_nivel_ycc <- function() {
  raiz_teste <- file.path(pasta_testes,'resultados','spread nivel UST diferenciado')
  anteriores <- readRDS(file.path(raiz_teste,'resultados.rds'))
  id_base <- 'spread_nivel_OIS_delta_USTdelta_lag0_arima'
  base <- anteriores$ajustes[[id_base]]
  stopifnot(!is.null(base))
  ycc <- function(x) as.integer(x>=yearmonth('2016 Sep') & x<yearmonth('2024 Apr'))
  treino <- base$treino |> mutate(ycc=ycc(date)) |> as_tsibble(index=date)
  novos <- base$insumos |> mutate(ycc=ycc(date))
  modelo <- treino |> model(satelite=ARIMA(d_spread~1+ois_pos+ust_x+ycc+
    pdq(2,0,0)+PDQ(0,0,0)))
  stopifnot(!inherits(modelo$satelite[[1]],'null_mdl'),all(novos$ycc==0))
  coef <- tidy(modelo); residuos <- augment(modelo) |> filter(is.finite(.innov))
  gl <- glance(modelo)
  lb <- map_dfr(c(12L,24L),function(k) tibble(lag=k,p_ljung_box=
    Box.test(residuos$.innov,lag=k,type='Ljung-Box',fitdf=2)$p.value))
  p_econ <- coef$p.value[match(c('ois_pos','ust_x','ycc'),coef$term)]
  ar <- coef$estimate[match(c('ar1','ar2'),coef$term)]
  e <- residuos$.innov
  z <- as.data.frame(embed(e^2,13)); names(z) <- paste0('v',seq_len(ncol(z)))
  arch <- lm(v1~.,data=z)
  diagnostico <- tibble(modelo='Spread nivel + Delta OIS + Delta UST + dummy YCC; AR(2)',
    inicio=as.Date(min(treino$date)),fim=as.Date(max(treino$date)),n=nrow(treino),
    AICc=gl$AICc,AICc_sem_dummy=base$resumo$AICc,
    p_dummy=coef$p.value[match('ycc',coef$term)],
    p_max_economico=max(p_econ),significativo=all(is.finite(p_econ) & p_econ<=.10),
    lb12=lb$p_ljung_box[1],lb24=lb$p_ljung_box[2],
    arch12_p=pchisq(nrow(z)*summary(arch)$r.squared,df=12,lower.tail=FALSE),
    shapiro_p=shapiro.test(e)$p.value,
    menor_raiz_AR=min(Mod(polyroot(c(1,-ar)))),
    aprovado=significativo & all(lb$p_ljung_box>=.05))
  # Resposta e nivel: nao acumula nem soma o ultimo spread realizado.
  prev <- forecast(modelo,new_data=novos); f <- as_tibble(prev)
  q <- function(p) vapply(seq_len(nrow(f)),function(i)
    as.numeric(unlist(quantile(f$d_spread[i],p=p))),numeric(1))
  futuro <- f |> transmute(date,spread_pb=.mean) |>
    mutate(p10=q(.1),p90=q(.9),p025=q(.025),p975=q(.975))
  stopifnot(all(is.finite(as.matrix(select(futuro,-date)))))
  # Usa a referencia ja projetada para conservar exatamente as mesmas curvas e origem.
  referencia <- readRDS(file.path(raiz_teste,'forecast','forecast.rds'))
  stopifnot(identical(futuro$date,referencia$tabela$date))
  observado <- referencia$observado
  ancora <- treino |> as_tibble() |> slice_tail(n=1) |>
    transmute(date,spread_pb=d_spread)
  linhas <- bind_rows(bind_rows(ancora,select(futuro,date,spread_pb)) |>
    mutate(modelo='Com dummy YCC'),
    bind_rows(ancora,select(referencia$tabela,date,spread_pb)) |>
      mutate(modelo='Sem dummy YCC')) |> mutate(date=as.Date(date))
  faixa <- mutate(futuro,date=as.Date(date))
  rotulos <- faixa |> filter(date %in% as.Date(c('2026-12-01','2027-12-01','2028-09-01'))) |>
    mutate(label=paste0(formatC(spread_pb,format='f',digits=1,decimal.mark=','),' pb'))
  destino <- file.path(raiz_teste,'dummy YCC')
  dir.create(destino,recursive=TRUE,showWarnings=FALSE)
  write_csv(coef,file.path(destino,'coeficientes.csv'))
  write_csv(diagnostico,file.path(destino,'diagnosticos.csv'))
  write_csv(mutate(futuro,date=as.Date(date)),file.path(destino,'forecast spread.csv'))
  write_csv(mutate(novos,date=as.Date(date)),file.path(destino,'regressores futuros.csv'))
  write_csv(as_tibble(treino) |> mutate(date=as.Date(date)),file.path(destino,'treino.csv'))
  write_csv(as_tibble(residuos) |> mutate(date=as.Date(date)),file.path(destino,'residuos.csv'))
  saveRDS(list(modelo=modelo,forecast=prev,tabela=futuro,diagnostico=diagnostico,
    coeficientes=coef,treino=treino,insumos=novos),file.path(destino,'resultado.rds'))
  g <- ggplot()+
    geom_ribbon(data=faixa,aes(date,ymin=p10,ymax=p90),fill='#b9d9ed',alpha=.65)+
    geom_line(data=observado,aes(date,spread_pb,colour='Realizado'),linewidth=.85)+
    geom_line(data=linhas,aes(date,spread_pb,colour=modelo,linetype=modelo),linewidth=.9)+
    geom_point(data=rotulos,aes(date,spread_pb),colour='#2563A6',size=2)+
    geom_label(data=rotulos,aes(date,spread_pb,label=label),nudge_y=11,
      colour='#16476d',fill='white',size=3.6,linewidth=.2)+
    geom_vline(xintercept=as.Date(max(treino$date)),linetype='dotted',colour='#777777')+
    scale_colour_manual(values=c('Realizado'='#171717','Com dummy YCC'='#2563A6',
      'Sem dummy YCC'='#999999'))+
    scale_linetype_manual(values=c('Com dummy YCC'='solid','Sem dummy YCC'='dashed'),guide='none')+
    scale_x_date(date_breaks='1 year',date_labels='%Y')+
    coord_cartesian(xlim=as.Date(c('2024-01-01','2028-12-01')))+
    theme_minimal(base_size=13)+theme(legend.position='top',panel.grid.minor=element_blank(),
      plot.title=element_text(face='bold'),plot.caption=element_text(hjust=0,size=10))+
    labs(title='Japao | JGB30Y - JGB5Y com dummy de YCC',
      subtitle=paste0('Spread em nivel; Delta OIS2Y e Delta UST30; erros AR(2) | p da dummy = ',
        formatC(diagnostico$p_dummy,format='f',digits=3,decimal.mark=',')),
      x=NULL,y='Pontos-base',colour=NULL,
      caption=paste0('Dummy = 1 de set/2016 a mar/2024; 0 fora desse periodo. Treino: fev/2014-ago/2026.\n',
        'Curvas Bloomberg de 30/10/2026. Mesmos dados, origem e regressores da versao sem dummy.\n',
        'Faixa de 80% condicional; exclui incerteza dos parametros e das curvas. Set/out realizados sem reancoragem.\n',
        if(diagnostico$aprovado) 'Passou na triagem de significancia e Ljung-Box; ainda sem CV.' else
          'Nao passou em todos os filtros: forecast exploratorio; nao adotar automaticamente.',
        ' Satelite isolado; nao e o BVAR condicionado.'))
  ggsave(file.path(destino,'forecast com dummy YCC.png'),g,width=13,height=7,dpi=160,bg='white')
  er <- residuos |> transmute(date=as.Date(date),residuo=.innov)
  gres <- ggplot(er,aes(date,residuo))+geom_hline(yintercept=0,colour='grey60')+
    geom_line()+geom_point(size=1)+theme_minimal()+
    labs(title='Residuos do satelite com dummy YCC',x=NULL,y='Inovacoes (pb)',
      subtitle=paste('Ljung-Box: 12m p=',round(diagnostico$lb12,3),
        '| 24m p=',round(diagnostico$lb24,3)))
  ggsave(file.path(destino,'residuos com dummy YCC.png'),gres,width=10,height=5,dpi=150,bg='white')
  print(coef); print(diagnostico,width=Inf)
  print(filter(futuro,date %in% yearmonth(c('2026 Dec','2027 Dec','2028 Sep'))))
  invisible(list(diagnostico=diagnostico,forecast=futuro))
}
