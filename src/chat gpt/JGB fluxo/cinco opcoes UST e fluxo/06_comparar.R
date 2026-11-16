exportar_satelite <- function(sat,destino) {
  write_csv(sat$coeficientes,file.path(destino,'coeficientes satelite.csv'))
  write_csv(sat$diagnosticos,file.path(destino,'diagnosticos satelite.csv'))
  write_csv(sat$residuos |> mutate(date=as.Date(date)),file.path(destino,'residuos satelite.csv'))
  e <- sat$residuos |> transmute(date=as.Date(date),residuo=.innov)
  g1 <- ggplot(e,aes(date,residuo))+geom_hline(yintercept=0,colour='grey60')+
    geom_line()+geom_point(size=1)+theme_minimal()+
    labs(title='Satelite do spread: residuos',x=NULL,y='pb/mes',
      subtitle=paste('Ljung-Box:',paste0(sat$diagnosticos$lag,'m p=',
        round(sat$diagnosticos$p_ljung_box,3),collapse='; ')))
  ac <- stats::acf(e$residuo,lag.max=24,plot=FALSE)
  a <- tibble(lag=as.numeric(ac$lag)[-1],acf=as.numeric(ac$acf)[-1])
  g2 <- ggplot(a,aes(lag,acf))+geom_hline(yintercept=0)+geom_segment(aes(xend=lag,yend=0))+
    geom_hline(yintercept=c(-1,1)*1.96/sqrt(nrow(e)),linetype=2,colour='blue')+
    theme_minimal()+labs(title='ACF dos residuos do satelite',x='Defasagem',y='ACF')
  g3 <- ggplot(e,aes(residuo))+geom_histogram(bins=20,fill='grey50',colour='white')+
    theme_minimal()+labs(title='Distribuicao dos residuos',x='pb/mes',y='Frequencia')
  for(j in 1:3) ggsave(file.path(destino,paste0('residuos ',j,'.png')),
    list(g1,g2,g3)[[j]],width=10,height=5,dpi=150,bg='white')
  if(MOSTRAR_GRAFICOS) {print(g1); print(g2); print(g3)}
}

comparar_forecasts <- function(chave) {
  arquivos <- file.path(pasta_testes,'resultados',paste0('opcao_',seq_along(nomes_testes)),
    'forecast','resultado.rds')
  resultados <- lapply(arquivos,function(a) cache_compativel(a,chave,TRUE)) |> compact()
  if(!length(resultados)) return(invisible(NULL))
  destino <- file.path(pasta_testes,'resultados','comparacao')
  dir.create(destino,recursive=TRUE,showWarnings=FALSE)
  tab <- bind_rows(lapply(resultados,`[[`,'tabela'))
  # Referencia e comum, eliminada a repeticao entre opcoes.
  completo <- tab |> select(-teste,-efeito_pb,-efeito_mesma_amostra_pb,
    -referencia,-controle_mesma_amostra) |> distinct()
  write_csv(tab |> mutate(date=as.Date(date)),file.path(destino,'todos forecasts.csv'))
  finais <- tab |> filter(modelo %in% nomes_testes,date==max(date))
  write_csv(finais |> mutate(date=as.Date(date)),file.path(destino,'valores finais.csv'))
  spread <- completo |> filter(variavel=='inclinacao') |>
    mutate(date=as.Date(date),valor=100*valor)
  labels <- spread |> group_by(cenario,modelo) |> slice_max(date,n=1,with_ties=FALSE) |>
    mutate(label=paste0(round(valor,1),' pb'))
  g <- ggplot(spread,aes(date,valor,colour=modelo))+geom_line(linewidth=.8)+
    geom_text(data=labels,aes(label=label),hjust=-.05,size=2.8,show.legend=FALSE)+
    facet_wrap(vars(cenario),ncol=2,scales='free_y')+
    scale_x_date(date_labels='%m/%Y',expand=expansion(mult=c(.02,.2)))+
    theme_minimal()+theme(legend.position='bottom')+
    labs(title='JGB30Y - JGB5Y: opcoes e BVAR original',
      subtitle='Fluxo futuro = media pos-YCC | BoJ terminal: 1,75% a 2,25%',
      x=NULL,y='Pontos-base',colour=NULL,
      caption='Centros condicionais. Opcoes 5-8 tem amostra menor; consultar controle sem fluxo na mesma amostra.')
  ggsave(file.path(destino,'spread quatro cenarios.png'),g,width=15,height=9,dpi=150,bg='white')
  # Todas as variaveis: atividade e CPI em variacao 12 meses; demais em nivel.
  hist <- readRDS(file.path(fonte_modelo,'dados.rds')) |> as_tibble() |>
    select(date,all_of(variaveis)) |>
    mutate(across(c(brent,ip_mundial,atividade,cpi_core,usdjpy),~exp(.x/100))) |>
    pivot_longer(-date,names_to='variavel',values_to='valor')
  base <- completo |> filter(cenario=='Base') |> group_by(modelo) |>
    group_modify(function(x,keys) {
      bind_rows(hist,select(x,date,variavel,valor)) |> arrange(variavel,date) |>
        group_by(variavel) |> mutate(valor=case_when(
          variavel %in% c('atividade','cpi_core') ~ 100*(valor/lag(valor,12)-1),
          variavel=='inclinacao' ~ 100*valor,TRUE ~ valor)) |>
        ungroup() |> filter(date %in% x$date)
    }) |> ungroup() |> mutate(date=as.Date(date))
  labels_base <- base |> group_by(modelo,variavel) |> slice_max(date,n=1,with_ties=FALSE) |>
    mutate(label=as.character(round(valor,2)))
  g2 <- ggplot(base,aes(date,valor,colour=modelo))+geom_line(linewidth=.7)+
    geom_text(data=labels_base,aes(label=label),hjust=-.1,size=2.4,show.legend=FALSE)+
    facet_wrap(vars(variavel),ncol=3,scales='free_y')+
    scale_x_date(date_labels='%m/%Y',expand=expansion(mult=c(.02,.22)))+
    theme_minimal()+theme(legend.position='bottom')+
    labs(title='Cenario Base: variaveis projetadas',x=NULL,y=NULL,colour=NULL,
      caption='CPI/atividade: % em 12 meses; inclinacao: pb; juros: %; fluxo: JPY tri/mes.')
  ggsave(file.path(destino,'todas variaveis Base.png'),g2,width=16,height=11,dpi=150,bg='white')
  triagem <- tab |> filter(modelo %in% nomes_testes,variavel=='inclinacao') |>
    group_by(teste,modelo,cenario) |> summarise(
      maior_efeito_pb=max(abs(efeito_pb)),spread_final_pb=100*last(valor),
      efeito_final_pb=last(efeito_pb),.groups='drop') |>
    mutate(status=if_else(maior_efeito_pb<1e-6,'Sem efeito numerico no spread','Avaliar coerencia e CV'))
  write_csv(triagem,file.path(destino,'triagem spread.csv'))
  if(MOSTRAR_GRAFICOS) {print(g); print(g2)}
  print(triagem,n=Inf)
  invisible(triagem)
}
