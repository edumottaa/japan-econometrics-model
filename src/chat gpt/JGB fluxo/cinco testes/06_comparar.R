anualizar_teste <- function(tabela,d) {
  obs <- d$historico |> filter(date<=d$origem) |>
    select(date,all_of(variaveis)) |>
    mutate(across(c(brent,ip_mundial,atividade,cpi_core,usdjpy),~exp(.x/100))) |>
    pivot_longer(-date,names_to='variavel',values_to='valor')
  tabela |> group_by(cenario,modelo) |> group_modify(function(x,chaves) {
    historia <- bind_rows(obs,select(x,date,variavel,valor)) |>
      arrange(variavel,date) |> group_by(variavel) |>
      mutate(valor_plot=if_else(variavel %in% c('atividade','cpi_core'),
        100*(valor/lag(valor,12)-1),valor)) |> ungroup() |>
      filter(date %in% d$datas) |> select(date,variavel,valor_plot)
    left_join(x,historia,by=c('date','variavel'))
  }) |> ungroup()
}

comparar_testes <- function() {
  arquivos <- file.path(pasta_testes,'resultados',paste0('teste_',1:6),'resultado.rds')
  arquivos <- arquivos[file.exists(arquivos)]
  if(!length(arquivos)) return(invisible(NULL))
  resultados <- lapply(arquivos,readRDS)
  atual <- resultados[[which.max(file.info(arquivos)$mtime)]]$meta
  compativel <- vapply(resultados,function(x)
    identical(x$meta$chave,atual$chave) && identical(x$meta$origem,atual$origem) &&
      identical(x$meta$datas,atual$datas) && identical(x$meta$banda,atual$banda) &&
      identical(x$meta$inicio,atual$inicio) &&
      identical(x$meta$parametros,atual$parametros),logical(1))
  if(any(!compativel)) warning('Ha resultados de outra configuracao/origem: excluidos da comparacao atual.')
  resultados <- resultados[compativel]
  tabela <- bind_rows(lapply(resultados,`[[`,'forecast'))
  # O controle e identico, vindo do mesmo cache e do mesmo condicionamento.
  nomes_controle <- c('BVAR sem fluxo: amostra comum','BVAR original salvo: amostra completa')
  controle <- tabela |> filter(modelo %in% nomes_controle) |>
    select(-teste) |> distinct()
  alt <- tabela |> filter(!modelo %in% nomes_controle)
  completo <- bind_rows(controle,select(alt,-teste))
  destino <- file.path(pasta_testes,'resultados','comparacao')
  dir.create(destino,recursive=TRUE,showWarnings=FALSE)
  write_csv(completo |> mutate(date=as.Date(date)),file.path(destino,'todos forecasts.csv'))
  spread <- alt |> filter(variavel=='inclinacao') |> arrange(teste,cenario,date)
  revisao <- spread |> group_by(teste,modelo,cenario) |>
    summarise(spread_final_pb=100*last(valor),efeito_final_pb=last(efeito_pb),
      maior_efeito_absoluto_pb=max(abs(efeito_pb)),
      maior_variacao_mensal_pb=if(n()>1L) max(abs(diff(100*valor))) else NA_real_,
      .groups='drop') |>
    mutate(status=if_else(maior_efeito_absoluto_pb<1e-6,
      'Sem efeito numerico no spread; candidato a descarte',
      'Altera o spread; avaliar coerencia, depois CV'))
  write_csv(revisao,file.path(destino,'triagem do spread.csv'))
  juros <- completo |> filter(variavel=='juro_curto',date==max(date))
  stopifnot(all(juros$valor>=BANDA_JURO[1]-1e-7),all(juros$valor<=BANDA_JURO[2]+1e-7))
  write_csv(juros |> mutate(date=as.Date(date)),file.path(destino,'verificacao banda juros.csv'))
  cobertura <- bind_rows(lapply(resultados,function(x) tibble(teste=x$meta$teste,
    modelo=x$meta$nome,meses_ex_post=x$meta$meses_fluxo_ex_post,
    horizonte=length(x$meta$datas),ex_post_completo=x$meta$ex_post_completo)))
  write_csv(cobertura,file.path(destino,'cobertura fluxo realizado.csv'))
  titulos <- c(ust2y='UST2Y (%)',brent='Brent (USD)',ip_mundial='IP mundial (indice)',
    atividade='Atividade (% 12m)',cpi_core='CPI core (% 12m)',
    juro_curto='Juro curto (%)',inclinacao='JGB30Y-5Y (pb)',usdjpy='USD/JPY',
    fluxo='Fluxo estrangeiro (tri de ienes/mes)')
  subtitulo <- paste('Origem:',format(atual$origem),
    '| Testes 1/6: BVAR original; testes 2-5: amostra comum | BoJ terminal: 1,75%-2,25%')
  figura <- completo |> filter(variavel=='inclinacao') |>
    mutate(date=as.Date(date),pb=100*valor)
  rotulos <- figura |> group_by(cenario,modelo) |> slice_max(date,n=1,with_ties=FALSE) |>
    ungroup() |> mutate(rotulo=paste0(formatC(pb,format='f',digits=1,decimal.mark=','),' pb'))
  g <- ggplot(figura,aes(date,pb,colour=modelo)) + geom_line(linewidth=.8) +
    geom_text(data=rotulos,aes(label=rotulo),hjust=-.1,size=3,show.legend=FALSE) +
    facet_wrap(vars(cenario),ncol=2,scales='free_y') +
    scale_x_date(date_breaks='6 months',date_labels='%m/%Y',expand=expansion(mult=c(.02,.22))) +
    theme_minimal(base_size=11) + theme(legend.position='bottom',panel.grid.minor=element_blank()) +
    labs(title='Spread JGB30Y-5Y: seis testes e controles',subtitle=subtitulo,
      x=NULL,y='pb',colour=NULL,
      caption='Centros condicionais. Ex post fixa apenas fluxo realizado disponivel; veja cobertura. CV ainda nao executado.')
  ggsave(file.path(destino,'spread quatro cenarios.png'),g,width=14,height=9,dpi=150,bg='white')
  cen_base <- if('Base' %in% completo$cenario) 'Base' else unique(completo$cenario)[1]
  base <- completo |> filter(cenario==cen_base) |>
    mutate(date=as.Date(date),valor_plot=if_else(variavel=='inclinacao',100*valor_plot,valor_plot))
  rotulos_base <- base |> group_by(modelo,variavel) |> slice_max(date,n=1,with_ties=FALSE) |>
    ungroup() |> mutate(rotulo=formatC(valor_plot,format='f',digits=2,decimal.mark=','))
  g2 <- ggplot(base,aes(date,valor_plot,colour=modelo)) + geom_line(linewidth=.7) +
    geom_text(data=rotulos_base,aes(label=rotulo),hjust=-.1,size=2.5,show.legend=FALSE) +
    facet_wrap(vars(variavel),ncol=3,scales='free_y',labeller=as_labeller(titulos)) +
    scale_x_date(date_labels='%m/%Y',expand=expansion(mult=c(.02,.22))) +
    theme_minimal(base_size=10) + theme(legend.position='bottom') +
    labs(title=paste('Todas as variaveis:',cen_base),subtitle=subtitulo,
      x=NULL,y=NULL,colour=NULL,caption='Criterio de selecao: contribuicao para o spread, nao para as demais variaveis.')
  ggsave(file.path(destino,'variaveis Base.png'),g2,width=15,height=11,dpi=150,bg='white')
  print(revisao,n=Inf,width=Inf)
  message('Comparacao atualizada com ',length(resultados),'/6 testes. Nao seleciona modelo sem avaliar forecast e CV.')
  invisible(revisao)
}
