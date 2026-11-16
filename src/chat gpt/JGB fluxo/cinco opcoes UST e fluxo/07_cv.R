# CV de 12 meses, expanding window, so apos os forecasts no executor principal.
# Condicional ex post aos externos e OIS. Fluxo futuro = media calculada no treino.
# Reestima BVAR e satelite em cada origem: nunca usa a posterior de 2026 no passado.
executar_cv <- function(numero,chave=chave_execucao()) {
  env <- environment(estimar_configuracao)
  opcoes <- list(S=CV_S,HIPER_S=CV_HIPER_S,HIPER_BURN=CV_HIPER_BURN,
    SORTEIOS_FORECAST=CV_SORTEIOS_FORECAST)
  anteriores <- mget(names(opcoes),envir=env,inherits=FALSE)
  on.exit(list2env(anteriores,envir=env),add=TRUE)
  list2env(opcoes,envir=env)
  historico <- readRDS(file.path(fonte_modelo,'dados.rds')) |> as_tibble() |>
    mutate(date=yearmonth(date)) |> arrange(date)
  ultima <- max(historico$date)-CV_HORIZONTE
  if(yearmonth(CV_INICIO)>ultima) stop('Sem origens completas para o CV.')
  origens <- seq(yearmonth(CV_INICIO),ultima,by=CV_PASSO)
  destino <- file.path(pasta_testes,'resultados',paste0('opcao_',numero),'CV')
  dir.create(destino,recursive=TRUE,showWarnings=FALSE)
  previsoes <- list(); status <- list()
  for(k in seq_along(origens)) {
    o <- origens[k]
    arquivo <- file.path(destino,paste0('origem_',format(as.Date(o),'%Y-%m'),'.rds'))
    salvo <- cache_compativel(arquivo,chave)
    if(!is.null(salvo)) {
      previsoes[[k]] <- salvo$previsoes
      status[[k]] <- tibble(origem=as.Date(o),status='Concluido',erro=NA_character_)
      next
    }
    erro <- tryCatch({
      d <- ler_insumos(o,modo_cv=TRUE)
      stopifnot(is.null(d$banda),max(d$banco$date)==o,
        all(d$caminhos$date>o),length(d$datas)==12L)
      valor <- calcular_opcao(numero,d,chave)
      previsto <- valor$tabela |> filter(variavel=='inclinacao') |>
        transmute(date,modelo,previsto_pb=100*valor)
      treino <- historico |> filter(date<=o)
      inicial <- 100*tail(treino$inclinacao,1)
      drift <- mean(diff(100*treino$inclinacao))
      # Mesmos benchmarks para todas as opcoes: Naive e Drift, e somente estes.
      benchmarks <- bind_rows(tibble(date=d$datas,modelo='Naive',previsto_pb=inicial),
        tibble(date=d$datas,modelo='Drift',previsto_pb=inicial+seq_len(12)*drift))
      observado <- historico |> filter(date %in% d$datas) |>
        transmute(date,observado_pb=100*inclinacao)
      linhas <- bind_rows(previsto,benchmarks) |> left_join(observado,by='date') |>
        mutate(teste=numero,origem=o,h=as.integer(date-o),erro_pb=previsto_pb-observado_pb)
      stopifnot(all(is.finite(linhas$erro_pb)),all(linhas$h %in% 1:12))
      guardar_cache(arquivo,chave,list(meta=valor$meta,previsoes=linhas))
      previsoes[[k]] <- linhas
      NA_character_
    },error=conditionMessage)
    status[[k]] <- tibble(origem=as.Date(o),status=if(is.na(erro)) 'Concluido' else 'Falhou',erro=erro)
    write_csv(bind_rows(status),file.path(destino,'status por origem.csv'))
    parcial <- bind_rows(previsoes)
    if(nrow(parcial)) write_csv(parcial |> mutate(date=as.Date(date),origem=as.Date(origem)),
      file.path(destino,'previsoes parciais.csv'))
    message('CV ',numero,': ',k,'/',length(origens),' | ',o,' | ',tail(status,1)[[1]]$status)
  }
  tab <- bind_rows(previsoes)
  meta <- list(teste=numero,origens_esperadas=as.Date(origens),
    completo=length(previsoes)==length(origens) &&
      all(bind_rows(status)$status=='Concluido'),status=bind_rows(status))
  guardar_cache(file.path(destino,'resultado.rds'),chave,list(meta=meta,previsoes=tab))
  write_csv(bind_rows(status),file.path(destino,'status por origem.csv'))
  if(nrow(tab)) {
    write_csv(tab |> mutate(date=as.Date(date),origem=as.Date(origem)),file.path(destino,'previsoes.csv'))
    write_csv(metricas_cv(tab),file.path(destino,'metricas.csv'))
  }
  if(!meta$completo) stop('Opcao ',numero,': uma ou mais origens falharam. Veja status por origem.csv.')
  invisible(tab)
}

metricas_cv <- function(tab) {
  resumir <- function(x,rotulo) x |> group_by(teste,modelo) |>
    summarise(origens=n_distinct(origem),previsoes=n(),
      rmse_pb=sqrt(mean(erro_pb^2)),mae_pb=mean(abs(erro_pb)),vies_pb=mean(erro_pb),
      .groups='drop') |> mutate(avaliacao=rotulo,.before=1)
  bind_rows(resumir(filter(tab,h==12L),'Horizonte 12: mes final'),
    resumir(tab,'Trajetoria: meses 1 a 12'))
}

comparar_cv <- function(chave) {
  arquivos <- file.path(pasta_testes,'resultados',paste0('opcao_',seq_along(nomes_testes)),
    'CV','resultado.rds')
  resultados <- lapply(arquivos,function(a) cache_compativel(a,chave,TRUE)) |> compact()
  if(!length(resultados)) return(invisible(NULL))
  destino <- file.path(pasta_testes,'resultados','comparacao')
  dir.create(destino,recursive=TRUE,showWarnings=FALSE)
  tab <- bind_rows(lapply(resultados,`[[`,'previsoes'))
  cobertura <- bind_rows(lapply(resultados,function(x) tibble(teste=x$meta$teste,
    completo=x$meta$completo,origens_concluidas=sum(x$meta$status$status=='Concluido'),
    origens_esperadas=length(x$meta$origens_esperadas))))
  write_csv(cobertura,file.path(destino,'CV cobertura.csv'))
  if(!nrow(tab)) return(invisible(NULL))
  todas <- metricas_cv(tab)
  write_csv(todas,file.path(destino,'CV metricas disponiveis.csv'))
  # Comparar as mesmas origens entre as opcoes disponiveis de cada bloco:
  # primeiro 1-4, depois 1-5, ao final 1-8. A cobertura identifica as ausentes.
  presentes <- sort(as.integer(vapply(resultados,function(x) x$meta$teste,numeric(1))))
  conjuntos <- lapply(presentes,function(i)
    unique(tab$origem[tab$teste==i & tab$modelo==nomes_testes[i]]))
  comuns <- Reduce(intersect,conjuntos)
  tab_comum <- tab |> filter(origem %in% comuns)
  if(!nrow(tab_comum)) {
    writeLines('Sem origens concluidas comuns aos testes disponiveis. Consultar cobertura e status.',
      file.path(destino,'CV SEM comparacao comum.txt'))
    warning('Ainda nao ha comparacao CV comum aos testes disponiveis.')
    return(invisible(todas))
  }
  comparacao <- metricas_cv(tab_comum) |>
    mutate(opcoes_disponiveis=length(presentes),opcoes_totais=length(nomes_testes))
  write_csv(comparacao,file.path(destino,'CV comparacao mesmas origens.csv'))
  write_csv(tab_comum |> mutate(date=as.Date(date),origem=as.Date(origem)),
    file.path(destino,'CV previsoes mesmas origens.csv'))
  barras <- comparacao |> filter(avaliacao=='Horizonte 12: mes final',
    modelo %in% c(nomes_testes,'Naive','Drift','Referencia: BVAR original')) |>
    select(modelo,rmse_pb,origens) |> distinct()
  g <- ggplot(barras,aes(reorder(modelo,rmse_pb),rmse_pb))+geom_col(fill='#426A8C')+
    geom_text(aes(label=round(rmse_pb,1)),hjust=-.1)+coord_flip()+
    scale_y_continuous(expand=expansion(mult=c(0,.15)))+theme_minimal()+
    labs(title='CV condicional: erro do spread no mes 12',x=NULL,y='RMSE (pb)',
      subtitle=paste(length(comuns),'origens comuns |',length(presentes),'de',
        length(nomes_testes),'opcoes disponiveis'),
      caption='Externos/OIS realizados; fluxo futuro = media pos-YCC do treino. Sem banda BoJ futura no CV. Janelas sobrepostas.')
  ggsave(file.path(destino,'CV RMSE 12 meses.png'),g,width=11,height=7,dpi=150,bg='white')
  curvas <- tab_comum |> filter(modelo %in% c(nomes_testes,'Naive','Drift')) |>
    group_by(modelo,h) |> summarise(rmse_pb=sqrt(mean(erro_pb^2)),.groups='drop')
  g2 <- ggplot(curvas,aes(h,rmse_pb,colour=modelo))+geom_line()+geom_point()+
    scale_x_continuous(breaks=1:12)+theme_minimal()+theme(legend.position='bottom')+
    labs(title='CV: erro por horizonte',x='Meses a frente',y='RMSE (pb)',colour=NULL)
  ggsave(file.path(destino,'CV erro por horizonte.png'),g2,width=12,height=7,dpi=150,bg='white')
  if(MOSTRAR_GRAFICOS) {print(g); print(g2)}
  print(comparacao |> filter(modelo %in% nomes_testes),n=Inf)
  invisible(comparacao)
}
