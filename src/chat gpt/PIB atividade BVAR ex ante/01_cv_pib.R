# Ponte existente do PIB, alimentada pelas atividades EX ANTE do CV BVAR atual.
# Nenhuma funcao deste arquivo chama estimacao ou forecast do BVAR.
fontes_atividade_pib <- function() tibble(
  modelo_pib=c('PIB | BVAR original','PIB | opcao 1: UST30',
    'PIB | opcao 2: UST30-UST2Y','PIB | opcao 3: satelite suave','PIB | opcao 4: satelite rigido'),
  opcao_fonte=c(1L,1L,2L,3L,4L),
  modelo_bvar=c('BVAR original ex post','1: UST30 exogeno','2: UST30 - UST2Y exogeno',
    '3: Satelite suave','4: Satelite rigido'))

ler_bases_pib_cv <- function() {
  mensal <- readRDS(dados_bvar_pib) |> as_tibble() |>
    transmute(date=yearmonth(date),atividade=exp(atividade/100)) |> as_tsibble(index=date)
  trimestral <- mensal |> index_by(trimestre=yearquarter(date)) |>
    summarise(meses=n(),atividade=mean(atividade)) |> rename(date=trimestre) |>
    filter(meses==3L) |> mutate(atividade_crescimento=100*(atividade/lag(atividade)-1))
  pib <- readRDS(arquivo_pib) |> mutate(pib_crescimento=100*(pib/lag(pib)-1)) |>
    left_join(select(trimestral,-meses),by='date') |>
    drop_na(pib_crescimento,atividade_crescimento) |> arrange(date)
  list(mensal=mensal,pib=pib)
}

chave_pib_cv <- function() {
  fontes <- c(dados_bvar_pib,arquivo_pib,funcoes_ponte,
    file.path(pasta_pib,c('00_configuracao.R','01_cv_pib.R')))
  list(hash=tools::md5sum(fontes),cv_bvar=chave_cv_original(chave_execucao()),
    horizonte_mensal_maximo=12L,treino_pib='ate trimestre anterior a origem',
    benchmarks=c('Naive','Drift'),atividade='media em nivel dos sorteios BVAR; futura prevista')
}

ler_cache_pib <- function(arquivo,chave) {
  if(!REESTIMAR_PIB && file.exists(arquivo)) {
    s <- readRDS(arquivo)
    if(identical(s$chave,chave)) return(s$valor)
  }
  NULL
}

salvar_cache_pib <- function(arquivo,chave,valor) {
  dir.create(dirname(arquivo),recursive=TRUE,showWarnings=FALSE)
  saveRDS(list(chave=chave,valor=valor),arquivo)
  valor
}

planejar_pib_cv <- function() {
  fontes <- fontes_atividade_pib()
  map_dfr(seq_len(nrow(fontes)),function(i) {
    pasta <- file.path(raiz_cv_original,paste0('opcao_',fontes$opcao_fonte[i]))
    arquivos <- sort(list.files(pasta,pattern='^origem_[0-9]{4}-[0-9]{2}\\.rds$',full.names=TRUE))
    if(!length(arquivos)) stop('CV de atividade ainda nao salvo: ',fontes$modelo_pib[i])
    tibble(modelo_pib=fontes$modelo_pib[i],modelo_bvar=fontes$modelo_bvar[i],
      arquivo=arquivos,
      origem=yearmonth(as.Date(paste0(sub('^origem_(.*)\\.rds$','\\1',basename(arquivos)),'-01'))))
  })
}

preparar_origem_pib <- function(origem,plano,bases,chave_bvar) {
  linhas <- plano |> filter(.data$origem==.env$origem)
  # Reusar leitura quando original e opcao 1 vem do mesmo arquivo.
  objetos <- set_names(lapply(unique(linhas$arquivo),readRDS),unique(linhas$arquivo))
  if(!all(vapply(objetos,function(x) identical(x$chave,chave_bvar),logical(1))))
    stop('CV BVAR de outra configuracao na origem ',origem,'. Nao mistura as bases.')
  o <- yearquarter(origem)
  treino <- bases$pib |> filter(date<=o-1L)
  if(!nrow(treino)) stop('Sem treino trimestral do PIB na origem ',origem)
  futuros <- map(seq_len(nrow(linhas)),function(i) {
    atividade_prevista <- objetos[[linhas$arquivo[i]]]$valor |>
      filter(variavel=='atividade',.model==linhas$modelo_bvar[i]) |> arrange(date)
    stopifnot(nrow(atividade_prevista)>0L,!anyDuplicated(atividade_prevista$date),
      all(atividade_prevista$date>origem),all(atividade_prevista$h<=12L))
    futuro <- preparar_futuro_pontes(
      bases$mensal |> filter(date<=origem),atividade_prevista,
      ultimo_pib=max(treino$date),log_bvar=TRUE) |>
      filter(date<=max(bases$pib$date))
    stopifnot(nrow(futuro)>0L,min(futuro$date)==max(treino$date)+1L,
      !any(has_gaps(futuro)$.gaps),all(is.finite(futuro$atividade_crescimento)))
    futuro
  }) |> set_names(linhas$modelo_pib)
  # Comparacao dentro de cada origem nos mesmos trimestres e horizontes.
  datas <- Reduce(intersect,lapply(futuros,function(x) x$date))
  futuros <- lapply(futuros,function(x) filter(x,date %in% datas))
  list(origem=o,origem_mensal=origem,treino=treino,futuros=futuros,
    dados_fontes=linhas,.id=unique(objetos[[1]]$valor$.id),
    sem_teste=!any(futuros[[1]]$date>o))
}

executar_cv_pib <- function() {
  bases <- ler_bases_pib_cv(); plano <- planejar_pib_cv(); chave <- chave_pib_cv()
  origens <- sort(unique(plano$origem)); resultados <- diagnosticos <- escolhas <- list()
  status <- list()
  dir.create(file.path(saida_pib,'checkpoints'),recursive=TRUE,showWarnings=FALSE)
  for(i in seq_along(origens)) {
    origem <- origens[i]
    codigo <- format(as.Date(origem),'%Y-%m')
    arquivos <- unique(filter(plano,.data$origem==.env$origem)$arquivo)
    chave_janela <- list(base=chave,arquivos=tools::md5sum(arquivos))
    arquivo_janela <- file.path(saida_pib,'checkpoints',paste0('origem_',codigo,'.rds'))
    valor <- ler_cache_pib(arquivo_janela,chave_janela)
    erro <- NA_character_
    if(is.null(valor)) erro <- tryCatch({
      janela <- preparar_origem_pib(origem,plano,bases,chave$cv_bvar)
      if(janela$sem_teste) {
        valor <- list(sem_teste=TRUE,previsoes=NULL,
          diagnostico=tibble(origem=as.Date(origem),status='Sem trimestre futuro completo observado'))
      } else {
        # 1. Mesma selecao fpp3 da ponte original, feita somente no treino.
        arquivo_ponte <- file.path(saida_pib,'cache pontes',paste0('ponte_',codigo,'.rds'))
        selecao <- ler_cache_pib(arquivo_ponte,chave)
        if(is.null(selecao)) {
          selecao <- ajustar_pontes_pib(janela$treino)
          salvar_cache_pib(arquivo_ponte,chave,selecao)
        }
        # 2. Uma ponte estimada por origem, aplicada aos cinco caminhos de atividade.
        pontes <- imap_dfr(janela$futuros,function(futuro,nome) {
          forecast(selecao$modelos$atividade,new_data=futuro) |> as_tibble() |>
            mutate(.model=nome)
        })
        # 3. Somente Naive/Drift; mesmos dados de treino e datas da ponte.
        benchmarks <- selecao$preparo$dados |> model(
          Naive=NAIVE(pib_crescimento),Drift=RW(pib_crescimento~drift())) |>
          forecast(new_data=select(janela$futuros[[1]],date)) |> as_tibble()
        previsoes <- bind_rows(pontes,benchmarks) |> filter(date>janela$origem) |>
          mutate(.id=janela$.id,origem=janela$origem,h=as.integer(date-janela$origem),
            periodo=case_when(year(date)<=2019~'2015-2019',
              year(date)<=2021~'2020-2021',TRUE~'2022-2026'))
        stopifnot(!anyDuplicated(select(previsoes,.model,date)),
          all(previsoes$h %in% 1:4),all(is.finite(previsoes$.mean)))
        valor <- list(sem_teste=FALSE,previsoes=previsoes,escolhas=selecao$escolhidos |>
          mutate(origem=as.Date(origem)),
          diagnostico=tibble(origem=as.Date(origem),ultimo_pib_treino=as.character(max(janela$treino$date)),
            modelos_atividade=length(janela$futuros),trimestres_avaliados=n_distinct(previsoes$date)))
      }
      salvar_cache_pib(arquivo_janela,chave_janela,valor)
      NA_character_
    },error=conditionMessage)
    if(is.na(erro)) {
      resultados[[i]] <- valor$previsoes; diagnosticos[[i]] <- valor$diagnostico
      escolhas[[i]] <- valor$escolhas
    }
    status[[i]] <- tibble(origem=as.Date(origem),
      status=if(is.na(erro)) if(isTRUE(valor$sem_teste)) 'Sem trimestre completo' else 'Concluido' else 'Falhou',
      erro=erro)
    write_csv(bind_rows(status),file.path(saida_pib,'status.csv'))
    message('CV PIB | ',i,'/',length(origens),' | ',origem,' | ',status[[i]]$status)
  }
  previsoes <- bind_rows(resultados)
  if(!nrow(previsoes)) stop('Nenhuma janela de PIB concluida; veja status.csv.')
  # Resultados parciais tambem sao guardados; nunca publicar conclusao completa em caso de falha.
  resultado <- list(chave=chave,previsoes=previsoes,diagnosticos=bind_rows(diagnosticos),
    escolhas=bind_rows(escolhas),status=bind_rows(status),
    completo=!any(bind_rows(status)$status=='Falhou'))
  saveRDS(resultado,file.path(saida_pib,'resultados_cv_pib.rds'))
  write_csv(resultado$diagnosticos,file.path(saida_pib,'diagnosticos.csv'))
  write_csv(resultado$escolhas,file.path(saida_pib,'escolhas por janela.csv'))
  resumir_cv_pib(resultado,bases)
  if(!resultado$completo) stop('CV PIB parcial. Corrija as janelas com erro e rode o mesmo executor novamente.')
  invisible(resultado)
}

metricas_pib <- function(previsoes,bases) {
  f <- previsoes |> as_tsibble(key=c(.id,.model),index=date) |>
    as_fable(response='pib_crescimento',distribution='pib_crescimento')
  obs <- bases$pib |> select(date,pib_crescimento)
  medidas <- list(RMSE=RMSE,MAE=MAE,CRPS=CRPS)
  contar <- function(colunas) previsoes |> group_by(across(all_of(colunas))) |>
    summarise(origens=n_distinct(origem),casos=n(),.groups='drop')
  list(geral=accuracy(f,obs,by='.model',measures=medidas) |> left_join(contar('.model'),by='.model'),
    periodos=accuracy(f,obs,by=c('periodo','.model'),measures=medidas) |>
      left_join(contar(c('periodo','.model')),by=c('periodo','.model')),
    horizontes=accuracy(f,obs,by=c('h','.model'),measures=medidas) |>
      left_join(contar(c('h','.model')),by=c('h','.model')))
}

resumir_cv_pib <- function(resultado,bases=ler_bases_pib_cv()) {
  p <- resultado$previsoes
  todas <- metricas_pib(p,bases)
  write_csv(todas$geral,file.path(saida_pib,'metricas gerais - cobertura disponivel.csv'))
  write_csv(todas$periodos,file.path(saida_pib,'metricas por periodo - cobertura disponivel.csv'))
  modelos <- fontes_atividade_pib()$modelo_pib
  amostras <- list('cinco BVARs'=modelos,'original e opcoes 1 e 2'=modelos[1:3])
  comparacoes <- imap(amostras,function(grupo,nome) {
    chaves <- p |> filter(.model %in% grupo) |> distinct(.id,date,.model) |>
      count(.id,date,name='modelos') |> filter(modelos==length(grupo)) |> select(.id,date)
    comum <- p |> filter(.model %in% c(grupo,'Naive','Drift')) |>
      semi_join(chaves,by=c('.id','date'))
    if(!nrow(comum)) return(NULL)
    metricas <- metricas_pib(comum,bases)
    pasta <- file.path(saida_pib,nome); dir.create(pasta,recursive=TRUE,showWarnings=FALSE)
    write_csv(metricas$geral,file.path(pasta,'metricas gerais.csv'))
    write_csv(metricas$periodos,file.path(pasta,'metricas por periodo.csv'))
    write_csv(metricas$horizontes,file.path(pasta,'metricas por horizonte.csv'))
    write_csv(comum |> select(-pib_crescimento) |>
      left_join(as_tibble(bases$pib) |> select(date,observado=pib_crescimento),by='date') |>
      mutate(date=as.Date(date),origem=as.Date(origem)),file.path(pasta,'previsoes pontuais.csv'))
    gerar_graficos_pib(metricas,comum,pasta,nome)
    list(metricas=metricas,previsoes=comum)
  })
  saveRDS(list(disponiveis=todas,comparacoes=comparacoes),file.path(saida_pib,'comparacoes_cv_pib.rds'))
  print(todas$geral,width=Inf)
  invisible(comparacoes)
}
