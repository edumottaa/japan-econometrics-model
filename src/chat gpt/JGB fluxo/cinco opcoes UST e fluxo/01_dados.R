# Somente leitura e montagem dos dados. Nao estima modelos nem faz forecasts.
validar_mensal <- function(x,colunas,rotulo) {
  if(!nrow(x) || anyDuplicated(x$date) ||
     !identical(x$date,seq(min(x$date),max(x$date),by=1L)) ||
     !all(is.finite(as.matrix(select(x,all_of(colunas))))))
    stop('Serie mensal incompleta/duplicada: ',rotulo,'. Nao preenche lacunas silenciosamente.')
  invisible(TRUE)
}

ler_insumos <- function(origem=NULL,modo_cv=FALSE) {
  historico <- readRDS(file.path(fonte_modelo,'dados.rds')) |> as_tibble() |>
    mutate(date=yearmonth(date)) |> arrange(date)
  validar_mensal(historico,variaveis,'BVAR original')
  if(is.null(origem)) origem <- max(historico$date) else origem <- yearmonth(origem)
  stopifnot(origem %in% historico$date)
  jsda <- read_csv(file.path(fonte_dados,'fluxos_jsda_superlongos.csv'),show_col_types=FALSE) |>
    filter(investidor=='Foreigners') |> transmute(date=yearmonth(mes),fluxo=liquido_tri) |>
    arrange(date)
  validar_mensal(jsda,'fluxo','Compras liquidas estrangeiras JSDA')
  mensal <- function(x,nome) x |> filter(is.finite(.data[[nome]])) |>
    group_by(date=yearmonth(date)) |> summarise(!!nome:=mean(.data[[nome]]),.groups='drop')
  ust_diario <- read_csv(file.path(fonte_dados,'DGS30_2026-11-04.csv'),
    na=c('','.'),show_col_types=FALSE) |> transmute(date=observation_date,ust30=DGS30)
  ois_diario <- read_csv(file.path(fonte_dados,'ois2_historico.csv'),show_col_types=FALSE) |>
    transmute(date=as.Date(date),ois2=ois2_pct)
  aux <- full_join(mensal(ust_diario,'ust30'),mensal(ois_diario,'ois2'),by='date') |>
    full_join(jsda,by='date') |> arrange(date)
  banco <- historico |> filter(date<=origem) |>
    left_join(select(aux,date,ust30,ois2,fluxo),by='date')
  # Exogenas tem a amostra completa; fluxo domestico comeca na primeira observacao JSDA.
  validar_mensal(banco,c(variaveis,'ust30'),'BVAR + UST30')
  banco_fluxo <- banco |> filter(date>=min(jsda$date))
  validar_mensal(banco_fluxo,c(variaveis,'fluxo'),'BVAR + fluxo domestico')
  media_pos <- jsda |> filter(date>=yearmonth(POS_YCC),date<=origem)
  if(!nrow(media_pos)) stop('Sem observacoes pos-YCC disponiveis ate a origem.')
  media_fluxo <- mean(media_pos$fluxo)

  if(modo_cv) {
    datas <- seq(origem+1L,origem+CV_HORIZONTE,by=1L)
    stopifnot(max(datas)<=max(historico$date))
    caminhos <- historico |> filter(date %in% datas) |> select(date,all_of(variaveis)) |>
      mutate(across(all_of(variaveis[4:8]),~NA_real_),cenario='Ex post: externos realizados')
    futuro_aux <- tibble(date=datas) |> left_join(select(aux,date,ust30,ois2),by='date')
    fonte_aux <- 'Realizado futuro (CV condicional ex post); fluxo = media do treino pos-YCC'
    banda <- NULL # A meta prospectiva de 2028 nao e conhecida nas origens historicas.
  } else {
    stopifnot(origem==max(historico$date))
    caminhos <- readRDS(file.path(fonte_forecast,'caminhos_novos.rds')) |> as_tibble() |>
      mutate(date=yearmonth(date)) |> filter(date>origem) |> arrange(cenario,date)
    # O caminho salvo deixa outubro sem UST2Y. Para UST30-UST2Y ser uma exogena
    # definida, completa APENAS essas lacunas com o centro UST2Y ja salvo, por cenario.
    # A mesma trajetoria completa e condicionada em todos os testes e na referencia.
    trajetorias <- readRDS(file.path(fonte_forecast,'trajetorias modelos.rds'))
    datas_salvas <- sort(unique(caminhos$date))
    centros_ust2 <- imap_dfr(trajetorias,function(a,cen) {
      stopifnot(dim(a)[1]==8L,dim(a)[2]==length(datas_salvas))
      tibble(date=datas_salvas,cenario=cen,
        ust2_salvo=rowMeans(matrix(a[1,,],nrow=length(datas_salvas))))
    })
    stopifnot(!anyDuplicated(select(centros_ust2,date,cenario)))
    caminhos <- caminhos |> left_join(centros_ust2,by=c('date','cenario')) |>
      mutate(ust2_completado=!is.finite(ust2y),ust2y=coalesce(ust2y,ust2_salvo)) |>
      select(-ust2_salvo)
    stopifnot(all(is.finite(caminhos$ust2y)))
    datas <- sort(unique(caminhos$date))
    curvas <- read_csv(ARQUIVO_CURVAS,show_col_types=FALSE)
    snapshot <- unique(as.Date(curvas$data_snapshot))
    stopifnot(length(snapshot)==1L,!is.na(snapshot))
    # Ate o mes do snapshot: medias mensais observadas. A partir do mes seguinte:
    # FWCV tenor constante, interpolado entre os horizontes exportados.
    # Preserva o ajuste de media mensal usado nos scripts anteriores; nao confunde
    # Forward Rate de um mes com o rendimento de um titulo de 30 anos.
    futuro_aux <- tibble(date=datas)
    for(v in c('ust30','ois2')) {
      pontos <- curvas |> filter(variavel==v) |> arrange(horizonte_meses)
      stopifnot(!anyDuplicated(pontos$horizonte_meses),min(pontos$horizonte_meses)==0L)
      h <- max(as.integer(datas-yearmonth(snapshot)))
      f <- approx(pontos$horizonte_meses,pontos$taxa_pct,xout=0:h,rule=1)$y
      if(any(!is.finite(f))) stop('A curva ',v,' nao cobre todo o horizonte. Atualize ARQUIVO_CURVAS.')
      diario <- if(v=='ust30') ust_diario else ois_diario
      spot <- diario[[v]][diario$date==snapshot]
      stopifnot(length(spot)==1L,is.finite(spot))
      termo <- tibble(date=yearmonth(snapshot)+seq_len(h),
        valor=spot+(head(f,-1)+tail(f,-1))/2-f[1])
      obs <- aux[[v]][match(datas,aux$date)]
      futuro_aux[[v]] <- ifelse(datas<=yearmonth(snapshot),obs,
        termo$valor[match(datas,termo$date)])
    }
    fonte_aux <- paste('FWCV + nowcasts; snapshot',snapshot,
      '| media mensal aproximada, ajuste de nivel preservado do projeto')
    banda <- BANDA_JURO
  }
  stopifnot(length(datas)>0L,identical(datas,seq(origem+1L,max(datas),by=1L)))
  validar_mensal(futuro_aux,c('ust30','ois2'),'UST30/OIS2 futuro')
  for(cen in unique(caminhos$cenario))
    stopifnot(identical(filter(caminhos,cenario==cen)$date,datas))
  list(banco=banco,banco_fluxo=banco_fluxo,historico=historico,jsda=jsda,aux=aux,
    origem=origem,datas=datas,caminhos=caminhos,futuro_aux=futuro_aux,
    media_fluxo=media_fluxo,n_media_fluxo=nrow(media_pos),banda=banda,
    modo_cv=modo_cv,fonte_aux=fonte_aux)
}

estimar_satelite_teste <- function(d) {
  # Especificacao pedida, sem selecao de outra familia ou previsao de fluxo por UST:
  # Delta spread(pb) = c + beta*Delta OIS2(pb)*posYCC + gamma*fluxo(t-2) + erro branco.
  historia <- d$historico |> select(date,inclinacao) |>
    left_join(select(d$aux,date,ois2,fluxo),by='date') |> arrange(date) |>
    mutate(d_spread=100*(inclinacao-lag(inclinacao)),
      ois_pos=100*(ois2-lag(ois2))*as.integer(date>=yearmonth(POS_YCC)),
      fluxo_l2=lag(fluxo,2)) |>
    filter(date<=d$origem,if_all(c(d_spread,ois_pos,fluxo_l2),is.finite))
  validar_mensal(historia,c('d_spread','ois_pos','fluxo_l2'),'Treino do satelite')
  modelo <- historia |> as_tsibble(index=date) |>
    model(satelite=ARIMA(d_spread~1+ois_pos+fluxo_l2+pdq(0,0,0)+PDQ(0,0,0)))
  stopifnot(!any(vapply(modelo$satelite,inherits,logical(1),'null_mdl')))
  # Nunca utiliza o fluxo realizado depois da origem, nem para a media nem para lags.
  inteiro <- tibble(date=seq(min(d$jsda$date),max(d$datas),by=1L)) |>
    left_join(filter(d$jsda,date<=d$origem),by='date') |>
    mutate(fluxo=if_else(date>d$origem,d$media_fluxo,fluxo),fluxo_l2=lag(fluxo,2))
  ois <- bind_rows(filter(d$aux,date<=d$origem) |> select(date,ois2),
    select(d$futuro_aux,date,ois2)) |> arrange(date) |>
    mutate(ois_pos=100*(ois2-lag(ois2))*as.integer(date>=yearmonth(POS_YCC)))
  novos <- tibble(date=d$datas) |> left_join(select(ois,date,ois_pos),by='date') |>
    left_join(select(inteiro,date,fluxo_l2),by='date') |> as_tsibble(index=date)
  stopifnot(all(is.finite(novos$ois_pos)),all(is.finite(novos$fluxo_l2)))
  sat <- forecast(modelo,new_data=novos) |> as_tibble() |>
    transmute(date,delta_pb=.mean,spread_pb=100*tail(d$banco$inclinacao,1)+cumsum(.mean))
  residuos <- augment(modelo) |> filter(is.finite(.innov))
  diagnosticos <- map_dfr(c(12L,24L),function(k) tibble(lag=k,
    p_ljung_box=Box.test(residuos$.innov,lag=k,type='Ljung-Box',fitdf=0)$p.value))
  # Erro ARIMA(0,0,0): inovacoes independentes; a covariancia acumulada e construida
  # por montar_satelite_suave. Exclui incerteza de parametros e da media do fluxo.
  sigma2 <- glance(modelo)$sigma2
  stopifnot(length(sigma2)==1L,is.finite(sigma2),sigma2>0)
  if(any(diagnosticos$p_ljung_box<.05)) warning('Satelite: autocorrelacao residual; consultar diagnosticos.')
  list(modelo=modelo,satelite=sat,cov_delta_pb2=diag(sigma2,length(d$datas)),
    residuos=residuos,diagnosticos=diagnosticos,coeficientes=tidy(modelo),
    treino=historia,insumos=novos,media_fluxo=d$media_fluxo)
}
