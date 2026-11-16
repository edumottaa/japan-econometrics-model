# Funcoes preservadas da configuracao selecionada. Source apenas define funcoes.

estimar_satelite_teste <- function(d) {
  # Especificacao pedida, sem selecao de outra familia ou previsao de fluxo por UST:
  # Delta spread(pb) = c + beta*Delta OIS2(pb)*posYCC + gamma*fluxo(t-2) + erro branco.
  historia <- preparar_variaveis_satelite(d$historico, d$aux, d$origem)
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


carregar_bvar_original <- function(d) {
  banco_original <- d$historico |> arrange(date)
  if(d$origem!=max(banco_original$date))
    stop('Opcoes 3/4 usam o BVAR salvo na origem atual. Para origens historicas use o CV, que reestima so com dados de treino.')
  post <- readRDS(file.path(fonte_modelo,'modelo.rds'))
  y <- as.matrix(select(banco_original,all_of(variaveis)))
  p <- post$last_draw$p
  stopifnot(p==P,nrow(post$posterior$A)==8L,
    max(abs(t(y[-seq_len(p),,drop=FALSE])-post$last_draw$data_matrices$Y))<1e-8,
    is.finite(post$posterior$ess),post$posterior$ess>=ESS_MINIMO)
  list(modelo=post,y=y,vars=variaveis,tipo='original salvo',
    inicio=min(banco_original$date))
}

