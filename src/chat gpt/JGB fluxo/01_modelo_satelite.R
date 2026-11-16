# CONFIGURACOES SELECIONADAS 
# Fluxo: media historica pos-YCC, sem regressao de previsao por UST30.
# Spread: Delta(30Y-5Y) = B + b*d(OIS2Y*posYCC) + b*Fluxo(t-2) + e.
# Unidades: spread/OIS em pb; fluxo estrangeiro JSDA em trilhoes de ienes/mes

# 1. Fluxo futuro constante na media de abr/2024 ate o fim da amostra
pos_ycc <- yearmonth('2024 Apr')
fluxo_pos_ycc <- jsda |> filter(mes>=pos_ycc,mes<=fim_amostra)
stopifnot(nrow(fluxo_pos_ycc)>0L,all(is.finite(fluxo_pos_ycc$fluxo)))
media_fluxo_pos_ycc <- mean(fluxo_pos_ycc$fluxo)
fluxo_futuro <- futuro |> as_tibble() |> transmute(mes,fluxo=media_fluxo_pos_ycc)
fluxo_completo <- bind_rows(jsda |> filter(mes<=fim_amostra),fluxo_futuro) |>
  arrange(mes) |> mutate(fluxo_l2=lag(fluxo,2))

# 2.  Evaluation
treino2 <- base |> filter(mes>=inicio_amostra,mes<=fim_amostra,
  if_all(c(d_spread,ois_pos,fluxo_l2),is.finite)) |> as_tsibble(index=mes)
stopifnot(nrow(treino2)>30L,!any(has_gaps(treino2)$.gaps))
estagio2 <- treino2 |>
  model(spread=ARIMA(d_spread~1+ois_pos+fluxo_l2+pdq(0,0,0)+PDQ(0,0,0)))
stopifnot(!any(vapply(estagio2$spread,inherits,logical(1),'null_mdl')))
coeficientes <- tidy(estagio2) |> mutate(estagio='Spread')
write_csv(coeficientes,file.path(saida,'coeficientes satelite.csv'))
write_csv(tibble(configuracao='Media historica pos-YCC',inicio=as.Date(pos_ycc),
  fim=as.Date(fim_amostra),n=nrow(fluxo_pos_ycc),media_tri=media_fluxo_pos_ycc),
  file.path(saida,'premissa fluxo media pos YCC.csv'))

# 3. Diagnotico dos Residuos
residuos <- augment(estagio2) |> filter(is.finite(.innov))
diagnosticos <- purrr::map_dfr(c(12L,24L),function(k) tibble(etapa='Spread',
  lag_teste=k,n=nrow(residuos),
  ljung_box_p=Box.test(residuos$.innov,lag=k,type='Ljung-Box',fitdf=0)$p.value))
write_csv(diagnosticos,file.path(saida,'diagnosticos satelite.csv'))
write_csv(residuos |> as_tibble() |> mutate(mes=as.Date(mes)),
  file.path(saida,'residuos satelite.csv'))
if(any(diagnosticos$ljung_box_p<.05))
  warning('Autocorrelacao residual no spread. Consulte diagnosticos antes de interpretar faixas.')

# 4.Forecast
novos <- futuro |> left_join(select(fluxo_completo,mes,fluxo_l2),by='mes')
stopifnot(all(is.finite(novos$fluxo_l2)),all(is.finite(novos$ois_pos)))
previsao_sat <- forecast(estagio2,new_data=novos)
nivel_inicial <- base |> filter(mes==fim_amostra) |> pull(spread)
stopifnot(length(nivel_inicial)==1L,is.finite(nivel_inicial))
satelite <- previsao_sat |> as_tibble() |>
  transmute(mes,delta_spread=.mean,spread=nivel_inicial+cumsum(delta_spread)) |>
  left_join(fluxo_futuro,by='mes')

#5 Graficos
set.seed(123)
faixa <- generate(estagio2,new_data=novos,times=2000) |> as_tibble() |>
  arrange(.rep,mes) |> group_by(.rep) |> mutate(nivel=nivel_inicial+cumsum(.sim)) |>
  ungroup() |> group_by(mes) |>
  summarise(p10=quantile(nivel,.1),p90=quantile(nivel,.9),.groups='drop')
satelite <- left_join(satelite,faixa,by='mes')
write_csv(satelite |> mutate(mes=as.Date(mes)),file.path(saida,'forecast satelite.csv'))
write_csv(satelite |> left_join(base |> select(mes,observado=spread),by='mes') |>
  filter(is.finite(observado)) |> mutate(erro_pb=spread-observado,mes=as.Date(mes)),
  file.path(saida,'nowcast meses observados apos amostra.csv'))
saveRDS(list(configuracao_fluxo='media_pos_ycc',media_fluxo_pos_ycc=media_fluxo_pos_ycc,
  estagio1=NULL,estagio2=estagio2,fluxo_futuro=fluxo_futuro,
  fluxo_completo=fluxo_completo,satelite=satelite),file.path(saida,'satelite.rds'))
print(coeficientes)
print(tail(satelite,1))
