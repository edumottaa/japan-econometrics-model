# Forecast: sorteios do BVAR de producao reponderados pelo satelite (tilting entropico).
# Nao reestima o BVAR; usa as trajetorias salvas pelo forecast de producao.
variaveis <- c('ust2y','brent','ip_mundial','atividade','cpi_core','juro_curto','inclinacao','usdjpy')
em_log <- c('brent','ip_mundial','atividade','cpi_core','usdjpy')

# 1. Sorteios do BVAR de producao (8 variaveis x 25 meses x 2500)
trajetorias <- readRDS('src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao/trajetorias modelos.rds')
datas <- origem+seq_len(dim(trajetorias[[1]])[2])

# 2. Premissas do satelite. OIS2Y: ate o snapshot, media mensal observada; depois,
# spot + variacao da curva forward (Bloomberg). Fluxo futuro = media pos-YCC.
curva <- curvas |> filter(variavel=='ois2') |> arrange(horizonte_meses)
snapshot <- unique(as.Date(curva$data_snapshot))
n <- max(as.integer(datas-yearmonth(snapshot)))
f <- approx(curva$horizonte_meses,curva$taxa_pct,xout=0:n,rule=1)$y
termo <- ois2$ois2[ois2$date==snapshot]+(head(f,-1)+tail(f,-1))/2-f[1]
media_fluxo <- fluxo_estrangeiro |> filter(date>=yearmonth(POS_YCC),date<=origem) |> pull(fluxo) |> mean()
premissas <- tibble(date=datas,fluxo=media_fluxo,
  ois2=ifelse(datas<=yearmonth(snapshot),aux_satelite$ois2[match(datas,aux_satelite$date)],
    termo[match(datas,yearmonth(snapshot)+seq_len(n))]))

# 3. Variacao do spread prevista pelo satelite
novos <- aux_satelite |> filter(date<=origem) |> select(date,ois2,fluxo) |> bind_rows(premissas) |>
  mutate(ois_pos=100*(ois2-lag(ois2))*as.integer(date>=yearmonth(POS_YCC)),fluxo_l2=lag(fluxo,2)) |>
  filter(date %in% datas) |> as_tsibble(index=date)
stopifnot(all(is.finite(novos$ois_pos)),all(is.finite(novos$fluxo_l2)))
delta <- forecast(modelo_satelite,new_data=novos)$.mean/100   # pb -> p.p.

# 4. Tilting: alvo do spread = meses ja fixos no cenario + variacoes do satelite
resultados <- alvos <- list()
for(cen in names(trajetorias)) {
  F <- trajetorias[[cen]]; dimnames(F) <- list(variaveis,NULL,NULL)
  livres <- which(apply(F['inclinacao',,],1,sd)>0)
  alvo <- F['inclinacao',,1]
  for(t in livres) alvo[t] <- alvo[t-1]+delta[t]
  pesos <- list(`BVAR original`=rep(1/dim(F)[3],dim(F)[3]),
    `BVAR + satelite (tilting)`=tilting(t(F['inclinacao',livres,]),alvo[livres]))
  F[em_log,,] <- exp(F[em_log,,]/100)
  for(m in names(pesos)) resultados[[paste(cen,m)]] <-
    as_tibble(t(apply(F,c(1,2),function(x) sum(x*pesos[[m]])))) |>
    mutate(date=datas,cenario=cen,modelo=m,.before=1)
  alvos[[cen]] <- premissas |> mutate(cenario=cen,alvo_spread_pb=100*alvo,sorteios_efetivos=1/sum(pesos[[2]]^2))
}
tabela <- bind_rows(resultados) |> pivot_longer(all_of(variaveis),names_to='variavel',values_to='valor')

# Armazenamento da saida; resumo_forecast.csv e a interface para JGB2Y/spreads/curva
trabalho <- file.path(saida,'forecast')
dir.create(trabalho,recursive=TRUE,showWarnings=FALSE)
write_csv(tabela |> mutate(date=as.Date(date)),file.path(trabalho,'forecasts.csv'))
write_csv(bind_rows(alvos) |> mutate(date=as.Date(date)),file.path(trabalho,'premissas.csv'))
tabela |> filter(modelo=='BVAR + satelite (tilting)') |>
  transmute(date=as.Date(date),cenario,variavel,media=valor) |>
  write_csv(file.path(saida,'resumo_forecast.csv'))
