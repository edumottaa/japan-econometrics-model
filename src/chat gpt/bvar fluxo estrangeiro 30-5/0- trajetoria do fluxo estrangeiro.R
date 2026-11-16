# Entrada do BVAR com fluxo: compra liquida estrangeira de superlongos (JSDA, tri ienes/mes)
# historica e projetada pelo estagio 1 do satelite (fluxo ~ UST30 nivel lag 2, UST30 a termo),
# e o coeficiente do fluxo no spread 30-5 do modelo satelite final (21-), em p.p. por tri ienes.
source('C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/14- forecast 30-5 com regime YCC.R')
dados <- dados |> mutate(ois_pos=d_ois2*(1-ycc))
final_m <- dados |> mutate(d_s30_5=if_else(row_number()<=K,NA_real_,d_s30_5)) |>
  model(a=ARIMA(d_s30_5 ~ ois_pos + liq_l2 + pdq(p=0:3,d=0,q=0:3) + PDQ(0,0,0) + 1,stepwise=FALSE,approximation=FALSE))
beta_bp <- tidy(final_m) |> filter(term=='liq_l2') |> pull(estimate)
x <- cenarios[['Trajetoria do mercado']]
fluxo <- bind_rows(hist_liq |> filter(is.finite(liq_estr)) |> mutate(tipo='historico'),
  tibble(mes=x$mes,liq_estr=forecast(e1,new_data=x)$.mean,tipo='projecao'))
pasta_bvar <- file.path(raiz,'src/chat gpt/bvar fluxo estrangeiro 30-5')
write_csv(fluxo |> mutate(mes=as.Date(mes)),file.path(pasta_bvar,'dados/fluxo estrangeiro.csv'))
write_csv(tibble(beta_bp=beta_bp,beta_pp=beta_bp/100,media_fluxo=mean(fluxo$liq_estr[fluxo$tipo=='historico'])),
  file.path(pasta_bvar,'dados/coeficiente fluxo.csv'))
print(range(fluxo$mes)); cat('beta (pb por tri ienes):',beta_bp,'\n')
