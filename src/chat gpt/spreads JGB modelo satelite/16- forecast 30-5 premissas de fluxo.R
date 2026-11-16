# Forecast do 30-5 (estagio 2 "OIS x YCC" do 14-) com PREMISSAS EXPLICITAS para a compra liquida
# estrangeira, no lugar do estagio 1 (fluxo ~ UST30). Reaproveita objetos do 14- (modelos e caminhos).
source('C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/14- forecast 30-5 com regime YCC.R')
m <- e2[['OIS x YCC']]
media_hist <- mean(dados$liq_estr,na.rm=TRUE)
ult12 <- mean(tail(dados$liq_estr,12)); m2426 <- mean(dados$liq_estr[dados$mes>=yearmonth('2024 Jan')])
n_fut <- length(cenarios[[1]]$mes)
premissas <- list(
  `Estagio 1 (UST30): ~0,64`=NULL,
  `Media 2024-26: 0,68`=rep(m2426,n_fut),
  `Ultimos 12m: 0,81`=rep(ult12,n_fut),
  `Aumenta: de 0,81 a 1,0`=seq(ult12,1.0,length.out=n_fut),
  `Volta a media historica: 0,37`=rep(media_hist,n_fut))
res <- imap_dfr(cenarios,function(x,cenario) imap_dfr(premissas,function(p,nome) {
  fluxo <- if(is.null(p)) forecast(e1,new_data=x)$.mean else p
  serie <- bind_rows(hist_liq,tibble(mes=x$mes,liq_estr=fluxo))
  x <- x |> mutate(liq_l2=serie$liq_estr[match(mes,serie$mes)-2L])
  sim <- generate(m,new_data=x,times=1000) |> as_tibble() |> arrange(.rep,mes) |>
    group_by(.rep) |> mutate(nivel=inicio+cumsum(.sim)) |> ungroup() |> filter(mes==max(mes))
  b <- tidy(m); bf <- b$estimate[b$term=='liq_l2']
  tibble(cenario=cenario,premissa=nome,fluxo_medio=mean(fluxo),
    nivel=inicio+sum(forecast(m,new_data=x)$.mean),p10=quantile(sim$nivel,.1),p90=quantile(sim$nivel,.9),
    contrib_fluxo=bf*sum(x$liq_l2)) }))
cat('\nInicio (set/2026):',round(inicio),'| BVAR Base set/2028:',round(last(bvar$nivel)),'\n')
print(res |> mutate(across(where(is.double),~round(.x,2))) |> arrange(cenario,desc(fluxo_medio)),n=Inf,width=Inf)
write_csv(res,file.path(trabalho,'forecast 30-5 premissas de fluxo.csv'))
