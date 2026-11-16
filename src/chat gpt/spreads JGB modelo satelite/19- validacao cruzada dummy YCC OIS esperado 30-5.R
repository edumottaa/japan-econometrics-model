Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/resultados'
dados <- readRDS(file.path(trabalho,'dados.rds'))$dados; K <- 3L; H <- 12L

# Igual ao 17-, mas com os insumos PROJETADOS em cada origem (ex ante):
#   Fluxo: premissa "continua" = media dos ultimos 12 meses ate a origem (constante no horizonte).
#   OIS 2Y: (a) realizado; (b) sem variacao; (c) ESPERADO pelo mercado na origem: overnight sai da
#   call rate r0 e sobe em linha reta ate r24 = 2*OIS2Y - r0 (o OIS 2Y e a media do overnight
#   esperado em 24 meses); depois fica em r24. OIS 2Y esperado em h = media do overnight de h a h+23.
# Os 2 primeiros meses do horizonte usam o fluxo ja observado (lag 2).
dados <- dados |> mutate(ycc=as.numeric(mes<=yearmonth('2024 Mar')),ois_x_ycc=d_ois2*ycc)
# OIS 2Y esperado na origem o, para os h meses seguintes (variacao mensal em pb).
base_niveis <- readRDS(file.path(trabalho,'dados.rds'))$base |> as_tibble() |> select(mes,ois2,call_jp)
ois_esperado <- function(o,h) {
  b <- base_niveis |> filter(mes==o); r0 <- b$call_jp; s <- b$ois2; r24 <- 2*s-r0
  caminho <- c(r0+(r24-r0)*(0:24)/24,rep(r24,h+24))
  esperado <- map_dbl(0:h,~ mean(caminho[(.x+1):(.x+24)]))
  esperado <- s+esperado-esperado[1]
  100*diff(esperado) }
versoes <- list(`Sem dummy`=c('d_ois2','liq_l2','pdq(1,0,0)'),
  `OIS x YCC`=c('d_ois2','ois_x_ycc','liq_l2','pdq(0,0,0)'),
  `OIS x YCC + dummy de nivel`=c('d_ois2','ois_x_ycc','liq_l2','ycc','pdq(0,0,0)'))
origens <- seq(yearmonth('2024 Oct'),max(dados$mes)-1L,by=1)
cv <- map_dfr(origens,function(o) {
  hist <- dados |> filter(mes<=o)
  treino <- hist |> mutate(liq_l2=lag(liq_estr,2),d_s30_5=if_else(row_number()<=K,NA_real_,d_s30_5))
  futuro <- dados |> filter(mes>o,mes<=o+H)
  premissa <- mean(tail(hist$liq_estr,12))
  serie <- c(hist$liq_estr,rep(premissa,nrow(futuro))); n0 <- nrow(hist)
  base_fut <- futuro |> mutate(liq_l2=serie[n0+row_number()-2L])
  insumos <- list(`Fluxo projetado, OIS realizado`=base_fut,
    `Fluxo projetado, OIS sem variacao`=base_fut |> mutate(d_ois2=0,ois_x_ycc=0),
    `Fluxo projetado, OIS esperado (call + 2Y spot)`=base_fut |> mutate(d_ois2=ois_esperado(o,nrow(futuro)),ois_x_ycc=0))
  map_dfr(names(versoes),function(nome) {
    m <- treino |> model(a=ARIMA(!!as.formula(paste('d_s30_5 ~',paste(versoes[[nome]],collapse=' + '),'+ PDQ(0,0,0) + 1'))))
    imap_dfr(insumos,~ tibble(origem=o,modelo=nome,insumo=.y,h=seq_len(nrow(futuro)),real=futuro$d_s30_5,
      previsto=forecast(m,new_data=.x)$.mean)) }) |>
    bind_rows(tibble(origem=o,modelo='Variacao zero',insumo=names(insumos)[1],h=seq_len(nrow(futuro)),real=futuro$d_s30_5,previsto=0)) }) |>
  group_by(origem,modelo,insumo) |> mutate(erro=cumsum(real)-cumsum(previsto)) |> ungroup()
met <- cv |> filter(h %in% c(3,6,12)) |> group_by(insumo,modelo,h) |>
  summarise(RMSE=sqrt(mean(erro^2)),vies=-mean(erro),.groups='drop')
print(met |> mutate(across(c(RMSE,vies),~round(.x,1))) |> pivot_wider(names_from=h,values_from=c(RMSE,vies)) |> arrange(insumo,RMSE_12),width=Inf)
write_csv(met,file.path(trabalho,'cv dummy YCC ex ante OIS esperado 30-5.csv'))
