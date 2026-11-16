Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/resultados'
dados <- readRDS(file.path(trabalho,'dados.rds'))$dados; K <- 3L; H <- 12L

# Opcao 1 precisa de dummy do YCC? Compara fora da amostra, so no regime pos-YCC (onde o forecast vive):
#   Sem dummy        : d_s30_5 ~ OIS + fluxo(lag 2) + constante + AR(1)
#   OIS x YCC        : + OIS x dummy (efeito do OIS diferente no YCC)
#   OIS x YCC + nivel: + dummy de nivel (constante propria pos-YCC)
# Origens de out/2024 (6 meses pos-YCC para estimar a interacao) ate ago/2026; horizonte ate 12m
# (menos origens nos horizontes longos). Ex post: OIS e fluxo REALIZADOS (o fluxo e premissa).
dados <- dados |> mutate(liq_l2=lag(liq_estr,2),ycc=as.numeric(mes<=yearmonth('2024 Mar')),ois_x_ycc=d_ois2*ycc)
versoes <- list(`Sem dummy`=c('d_ois2','liq_l2','pdq(1,0,0)'),
  `OIS x YCC`=c('d_ois2','ois_x_ycc','liq_l2','pdq(0,0,0)'),
  `OIS x YCC + dummy de nivel`=c('d_ois2','ois_x_ycc','liq_l2','ycc','pdq(0,0,0)'))
origens <- seq(yearmonth('2024 Oct'),max(dados$mes)-1L,by=1)
cv <- map_dfr(origens,function(o) {
  treino <- dados |> filter(mes<=o) |> mutate(d_s30_5=if_else(row_number()<=K,NA_real_,d_s30_5))
  futuro <- dados |> filter(mes>o,mes<=o+H)
  imap_dfr(versoes,function(r,nome) {
    m <- treino |> model(a=ARIMA(!!as.formula(paste('d_s30_5 ~',paste(r,collapse=' + '),'+ PDQ(0,0,0) + 1'))))
    tibble(origem=o,modelo=nome,h=seq_len(nrow(futuro)),real=futuro$d_s30_5,previsto=forecast(m,new_data=futuro)$.mean) }) |>
    bind_rows(tibble(origem=o,modelo='Variacao zero',h=seq_len(nrow(futuro)),real=futuro$d_s30_5,previsto=0)) }) |>
  group_by(origem,modelo) |> mutate(erro=cumsum(real)-cumsum(previsto),vies=-erro) |> ungroup()
met <- cv |> filter(h %in% c(1,3,6,12)) |> group_by(modelo,h) |>
  summarise(RMSE=sqrt(mean(erro^2)),vies=mean(vies),n=n(),.groups='drop')
print(met |> mutate(across(c(RMSE,vies),~round(.x,1))) |> pivot_wider(names_from=h,values_from=c(RMSE,vies,n)),width=Inf)
write_csv(met,file.path(trabalho,'cv dummy YCC 30-5.csv'))
