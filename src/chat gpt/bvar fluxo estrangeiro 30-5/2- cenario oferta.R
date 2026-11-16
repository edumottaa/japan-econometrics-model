Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse));suppressPackageStartupMessages(library(fpp3))
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/jgb30y_5y com tendencia'
saida <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/bvar fluxo estrangeiro 30-5/resultados'
dir.create(saida, recursive = TRUE, showWarnings = FALSE)

# 1. Separar caminho base e os outros cenários
caminhos <- readRDS(file.path(raiz,'src/Outputs/4- Forecast/choque unico Treasury/producao/caminhos_novos.rds'))
# Estender ate setembro/2028 com as curvas ja disponiveis e o mesmo ARIMA da IP.
fim_forecast <- yearmonth('2028 Sep')
if (max(caminhos$date) < fim_forecast) {
  source('src/7- forecast/scenarios brent/codigos/1- construir cenarios brent.R',
         local=TRUE,encoding='UTF-8')
  source('src/7- forecast/scenarios ust2y/codigos/1- preparar treasury2y.R',
         local=TRUE,encoding='UTF-8')
  modelo_ip <- readRDS(file.path(raiz,'src/Outputs/4- Forecast/producao mundial/modelo producao ARIMA.rds'))
  adicionais <- list()
  for (cen in unique(caminhos$cenario)) {
    antigo <- caminhos |> filter(cenario==cen) |> arrange(date)
    novos_meses <- seq(max(antigo$date)+1,fim_forecast,by=1)
    novo <- antigo[rep(nrow(antigo),length(novos_meses)),]
    novo$date <- novos_meses
    novo$brent <- 100*log(cenarios_brent$preco[match(
      paste(novos_meses,cen),paste(cenarios_brent$date,cenarios_brent$cenario))])
    novo$ust2y <- caminho_ust2y$ust2y[match(novos_meses,caminho_ust2y$date)]
    novo[,c('atividade','cpi_core','juro_curto','inclinacao','usdjpy')] <- NA_real_
    # O forecast antigo guarda os preditores dos meses que ficaram livres no BVAR.
    preditores <- readRDS(file.path(raiz,'src/Outputs/4- Forecast/choque unico Treasury/producao',
      paste0('producao ARIMA ',cen,'.rds'))) |>
      as_tibble() |> select(date,ust2y,brent)
    futuro_ip <- bind_rows(preditores,select(novo,date,ust2y,brent)) |> as_tsibble(index=date)
    stopifnot(all(is.finite(futuro_ip$ust2y)),all(is.finite(futuro_ip$brent)))
    ip <- forecast(modelo_ip,new_data=futuro_ip)
    novo$ip_mundial <- ip$.mean[match(novos_meses,ip$date)]
    adicionais[[cen]] <- novo
  }
  caminhos <- bind_rows(caminhos,bind_rows(adicionais)) |> arrange(cenario,date)
}
caminhos <- caminhos |> filter(date<=fim_forecast)
stopifnot(max(caminhos$date)==fim_forecast)
# A condição observada da inclinação deve usar 30Y−5Y, como a estimação atual.
spread <- read_csv(file.path(raiz,'data/processed/jgb30y_5y_mof_mensal.csv'),
                   show_col_types=FALSE) |>
  transmute(date=yearmonth(date), observado_30_5=inclinacao_30_5)
caminhos <- caminhos |> left_join(spread,by='date') |>
  mutate(inclinacao=if_else(is.finite(inclinacao),observado_30_5,NA_real_)) |>
  select(-observado_30_5)
stopifnot(all(is.finite(caminhos$inclinacao[caminhos$date==min(caminhos$date)])))
base <- caminhos |> filter(cenario=='Base') |> arrange(date)
h <- nrow(base)

# 2. Selecionar o valor das IRFs do choque de oferta e adapta-las aos cenários divergentes do base;
#  Premissa: cada cenário apresenta um choque de oferta diferente
#  Calculo da sequência de choques de oferta necessária para reproduzir o caminho do Brent de cada cenário

irfs <- readRDS(file.path(raiz,'src/chat gpt/bvar fluxo estrangeiro 30-5/modelo/irfs.rds'))
stopifnot(dim(irfs)[3]>=h)
irf <- apply(irfs[,2,,],c(1,2),median)
H <- matrix(0,h,h)
for(t in seq_len(h)) for(j in seq_len(t)) H[t,j] <- irf[2,t-j+1]
choques <- respostas <- diagnosticos <- list()
for(cen in unique(caminhos$cenario)) {
  c <- caminhos |> filter(cenario==cen) |> arrange(date)
  delta <- c$brent-base$brent;delta[is.na(delta)]<-0
  choques[[cen]] <- drop(forwardsolve(H,delta))
  resposta <- matrix(0,h,8,dimnames=list(NULL,c('ust2y','brent','ip_mundial',
    'atividade','cpi_core','juro_curto','inclinacao','usdjpy')))
  for(t in seq_len(h)) for(j in seq_len(t))
    resposta[t,] <- resposta[t,]+irf[,t-j+1]*choques[[cen]][j]
  stopifnot(max(abs(resposta[,'brent']-delta))<1e-8,all(is.finite(resposta)))
  respostas[[cen]] <- resposta
  diagnosticos[[cen]] <- tibble(cenario=cen,erro_brent=max(abs(resposta[,'brent']-delta)),
    maior_choque=max(abs(choques[[cen]])),meses_com_choque=sum(abs(choques[[cen]])>1e-10),
    efeito_final_atividade_pct=100*expm1(tail(resposta[,'atividade'],1)/100))
}



# Armazenamento da saida em outputs
saveRDS(caminhos,file.path(saida,'caminhos_novos.rds'))
saveRDS(choques,file.path(saida,'choques oferta.rds'))
saveRDS(respostas,file.path(saida,'resposta oferta.rds'))
write_csv(bind_rows(diagnosticos),file.path(saida,'diagnosticos cenarios.csv'))
write_csv(map_dfr(names(choques),~ tibble(cenario=.x,date=as.Date(base$date),choque_oferta=choques[[.x]])),
  file.path(saida,'choques oferta.csv'))
write_csv(map_dfr(names(respostas),~ bind_cols(tibble(cenario=.x,date=as.Date(base$date)),as_tibble(respostas[[.x]]))),
  file.path(saida,'efeitos oferta.csv'))
print(bind_rows(diagnosticos))
