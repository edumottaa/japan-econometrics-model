Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse));suppressPackageStartupMessages(library(fpp3))
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/sensibilidade pesos/rho_0980'
saida <- trabalho
dir.create(saida, recursive = TRUE, showWarnings = FALSE)

# 1. Separar caminho base e os outros cenários
caminhos <- readRDS(file.path(raiz,'src/Outputs/4- Forecast/choque unico Treasury/producao/caminhos_novos.rds'))
base <- caminhos |> filter(cenario=='Base') |> arrange(date)
h <- nrow(base)

# 2. Selecionar o valor das IRFs do choque de oferta e adapta-las aos cenários divergentes do base;
#  Premissa: cada cenário apresenta um choque de oferta diferente
#  Calculo da sequência de choques de oferta necessária para reproduzir o caminho do Brent de cada cenário

irfs <- readRDS(file.path(raiz,'data/processed/sensibilidade_pesos_sem_tendencia/rho_0980/irfs.rds'))
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

