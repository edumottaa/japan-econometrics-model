Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse));suppressPackageStartupMessages(library(fpp3))
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/cenario oferta estrutural central'
saida <- file.path(trabalho,'producao')

# 1. Mesmos caminhos anteriores; mudar apenas o mecanismo do adverso estrutural.
caminhos <- readRDS(file.path(raiz,'src/Outputs/4- Forecast/choque unico Treasury/producao/caminhos_novos.rds'))
base <- caminhos |> filter(cenario=='Base') |> arrange(date)
adverso <- caminhos |> filter(cenario=='Adverso estrutural') |> arrange(date)
delta <- adverso$brent-base$brent;delta[is.na(delta)]<-0
h <- length(delta)

# 2. Calibrar apenas choques de oferta, considerando seus efeitos anteriores.
# IRF central: mediana de TODOS os 500 sorteios; nenhum filtro ou reestimacao.
irfs <- readRDS(file.path(raiz,'data/processed/cenarios_propios_20261104/modelo_producao/irfs.rds'))
irf <- apply(irfs[,2,,],c(1,2),median)
H <- matrix(0,h,h)
for(t in seq_len(h)) for(j in seq_len(t)) H[t,j] <- irf[2,t-j+1]
choques <- drop(forwardsolve(H,delta))
resposta <- matrix(0,h,8,dimnames=list(NULL,c('ust2y','brent','ip_mundial',
  'atividade','cpi_core','juro_curto','inclinacao','usdjpy')))
for(t in seq_len(h)) for(j in seq_len(t))
  resposta[t,] <- resposta[t,]+irf[,t-j+1]*choques[j]
stopifnot(max(abs(resposta[,'brent']-delta))<1e-8,all(is.finite(resposta)))

# Armazenamento da saida em outputs.
saveRDS(caminhos,file.path(saida,'caminhos_novos.rds'))
saveRDS(choques,file.path(saida,'choques oferta.rds'))
saveRDS(resposta,file.path(saida,'resposta oferta.rds'))
write_csv(tibble(date=as.Date(base$date),choque_oferta=choques,
  erro_brent=resposta[,'brent']-delta),file.path(saida,'choques oferta.csv'))
write_csv(bind_cols(tibble(date=as.Date(base$date)),as_tibble(resposta)),
  file.path(saida,'efeitos oferta.csv'))
print(tibble(rcond=rcond(H),erro_brent=max(abs(resposta[,'brent']-delta)),
  maior_choque=max(abs(choques)),meses_com_choque=sum(abs(choques)>1e-10),
  efeito_final_atividade_pct=100*expm1(tail(resposta[,'atividade'],1)/100)))
