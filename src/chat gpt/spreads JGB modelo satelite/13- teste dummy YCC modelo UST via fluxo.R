Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/resultados'
dados <- readRDS(file.path(trabalho,'dados.rds'))$dados; K <- 3L

# Teste de regime no modelo "UST so via fluxo" (10-):
#   Estagio 1: liq_estr ~ UST30 (lag 2) + erros MA(2), sem constante
#   Estagio 2: d_s30_5 ~ OIS2Y + compra (lag 2) + constante + erros AR(1)
# Ordens ARIMA fixas para o AICc comparar so os regressores (mesma amostra em cada bloco).
# Dummies (1 = regime de controle da curva):
#   ycc_2024: jun/2018 a mar/2024 (fim do YCC em mar/2024)
#   ycc_2022: jun/2018 a dez/2022 (antes do 1o alargamento da banda, dez/2022)
# Interacao = dummy x regressor: testa se o coeficiente muda no regime do YCC.
dados <- dados |> mutate(ust30_l2=lag(ust30,2),liq_l2=lag(liq_estr,2),
  ycc_2024=as.numeric(mes<=yearmonth('2024 Mar')),ycc_2022=as.numeric(mes<=yearmonth('2022 Dec')),
  ust_x_ycc_2024=ust30_l2*ycc_2024,ust_x_ycc_2022=ust30_l2*ycc_2022,
  liq_x_ycc_2024=liq_l2*ycc_2024,liq_x_ycc_2022=liq_l2*ycc_2022,
  ois_x_ycc_2024=d_ois2*ycc_2024,ois_x_ycc_2022=d_ois2*ycc_2022)

ajusta <- function(d,alvo,regs,ordem,const) {
  f <- as.formula(paste(alvo,'~',paste(regs,collapse=' + '),'+',ordem,'+ PDQ(0,0,0)',if(const) '+ 1' else '+ 0'))
  d |> mutate(!!alvo:=if_else(row_number()<=K,NA_real_,.data[[alvo]])) |> model(a=ARIMA(!!f)) }
resumo <- function(m,nome,foco) { t <- tidy(m)
  tibble(modelo=nome,AICc=glance(m)$AICc,
    coefs=paste(sprintf('%s=%.3g (p=%.2g)',t$term,t$estimate,t$p.value)[t$term %in% foco],collapse='; ')) }

# 1. Estagio 1 (fluxo).
e1 <- list(
  `Base`=c('ust30_l2'),
  `+ dummy YCC ate mar/2024`=c('ust30_l2','ycc_2024'),
  `+ dummy YCC ate dez/2022`=c('ust30_l2','ycc_2022'),
  `+ dummy e interacao (mar/2024)`=c('ust30_l2','ycc_2024','ust_x_ycc_2024'),
  `+ dummy e interacao (dez/2022)`=c('ust30_l2','ycc_2022','ust_x_ycc_2022'))
t1 <- imap_dfr(e1,~ resumo(ajusta(dados,'liq_estr',.x,'pdq(0,0,2)',FALSE),.y,c(.x,'intercept'))) |>
  mutate(ganho_AICc=first(AICc)-AICc,.after=AICc)
# Com dummy a constante faz sentido (nivel diferente fora do YCC): versao com constante.
t1c <- imap_dfr(e1,~ resumo(ajusta(dados,'liq_estr',.x,'pdq(0,0,2)',TRUE),paste(.y,'| com constante'),c(.x,'intercept')))
cat('\n===== Estagio 1: compra liquida estrangeira\n'); print(bind_rows(t1,t1c |> mutate(ganho_AICc=first(t1$AICc)-AICc)),n=Inf,width=Inf)

# 2. Estagio 2 (spread 30-5).
e2 <- list(
  `Base`=c('d_ois2','liq_l2'),
  `+ dummy YCC ate mar/2024`=c('d_ois2','liq_l2','ycc_2024'),
  `+ dummy YCC ate dez/2022`=c('d_ois2','liq_l2','ycc_2022'),
  `+ dummy e interacao fluxo (mar/2024)`=c('d_ois2','liq_l2','ycc_2024','liq_x_ycc_2024'),
  `+ dummy e interacao fluxo (dez/2022)`=c('d_ois2','liq_l2','ycc_2022','liq_x_ycc_2022'),
  `+ interacoes fluxo e OIS (mar/2024)`=c('d_ois2','liq_l2','ycc_2024','liq_x_ycc_2024','ois_x_ycc_2024'))
t2 <- imap_dfr(e2,~ resumo(ajusta(dados,'d_s30_5',.x,'pdq(1,0,0)',TRUE),.y,c(.x,'intercept'))) |>
  mutate(ganho_AICc=first(AICc)-AICc,.after=AICc)
cat('\n===== Estagio 2: spread 30-5\n'); print(t2,n=Inf,width=Inf)

# 3. Subamostras: coeficientes mantem sinal? (ordens fixas; AICc nao comparavel entre amostras)
sub <- list(`Completa (jun/2018-set/2026)`=yearmonth('2018 Jun'),`Desde jan/2022`=yearmonth('2022 Jan'),
  `Desde abr/2024 (pos-YCC)`=yearmonth('2024 Apr'))
t3 <- imap_dfr(sub,function(ini,nome) { d <- dados |> filter(mes>=ini)
  bind_rows(resumo(ajusta(d,'liq_estr','ust30_l2','pdq(0,0,2)',FALSE),paste('Estagio 1 |',nome),'ust30_l2'),
    resumo(ajusta(d,'liq_estr','ust30_l2','pdq(0,0,2)',TRUE),paste('Estagio 1 com constante |',nome),c('ust30_l2','intercept')),
    resumo(ajusta(d,'d_s30_5',c('d_ois2','liq_l2'),'pdq(1,0,0)',TRUE),paste('Estagio 2 |',nome),c('d_ois2','liq_l2','intercept'))) |>
    mutate(n=nrow(d)-K) })
cat('\n===== Subamostras\n'); print(t3 |> arrange(modelo),n=Inf,width=Inf)

write_csv(bind_rows(mutate(t1,estagio=1),mutate(t1c,estagio=1),mutate(t2,estagio=2)),file.path(trabalho,'teste dummy YCC.csv'))
write_csv(t3,file.path(trabalho,'teste subamostras YCC.csv'))
cat('CONCLUIDO\n')
