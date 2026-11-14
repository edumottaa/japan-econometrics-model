Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/choque unico Treasury'
fonte <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/data/processed/cenarios_propios_20261104/modelo_producao'
dados <- readRDS(file.path(fonte,'dados.rds')) |>
  as_tibble() |> select(date,ip_mundial,ust2y,brent) |> as_tsibble(index=date)

# 1. Diferenciacao: mesmo diagnostico do codigo EUA, antes de comparar os lags.
# IP e Brent em 100*log; Treasury em % a.a. Diferenciar conjuntamente pelo maior d.
testes_d <- map_dfr(c('ip_mundial','ust2y','brent'),function(x)
  tibble(variavel=x,ndiffs=feasts::unitroot_ndiffs(dados[[x]])[[1]]))
testes_D <- map_dfr(c('ip_mundial','ust2y','brent'),function(x)
  tibble(variavel=x,nsdiffs=feasts::unitroot_nsdiffs(dados[[x]])[[1]]))
d <- max(testes_d$ndiffs)
stopifnot(d %in% 0:2)
print(testes_d); print(testes_D)
# Sem termos sazonais: IP e ajustada sazonalmente; juros e Brent nao tem calendario fixo.

# 2. FPP3/EUA: familia A contemporanea + lags; familia B apenas lags.
# Mesmas especificacoes mensais ja usadas no teste de lags do S2.
specs <- list(A_lag0=0,A_lag03=c(0,3),A_lag036=c(0,3,6),
  B_1=1,B_3=3,B_36=c(3,6),B_6=6,B_69=c(6,9),B_9=9,B_12=12)
K <- max(unlist(specs))
lag_data <- dados |> mutate(y=if_else(row_number()<=K,NA_real_,ip_mundial))
termos <- function(x,lags) paste(ifelse(lags==0,x,paste0('lag(',x,',',lags,')')),collapse=' + ')
testa_lags <- function(x) {
  map_dfr(names(specs),function(nome) {
    formula <- as.formula(paste('y ~',termos(x,specs[[nome]]),
      '+ pdq(p=0:3,d=',d,',q=0:3) + PDQ(0,0,0)'))
    ajuste <- lag_data |> model(arima=ARIMA(!!formula,stepwise=FALSE,approximation=FALSE))
    cat('LAGS',x,nome,'\n'); flush.console()
    glance(ajuste) |> transmute(regressor=x,spec=nome,n_termos=length(specs[[nome]]),
      menor_lag=min(specs[[nome]]),AICc,BIC)
  }) |> filter(is.finite(AICc)) |> arrange(AICc) |>
    mutate(delta_AICc=AICc-min(AICc),delta_BIC=BIC-min(BIC))
}
testes <- map_dfr(c('ust2y','brent'),testa_lags)
# Regra EUA: delta AICc < 2, menos termos, maior menor lag, menor AICc.
escolhas <- testes |> filter(delta_AICc<2) |> group_by(regressor) |>
  arrange(n_termos,desc(menor_lag),AICc,.by_group=TRUE) |> slice_head(n=1) |> ungroup()
stopifnot(nrow(escolhas)==2L)
formula_final <- as.formula(paste('ip_mundial ~',paste(map2_chr(escolhas$regressor,
  escolhas$spec,~ termos(.x,specs[[.y]])),collapse=' + '),
  '+ pdq(p=0:3,d=',d,',q=0:3) + PDQ(0,0,0)'))

# Armazenamento da saida em outputs.
write_csv(testes_d,file.path(trabalho,'testes diferenciacao.csv'))
write_csv(testes_D,file.path(trabalho,'testes sazonalidade.csv'))
write_csv(testes,file.path(trabalho,'teste defasagens producao.csv'))
write_csv(escolhas,file.path(trabalho,'escolhas defasagens producao.csv'))
saveRDS(list(dados=dados,formula=formula_final,escolhas=escolhas,d=d,K=K),
  file.path(trabalho,'selecao producao.rds'))
print(escolhas);print(formula_final)
cat('SELECAO_IP_CONCLUIDA\n')
