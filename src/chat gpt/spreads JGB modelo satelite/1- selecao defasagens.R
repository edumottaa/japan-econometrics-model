Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/resultados'
entrada <- readRDS(file.path(trabalho,'dados.rds'))
dados <- entrada$dados; alvos <- entrada$alvos; preditores <- entrada$preditores

# 1. Diferenciacao: niveis devem pedir d=1 (por isso o modelo usa variacoes);
# as variacoes devem pedir d=0. A compra liquida ja e fluxo e entra em nivel.
niveis <- c(s30_10='d_s30_10',s30_5='d_s30_5',ust30='d_ust30',ois2='d_ois2',
  usdjpy='d_usdjpy',liq_estr='liq_estr',fatia_estr='d_fatia')
testes_d <- map_dfr(names(niveis),function(x) tibble(nivel=x,variacao=niveis[[x]],
  ndiffs_nivel=unitroot_ndiffs(if(x=='usdjpy') log(dados[[x]]) else dados[[x]])[[1]],
  ndiffs_variacao=unitroot_ndiffs(dados[[niveis[[x]]]])[[1]],
  kpss_pvalor_variacao=unitroot_kpss(dados[[niveis[[x]]]])[['kpss_pvalue']]))
testes_D <- map_dfr(unname(niveis),function(x)
  tibble(variavel=x,nsdiffs=unitroot_nsdiffs(dados[[x]])[[1]]))
# Diferente da producao mundial: d vem so dos alvos. A variacao do OIS2Y rejeita o KPSS
# pela troca de regime (juro zero ate 2023, altas depois), nao por raiz unitaria nas
# variacoes; d=1 no erro diferenciaria duas vezes os spreads (sobrediferenciacao).
# O fluxo estrangeiro em nivel tambem rejeita o KPSS (compras maiores em 2025-26).
d <- max(testes_d$ndiffs_variacao[testes_d$variacao %in% alvos])
stopifnot(d==0L)
print(testes_d); print(testes_D)
# Sem termos sazonais: juros, cambio e fluxos de mercado nao tem calendario fixo.

# 2. FPP3 (lagged predictors): familia A contemporanea + lags; familia B apenas lags.
# Constante OBRIGATORIA na estimacao ('+ 1'): sem ela, o fluxo em nivel (media +0,37 tri)
# absorve a tendencia de inclinacao de 2018-26 e o beta do fluxo sai viesado. Na projecao
# ela sai, como a tendencia no BVAR: com variacoes, viraria tendencia do spread.
# Lags curtos: variacoes mensais de mercado repassam rapido para o spread.
specs <- list(A_lag0=0,A_lag01=c(0,1),A_lag012=c(0,1,2),
  B_1=1,B_2=2,B_12=c(1,2),B_3=3)
K <- max(unlist(specs))
termos <- function(x,lags) paste(ifelse(lags==0,x,paste0('lag(',x,',',lags,')')),collapse=' + ')
testa_lags <- function(alvo,x) {
  # Mesma amostra efetiva para todas as especificacoes: descartar os K primeiros alvos.
  lag_data <- dados |> mutate(y=if_else(row_number()<=K,NA_real_,.data[[alvo]]))
  map_dfr(names(specs),function(nome) {
    formula <- as.formula(paste('y ~',termos(x,specs[[nome]]),
      '+ pdq(p=0:3,d=',d,',q=0:3) + PDQ(0,0,0) + 1'))
    ajuste <- lag_data |> model(arima=ARIMA(!!formula,stepwise=FALSE,approximation=FALSE))
    cat('LAGS',alvo,x,nome,'\n'); flush.console()
    glance(ajuste) |> transmute(alvo=alvo,regressor=x,spec=nome,n_termos=length(specs[[nome]]),
      menor_lag=min(specs[[nome]]),AICc,BIC)
  }) |> filter(is.finite(AICc)) |> arrange(AICc) |>
    mutate(delta_AICc=AICc-min(AICc),delta_BIC=BIC-min(BIC))
}
testes <- expand_grid(alvo=alvos,regressor=preditores) |>
  pmap_dfr(function(alvo,regressor) testa_lags(alvo,regressor))
# Regra EUA: delta AICc < 2, menos termos, maior menor lag, menor AICc.
escolhas <- testes |> filter(delta_AICc<2) |> group_by(alvo,regressor) |>
  arrange(n_termos,desc(menor_lag),AICc,.by_group=TRUE) |> slice_head(n=1) |> ungroup()
stopifnot(nrow(escolhas)==length(alvos)*length(preditores))
formulas <- map(set_names(alvos),function(a) {
  e <- escolhas |> filter(alvo==a)
  as.formula(paste(a,'~',paste(map2_chr(e$regressor,e$spec,~ termos(.x,specs[[.y]])),collapse=' + '),
    '+ pdq(p=0:3,d=',d,',q=0:3) + PDQ(0,0,0) + 1'))
})

# Armazenamento da saida em outputs.
write_csv(testes_d,file.path(trabalho,'testes diferenciacao.csv'))
write_csv(testes_D,file.path(trabalho,'testes sazonalidade.csv'))
write_csv(testes,file.path(trabalho,'teste defasagens.csv'))
write_csv(escolhas,file.path(trabalho,'escolhas defasagens.csv'))
saveRDS(list(dados=dados,formulas=formulas,escolhas=escolhas,specs=specs,d=d,K=K),
  file.path(trabalho,'selecao.rds'))
print(escolhas,n=Inf); print(formulas)
cat('SELECAO_CONCLUIDA\n')
