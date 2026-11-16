Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/resultados'
selecao <- readRDS(file.path(trabalho,'selecao.rds'))

# 1. Regressao conjunta com as defasagens selecionadas e AR/MA por AICc.
# Mesma amostra efetiva da selecao: os K primeiros alvos ficam fora.
modelos <- imap(selecao$formulas,function(formula,alvo) {
  lag_data <- selecao$dados |> mutate(!!alvo:=if_else(row_number()<=selecao$K,NA_real_,.data[[alvo]]))
  m <- lag_data |> model(arima=ARIMA(!!formula,stepwise=FALSE,approximation=FALSE))
  stopifnot(!any(vapply(m$arima,inherits,logical(1),'null_mdl')))
  cat('=====',alvo,'\n'); report(m); m
})

# 2. Diagnostico das inovacoes no proprio script do modelo.
diagnostico <- imap_dfr(modelos,function(m,alvo) {
  residuos <- augment(m) |> filter(is.finite(.innov))
  dof <- sum(str_detect(tidy(m)$term,'^(ar|ma|sar|sma)[0-9]+$'))
  map_dfr(c(12L,24L),function(k) residuos |>
    features(.innov,ljung_box,lag=k,dof=dof) |> mutate(alvo=alvo,lag_teste=k,dof=dof))
})
print(diagnostico)
coeficientes <- imap_dfr(modelos,~ tidy(.x) |> mutate(alvo=.y,.before=1))
ajuste <- imap_dfr(modelos,~ glance(.x) |> mutate(alvo=.y,.before=1) |> select(-ar_roots,-ma_roots))
print(coeficientes,n=Inf)

# Armazenamento da saida em outputs.
saveRDS(modelos,file.path(trabalho,'modelos ARIMA.rds'))
write_csv(coeficientes,file.path(trabalho,'coeficientes ARIMA.csv'))
write_csv(ajuste,file.path(trabalho,'ajuste ARIMA.csv'))
write_csv(diagnostico,file.path(trabalho,'diagnostico ARIMA.csv'))
iwalk(modelos,function(m,alvo) {
  ggsave(file.path(trabalho,paste0('residuos ARIMA ',alvo,'.png')),gg_tsresiduals(m),
    width=11,height=7,dpi=160,bg='white')
  capture.output(report(m),file=file.path(trabalho,paste0('especificacao ARIMA ',alvo,'.txt')))
})
cat('MODELO_CONCLUIDO\n')
