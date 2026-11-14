Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/choque unico Treasury'
selecao <- readRDS(file.path(trabalho,'selecao producao.rds'))

# 1. Regressao conjunta com as defasagens selecionadas e AR/MA por AICc.
formula <- selecao$formula
modelo_ip <- selecao$dados |> model(arima=ARIMA(!!formula,stepwise=FALSE,approximation=FALSE))
stopifnot(!any(vapply(modelo_ip$arima,inherits,logical(1),'null_mdl')))
report(modelo_ip)

# 2. Diagnostico das inovacoes no proprio script do modelo.
residuos <- augment(modelo_ip) |> filter(is.finite(.innov))
dof <- sum(str_detect(tidy(modelo_ip)$term,'^(ar|ma|sar|sma)[0-9]+$'))
diagnostico <- map_dfr(c(12L,24L),function(k) residuos |>
  features(.innov,ljung_box,lag=k,dof=dof) |> mutate(lag_teste=k,dof=dof))
print(diagnostico)

# Armazenamento da saida em outputs.
saveRDS(modelo_ip,file.path(trabalho,'modelo producao ARIMA.rds'))
write_csv(tidy(modelo_ip),file.path(trabalho,'coeficientes producao ARIMA.csv'))
write_csv(glance(modelo_ip),file.path(trabalho,'ajuste producao ARIMA.csv'))
write_csv(diagnostico,file.path(trabalho,'diagnostico producao ARIMA.csv'))
ggsave(file.path(trabalho,'residuos producao ARIMA.png'),gg_tsresiduals(modelo_ip),
  width=11,height=7,dpi=160,bg='white')
capture.output(report(modelo_ip),file=file.path(trabalho,'especificacao producao ARIMA.txt'))
cat('MODELO_IP_CONCLUIDO\n')
