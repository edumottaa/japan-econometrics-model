Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/resultados'
selecao <- readRDS(file.path(trabalho,'selecao.rds'))
dados <- readRDS(file.path(trabalho,'dados.rds'))$dados
specs <- selecao$specs; K <- selecao$K
termos <- function(x,lags) paste(ifelse(lags==0,x,paste0('lag(',x,',',lags,')')),collapse=' + ')
# Compra liquida dos estrangeiros (JSDA superlongos, tri de ienes, em nivel: ja e fluxo).
# Mesma amostra efetiva em todos os testes: descartar os K primeiros meses.
lag_data <- dados |> mutate(y=if_else(row_number()<=K,NA_real_,liq_estr))

# 1. Qual juro move o fluxo? Quatro especificacoes em nivel, lag 0, d=0 e constante,
# comparaveis pelo AICc. FPP3 10.2: o erro da regressao precisa ser estacionario.
candidatos <- list(`UST30 - JGB30`='diferencial',`JGB30`='jgb30',
  `UST30 e JGB30 separados`=c('ust30','jgb30'),`JGB30 com hedge - UST30`='diferencial_hedge')
comparacao <- imap_dfr(candidatos,function(x,nome) {
  m <- lag_data |> model(arima=ARIMA(!!as.formula(paste('y ~',paste(x,collapse=' + '),
    '+ pdq(p=0:3,d=0,q=0:3) + PDQ(0,0,0) + 1')),stepwise=FALSE,approximation=FALSE))
  erro <- residuals(m,type='regression') |> filter(is.finite(.resid))
  dof <- sum(str_detect(tidy(m)$term,'^(ar|ma)[0-9]+$'))
  coefs <- tidy(m) |> filter(term %in% x)
  tibble(especificacao=nome,regressores=list(x),AICc=glance(m)$AICc,
    coeficientes=paste0(coefs$term,'=',signif(coefs$estimate,3),' (p=',round(coefs$p.value,3),')',collapse='; '),
    ndiffs_erro=unitroot_ndiffs(erro$.resid)[[1]],kpss_pvalor_erro=unitroot_kpss(erro$.resid)[['kpss_pvalue']],
    lb12_pvalor=features(augment(m) |> filter(is.finite(.innov)),.innov,ljung_box,lag=12,dof=dof)$lb_pvalue)
}) |> arrange(AICc) |> mutate(delta_AICc=AICc-min(AICc))
print(comparacao |> select(-regressores))

# 2. Escolha: erro estacionario primeiro, depois menor AICc. Se nenhum erro for
# estacionario, d=1 sem constante (o ARIMA diferencia y e x; beta segue em nivel).
escolhida <- comparacao |> arrange(desc(ndiffs_erro==0),AICc) |> slice_head(n=1)
d <- escolhida$ndiffs_erro; stopifnot(d %in% 0:1)
constante <- if(d==0) '+ 1' else '+ 0'
regressores <- escolhida$regressores[[1]]
cat('ESPECIFICACAO:',escolhida$especificacao,'| d =',d,'\n')

# 3. Defasagens de cada regressor (mesma regra dos spreads).
testes <- map_dfr(regressores,function(x) map_dfr(names(specs),function(nome) {
  formula <- as.formula(paste('y ~',termos(x,specs[[nome]]),
    '+ pdq(p=0:3,d=',d,',q=0:3) + PDQ(0,0,0)',constante))
  ajuste <- lag_data |> model(arima=ARIMA(!!formula,stepwise=FALSE,approximation=FALSE))
  glance(ajuste) |> transmute(regressor=x,spec=nome,n_termos=length(specs[[nome]]),
    menor_lag=min(specs[[nome]]),AICc,BIC)
}) |> filter(is.finite(AICc)) |> arrange(AICc) |> mutate(delta_AICc=AICc-min(AICc)))
# Regra EUA: delta AICc < 2, menos termos, maior menor lag, menor AICc.
escolhas <- testes |> filter(delta_AICc<2) |> group_by(regressor) |>
  arrange(n_termos,desc(menor_lag),AICc,.by_group=TRUE) |> slice_head(n=1) |> ungroup()
lado_direito <- paste(map2_chr(escolhas$regressor,escolhas$spec,~ termos(.x,specs[[.y]])),collapse=' + ')
formula <- as.formula(paste('liq_estr ~',lado_direito,'+ pdq(p=0:3,d=',d,',q=0:3) + PDQ(0,0,0)',constante))
print(testes,n=Inf); print(formula)

# 4. Modelo final e diagnostico das inovacoes.
modelo_fluxo <- dados |> mutate(liq_estr=if_else(row_number()<=K,NA_real_,liq_estr)) |>
  model(arima=ARIMA(!!formula,stepwise=FALSE,approximation=FALSE))
stopifnot(!any(vapply(modelo_fluxo$arima,inherits,logical(1),'null_mdl')))
report(modelo_fluxo)
residuos <- augment(modelo_fluxo) |> filter(is.finite(.innov))
dof <- sum(str_detect(tidy(modelo_fluxo)$term,'^(ar|ma)[0-9]+$'))
diagnostico <- map_dfr(c(12L,24L),function(k) residuos |>
  features(.innov,ljung_box,lag=k,dof=dof) |> mutate(lag_teste=k,dof=dof))
print(diagnostico)
g <- augment(modelo_fluxo) |> ggplot(aes(as.Date(mes)))+
  geom_col(aes(y=liq_estr),fill='grey70')+geom_line(aes(y=.fitted),colour='#B2182B',linewidth=.9)+
  theme_minimal(base_size=12)+labs(title='Compra liquida dos estrangeiros em superlongos (JSDA)',
    subtitle=paste('Barras: observado | linha: ajuste com',lado_direito),x=NULL,y='Tri de ienes')

# Armazenamento da saida em outputs.
saveRDS(list(modelo=modelo_fluxo,formula=formula,escolhas=escolhas,comparacao=comparacao),
  file.path(trabalho,'modelo fluxo ARIMA.rds'))
write_csv(comparacao |> select(-regressores),file.path(trabalho,'fluxo comparacao especificacoes.csv'))
write_csv(testes,file.path(trabalho,'fluxo teste defasagens.csv'))
write_csv(tidy(modelo_fluxo),file.path(trabalho,'fluxo coeficientes ARIMA.csv'))
write_csv(diagnostico,file.path(trabalho,'fluxo diagnostico ARIMA.csv'))
ggsave(file.path(trabalho,'fluxo residuos ARIMA.png'),gg_tsresiduals(modelo_fluxo),width=11,height=7,dpi=160,bg='white')
ggsave(file.path(trabalho,'fluxo ajuste.png'),g,width=11,height=5,dpi=160,bg='white')
capture.output(report(modelo_fluxo),file=file.path(trabalho,'fluxo especificacao ARIMA.txt'))
cat('MODELO_FLUXO_CONCLUIDO\n')
