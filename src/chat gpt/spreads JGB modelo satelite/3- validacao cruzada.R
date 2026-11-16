Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/resultados'
selecao <- readRDS(file.path(trabalho,'selecao.rds'))
modelos <- readRDS(file.path(trabalho,'modelos ARIMA.rds'))
dados <- selecao$dados; K <- selecao$K; H <- 12L
preditores <- readRDS(file.path(trabalho,'dados.rds'))$preditores

# 1. Origem movel (FPP3 cap. 5.10): reestimar ate cada origem, prever 1 a 12 meses.
# Ordens p,q e constante fixadas no modelo final; a selecao de lags usou a amostra toda.
ordem_fixa <- function(alvo) {
  termos <- tidy(modelos[[alvo]])$term
  p <- sum(str_detect(termos,'^ar[0-9]+$')); q <- sum(str_detect(termos,'^ma[0-9]+$'))
  as.formula(str_replace(deparse1(selecao$formulas[[alvo]]),'pdq\\(.*\\) \\+ PDQ\\(0, 0, 0\\)',
    paste0('pdq(',p,',0,',q,') + PDQ(0,0,0) + ',as.integer('intercept'%in%termos))))
}
origens <- seq(yearmonth('2022 Dec'),max(dados$mes)-H,by=1)

# 2. Concorrentes. Rotulos: 'ex post' usa preditores futuros REALIZADOS, o que nao mede
# o desempenho com as curvas a termo disponiveis em cada origem.
cv_alvo <- function(alvo) {
  formula <- ordem_fixa(alvo)
  map_dfr(origens,function(o) {
    treino <- dados |> filter(mes<=o) |>
      mutate(!!alvo:=if_else(row_number()<=K,NA_real_,.data[[alvo]]))
    futuro <- dados |> filter(mes>o,mes<=o+H)
    ajuste <- treino |> model(reg=ARIMA(!!formula),
      sem_regressores=ARIMA(!!sym(alvo) ~ pdq(p=0:3,d=0,q=0:3) + PDQ(0,0,0)))
    reg_ex_post <- forecast(ajuste |> select(reg),new_data=futuro)
    # Preditores constantes: variacoes em zero; fluxo em nivel fixo no ultimo valor da origem.
    reg_zero <- forecast(ajuste |> select(reg),new_data=futuro |>
      mutate(across(starts_with('d_') & all_of(preditores),~0),liq_estr=last(treino$liq_estr)))
    sem_reg <- forecast(ajuste |> select(sem_regressores),h=H)
    # Regra da projecao: sem tendencia = sem constante e regressores como desvio da media
    # do treino. Equivale a subtrair c + soma(beta * media de x) de cada mes previsto.
    coefs <- tidy(ajuste |> select(reg)) |> filter(!str_detect(term,'^(ar|ma)[0-9]+$'))
    x_medio <- coefs |> mutate(variavel=str_remove(str_remove(term,'^lag\\('),',.*$'),
      media=if_else(term=='intercept',1,map_dbl(variavel,~ if(.x %in% names(treino)) mean(treino[[.x]],na.rm=TRUE) else NA_real_)))
    stopifnot(all(is.finite(x_medio$media)))
    tendencia <- sum(x_medio$estimate*x_medio$media)
    cat('CV',alvo,format(o),'\n'); flush.console()
    real <- futuro[[alvo]]
    tibble(alvo=alvo,origem=o,mes=futuro$mes,h=seq_len(H),real=real,
      `Regressao ex post (preditores realizados)`=reg_ex_post$.mean,
      `Regressao ex post sem tendencia`=reg_ex_post$.mean-tendencia,
      `Regressao com preditores constantes, sem tendencia`=reg_zero$.mean-tendencia,
      `ARIMA sem regressores`=sem_reg$.mean,`Variacao zero (nivel constante)`=0)
  })
}
previsoes <- map_dfr(names(modelos),cv_alvo)

# 3. Erro no NIVEL do spread: soma das variacoes previstas ate o horizonte h.
erros <- previsoes |> pivot_longer(-c(alvo,origem,mes,h,real),names_to='modelo',values_to='previsto') |>
  arrange(alvo,modelo,origem,h) |> group_by(alvo,modelo,origem) |>
  mutate(erro_nivel=cumsum(real)-cumsum(previsto)) |> ungroup()
metricas <- erros |> filter(h %in% c(1,3,6,12)) |> group_by(alvo,modelo,h) |>
  summarise(RMSE_pb=sqrt(mean(erro_nivel^2)),MAE_pb=mean(abs(erro_nivel)),
    n_origens=n(),.groups='drop') |> arrange(alvo,h,RMSE_pb)
print(metricas,n=Inf)
g <- erros |> group_by(alvo,modelo,h) |> summarise(RMSE=sqrt(mean(erro_nivel^2)),.groups='drop') |>
  ggplot(aes(h,RMSE,colour=modelo))+geom_line(linewidth=.9)+geom_point(size=1.4)+
  facet_wrap(vars(alvo),ncol=2)+scale_x_continuous(breaks=c(1,3,6,9,12))+
  theme_minimal(base_size=11)+theme(legend.position='bottom',legend.direction='vertical')+
  labs(title='Validacao cruzada temporal: RMSE do nivel do spread (pb)',
    subtitle=paste('Origens mensais de',format(min(origens)),'a',format(max(origens)),
      '| ordens ARIMA fixas, coeficientes reestimados'),x='Horizonte (meses)',y='RMSE (pb)',colour=NULL,
    caption='Ex post: preditores futuros realizados (desempenho condicional, nao reproduzivel em tempo real).')

# Armazenamento da saida em outputs.
write_csv(previsoes |> mutate(origem=as.Date(origem),mes=as.Date(mes)),file.path(trabalho,'cv previsoes.csv'))
write_csv(metricas,file.path(trabalho,'cv metricas.csv'))
ggsave(file.path(trabalho,'cv RMSE por horizonte.png'),g,width=11,height=6,dpi=160,bg='white')
cat('CV_CONCLUIDA\n')
