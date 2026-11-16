Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/resultados'
dados <- readRDS(file.path(trabalho,'dados.rds'))$dados
K <- 3L; H <- 12L

# Cadeia de projecao: VAR (compra liquida, fatia | UST30, OIS2Y) projeta as variaveis
# estrangeiras -> cada spread pelo seu modelo fechado. Comparar com e sem USD/JPY.
# Spreads fechados: 30-10 com fatia (lag 3); 30-5 com compra liquida (lag 2). Erro AR(1).
spreads <- list(d_s30_10='lag(d_fatia, 3)',d_s30_5='lag(liq_estr, 2)')
# USD/JPY ja testado e descartado (piorou em todos os horizontes); fica so UST30 e OIS2Y.
exogenas <- list(`Sem USD/JPY`=c('d_ust30','d_ois2'))
origens <- seq(yearmonth('2022 Dec'),max(dados$mes)-H,by=1)

# Regra da projecao: tirar a constante de tendencia = intercepto + beta da variavel
# estrangeira x sua media no treino. Os juros mantem o efeito completo.
tendencia <- function(ajuste,treino) {
  coefs <- tidy(ajuste) |> filter(term=='intercept' | str_detect(term,'liq_estr|d_fatia'))
  media <- map_dbl(coefs$term,function(t) if(t=='intercept') 1 else
    mean(treino[[str_remove(str_remove(t,'^lag\\('),',.*$')]],na.rm=TRUE))
  sum(coefs$estimate*media)
}

# ATENCAO: UST30, OIS2Y e USD/JPY futuros sao os REALIZADOS (ex post). So as variaveis
# estrangeiras sao projetadas pelo VAR, como na projecao real.
cv <- map_dfr(origens,function(o) {
  treino <- dados |> filter(mes<=o); futuro <- dados |> filter(mes>o,mes<=o+H)
  imap_dfr(exogenas,function(x,versao) {
    var <- treino |> model(v=VAR(!!as.formula(paste('vars(liq_estr,d_fatia) ~ AR(p=1:2) + xreg(1,',
      paste(x,collapse=','),')')),ic='aicc'))
    media_var <- forecast(var,new_data=futuro)$.mean
    futuro_var <- futuro |> mutate(liq_estr=media_var[,'liq_estr'],d_fatia=media_var[,'d_fatia'])
    imap_dfr(spreads,function(estrangeira,alvo) {
      f <- as.formula(paste(alvo,'~',paste(c(x,estrangeira),collapse=' + '),'+ pdq(1,0,0) + PDQ(0,0,0) + 1'))
      ajuste <- treino |> mutate(!!alvo:=if_else(row_number()<=K,NA_real_,.data[[alvo]])) |> model(a=ARIMA(!!f))
      desloc <- tendencia(ajuste,treino)
      previsto <- forecast(ajuste,new_data=futuro_var)$.mean-desloc
      teto <- forecast(ajuste,new_data=futuro)$.mean-desloc
      real <- futuro[[alvo]]
      tibble(alvo=alvo,versao=versao,origem=o,h=seq_len(H),real=real,
        `VAR, com constante de tendencia`=previsto+desloc,`VAR, sem constante de tendencia`=previsto,
        `Estrangeiras realizadas, com constante (teto)`=teto+desloc)
    })
  }) |> mutate(`Variacao zero`=0)
}) |> pivot_longer(-c(alvo,versao,origem,h,real),names_to='fonte',values_to='previsto') |>
  mutate(modelo=fonte) |>
  distinct(alvo,modelo,origem,h,.keep_all=TRUE) |>
  group_by(alvo,modelo,origem) |> arrange(h,.by_group=TRUE) |>
  mutate(erro=cumsum(real)-cumsum(previsto)) |> ungroup()

# Erro no NIVEL do spread (soma das variacoes ate h), em pb.
metricas <- cv |> filter(h %in% c(1,3,6,12)) |> group_by(alvo,modelo,h) |>
  summarise(RMSE_pb=sqrt(mean(erro^2)),MAE_pb=mean(abs(erro)),n_origens=n(),.groups='drop') |>
  arrange(alvo,h,RMSE_pb)
print(metricas,n=Inf)
g <- cv |> group_by(alvo,modelo,h) |> summarise(RMSE=sqrt(mean(erro^2)),.groups='drop') |>
  ggplot(aes(h,RMSE,colour=modelo))+geom_line(linewidth=.9)+geom_point(size=1.4)+
  facet_wrap(vars(alvo),ncol=2)+scale_x_continuous(breaks=c(1,3,6,9,12))+
  theme_minimal(base_size=11)+theme(legend.position='bottom',legend.direction='vertical')+
  labs(title='Validacao cruzada da cadeia VAR -> spread: RMSE do nivel (pb)',
    subtitle=paste('Origens mensais de',format(min(origens)),'a',format(max(origens)),'| com e sem a constante de tendencia'),
    x='Horizonte (meses)',y='RMSE (pb)',colour=NULL,
    caption='UST30, OIS2Y e USD/JPY futuros realizados (ex post). Teto: variaveis estrangeiras realizadas.')

# Armazenamento da saida em outputs.
write_csv(cv |> mutate(origem=as.Date(origem)),file.path(trabalho,'cv VAR previsoes.csv'))
write_csv(metricas,file.path(trabalho,'cv VAR metricas.csv'))
ggsave(file.path(trabalho,'cv VAR RMSE por horizonte.png'),g,width=11,height=6.5,dpi=160,bg='white')
cat('CV_VAR_CONCLUIDA\n')
