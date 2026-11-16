# Teste isolado: adicionar inflacao a equacao do spread. Nao reestima o BVAR.
source('src/chat gpt/JGB fluxo/00_dados.R',encoding='UTF-8')
saida_teste <- file.path(saida,'teste inflacao')
dir.create(saida_teste,recursive=TRUE,showWarnings=FALSE)

# 1. CPI do proprio banco de dados do BVAR e 100*log(indice), nao inflacao.
# Converter para inflacao observada em 12 meses (%), sem usar forecasts no treino.
inflacao <- readRDS(file.path(fonte_modelo,'dados.rds')) |> as_tibble() |>
  arrange(date) |>
  transmute(mes=yearmonth(date),inflacao_12m=100*(exp((cpi_core-lag(cpi_core,12))/100)-1))
amostra <- base |> inner_join(inflacao,by='mes') |>
  filter(mes>=inicio_amostra,mes<=fim_amostra,
         if_all(c(d_spread,ois_pos,fluxo_l2,inflacao_12m),is.finite)) |>
  as_tsibble(index=mes)
stopifnot(!any(has_gaps(amostra)$.gaps))

# 2. Mesma amostra e erro branco do satelite original. Apenas um regressor novo.
# Delta spread_t = c + b1*Delta OIS_pos_t + b2*fluxo_(t-2) + b3*inflacao12m_t + erro_t.
# Inflacao contemporanea REALIZADA: analise ex post, nao surpresa inflacionaria.
ajustar_inflacao <- function(x) x |> model(
  sem_inflacao=ARIMA(d_spread ~ 1+ois_pos+fluxo_l2+pdq(0,0,0)+PDQ(0,0,0)),
  com_inflacao=ARIMA(d_spread ~ 1+ois_pos+fluxo_l2+inflacao_12m+pdq(0,0,0)+PDQ(0,0,0)))
ajustes <- ajustar_inflacao(amostra)
coeficientes <- tidy(ajustes)
comparacao <- glance(ajustes) |> select(.model,AIC,AICc,BIC)
residuos <- augment(ajustes) |> as_tibble()
diagnosticos <- residuos |> group_by(.model) |>
  summarise(n=sum(is.finite(.innov)),
    r2=1-sum(.innov^2,na.rm=TRUE)/sum((d_spread-mean(d_spread,na.rm=TRUE))^2,na.rm=TRUE),
    ljung_box_12_p=Box.test(na.omit(.innov),lag=12,type='Ljung-Box',fitdf=0)$p.value,
    .groups='drop')
comparacao <- left_join(comparacao,diagnosticos,by='.model')

# Verificacao adicional de significancia com erros HAC (3 meses), mesma regressao.
# Com erro ARIMA(0,0,0), a equacao de media coincide com MQO.
ols <- lm(d_spread ~ ois_pos+fluxo_l2+inflacao_12m,data=amostra)
hac <- lmtest::coeftest(ols,vcov.=sandwich::NeweyWest(ols,lag=3,prewhite=FALSE,adjust=TRUE))
hac <- tibble(term=rownames(hac),estimate_hac=hac[,1],std_error_hac=hac[,2],p_hac=hac[,4])
coeficientes <- coeficientes |> left_join(hac,by='term') |>
  mutate(across(c(estimate_hac,std_error_hac,p_hac),~if_else(.model=='com_inflacao',.x,NA_real_)))

# 3. CV recente EX POST de 12 meses; mesmos insumos realizados e origens comuns.
# O corte do banco BVAR e ago/2026, portanto sao 11 origens completas, nao 12.
origens <- seq(yearmonth('2024 Oct'),max(amostra$mes)-12L,by=1L)
cv <- purrr::map_dfr(origens,function(o) {
  treino <- amostra |> filter(mes<=o)
  janela <- amostra |> filter(mes>o,mes<=o+12L)
  previsao <- forecast(ajustar_inflacao(treino),new_data=janela) |> as_tibble() |>
    group_by(.model) |> summarise(previsto=sum(.mean),.groups='drop')
  bind_rows(previsao,tibble(.model=c('Naive','Drift'),
                            previsto=c(0,12*mean(treino$d_spread)))) |>
    mutate(origem=o,observado=sum(janela$d_spread))
})
metricas <- cv |> group_by(.model) |>
  summarise(origens=n(),rmse_pb=sqrt(mean((previsto-observado)^2)),
            mae_pb=mean(abs(previsto-observado)),.groups='drop')
write_csv(coeficientes,file.path(saida_teste,'coeficientes.csv'))
write_csv(comparacao,file.path(saida_teste,'ajuste e residuos.csv'))
write_csv(cv |> mutate(origem=as.Date(origem)),file.path(saida_teste,'CV ex post previsoes.csv'))
write_csv(metricas,file.path(saida_teste,'CV ex post metricas.csv'))
write_csv(amostra |> as_tibble() |> mutate(mes=as.Date(mes)),file.path(saida_teste,'amostra.csv'))
saveRDS(ajustes,file.path(saida_teste,'modelos.rds'))
g <- ggplot(metricas,aes(reorder(.model,rmse_pb),rmse_pb)) +
  geom_col(fill='#315B7A',width=.65) +
  geom_text(aes(label=formatC(rmse_pb,format='f',digits=1,decimal.mark=',')),hjust=-.2) +
  coord_flip() + scale_y_continuous(expand=expansion(mult=c(0,.15))) +
  theme_minimal(base_size=12) + labs(title='Inflacao no satelite: CV ex post em 12 meses',
    subtitle=paste(length(origens),'origens comuns | OIS, fluxo e inflacao realizados'),
    x=NULL,y='RMSE (pb; menor e melhor)',
    caption='Compara somente a equacao do spread. Nao valida as trajetorias futuras dos insumos.')
ggsave(file.path(saida_teste,'CV ex post.png'),g,width=9,height=4.8,dpi=170,bg='white')
cat('Amostra:',format(min(amostra$mes)),'a',format(max(amostra$mes)),'\n')
print(coeficientes,width=Inf)
print(comparacao,width=Inf)
print(metricas,width=Inf)
