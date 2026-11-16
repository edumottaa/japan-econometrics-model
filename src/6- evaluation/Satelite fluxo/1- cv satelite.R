# CV do modelo satelite: origens mensais, janela crescente, 12 meses a frente.
# Regressores realizados (OIS e fluxo ex post). Erro no nivel do spread = soma dos erros de Delta.
# Benchmarks: Naive (spread parado) e Drift (media historica de Delta).
trabalho <- file.path(saida,'cv satelite')
dir.create(trabalho,recursive=TRUE,showWarnings=FALSE)

# 1. Janelas: a primeira termina um ano depois do fim do YCC (mar/2025)
inicio <- which(treino$date==yearmonth(POS_YCC))+11L
cv <- treino |> stretch_tsibble(.init=inicio,.step=1)

# 2. Estimar em cada janela e prever 12 meses com os regressores realizados
ajustes <- cv |> model(Satelite=ARIMA(d_spread~1+ois_pos+fluxo_l2+pdq(0,0,0)+PDQ(0,0,0)),
                       Drift=MEAN(d_spread))
novos <- new_data(cv,12) |> left_join(as_tibble(treino) |> select(date,ois_pos,fluxo_l2),by='date') |>
  filter(!is.na(ois_pos))
previsoes <- forecast(ajustes,new_data=novos) |> as_tibble() |> select(.id,.model,date,.mean)
previsoes <- bind_rows(previsoes,distinct(previsoes,.id,date) |> mutate(.model='Naive',.mean=0))

# 3. Erro no nivel do spread (pb) por horizonte
erros <- previsoes |> left_join(as_tibble(treino) |> select(date,d_spread),by='date') |>
  group_by(.id,.model) |> arrange(date) |>
  mutate(h=row_number(),erro=cumsum(d_spread-.mean)) |> ungroup()
metricas <- erros |> group_by(.model,h) |>
  summarise(origens=n(),RMSE=sqrt(mean(erro^2)),MAE=mean(abs(erro)),.groups='drop')
print(metricas |> filter(h %in% c(1,3,6,12)) |> arrange(h,RMSE),n=Inf)

# Armazenamento da saida em outputs.
write_csv(erros |> mutate(date=as.Date(date)),file.path(trabalho,'erros cv.csv'))
write_csv(metricas,file.path(trabalho,'metricas cv.csv'))
g <- ggplot(metricas,aes(h,RMSE,colour=.model))+geom_line(linewidth=.9)+geom_point()+
  scale_x_continuous(breaks=1:12)+theme_minimal()+theme(legend.position='bottom')+
  labs(title='CV do satelite: RMSE do nivel do spread 30-5 (pb)',x='Meses a frente',y=NULL,colour=NULL,
    caption=paste('Origens mensais de',format(min(erros$date)-1L),'em diante; OIS e fluxo realizados.'))
ggsave(file.path(trabalho,'rmse cv.png'),g,width=10,height=6,dpi=150,bg='white')
