# MODELO FINAL do JGB 30Y-5Y (substitui o 20-, que tinha dummy de nivel e projetava ~278 pb).
# Estimacao (historico, jun/2018-set/2026):
#   Estagio 1: compra liquida estrangeira ~ UST30 em nivel (lag 2) + erros ARIMA
#   Estagio 2: d_s30_5 ~ OIS2Y SO pos-YCC + compra liquida (lag 2) + constante + erros ARIMA
#              OIS so pos-YCC = variacao do OIS2Y a partir de abr/2024 e zero antes: no YCC o OIS
#              nao mexia no spread (efeito imposto = 0). Sem dummy de nivel.
# Validacao ex post (OIS e fluxo realizados), origens out/2024 em diante.
# Premissas: OIS2Y e UST30 a termo (Bloomberg FWCV, 30/10/2026); fluxo pelo estagio 1.
source('C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/14- forecast 30-5 com regime YCC.R')
dados <- dados |> mutate(ois_pos=d_ois2*(1-ycc))
final_m <- dados |> mutate(d_s30_5=if_else(row_number()<=K,NA_real_,d_s30_5)) |>
  model(a=ARIMA(d_s30_5 ~ ois_pos + liq_l2 + pdq(p=0:3,d=0,q=0:3) + PDQ(0,0,0) + 1,stepwise=FALSE,approximation=FALSE))
cat('\n===== MODELO FINAL (OIS so pos-YCC)\n'); report(final_m)
print(tidy(final_m) |> select(term,estimate,std.error,p.value) |> mutate(across(where(is.double),~signif(.x,3))))
cat('Ljung-Box (12 lags) p =',signif(augment(final_m) |> features(.innov,ljung_box,lag=12) |> pull(lb_pvalue),3),'\n')
ordem <- final_m$a[[1]]$fit$spec; pdq_final <- sprintf('pdq(%d,0,%d)',ordem$p,ordem$q)

# Validacao cruzada ex post (mesmo desenho do 17-), contra as versoes anteriores.
versoes_cv <- list(`OIS so pos-YCC (final)`=c('ois_pos','liq_l2',pdq_final),`Sem dummy`=c('d_ois2','liq_l2','pdq(1,0,0)'),
  `OIS x YCC + dummy de nivel`=c('d_ois2','ois_x_ycc','liq_l2','ycc','pdq(0,0,0)'))
origens <- seq(yearmonth('2024 Oct'),max(dados$mes)-1L,by=1)
cv <- map_dfr(origens,function(o) {
  treino <- dados |> filter(mes<=o) |> mutate(d_s30_5=if_else(row_number()<=K,NA_real_,d_s30_5))
  futuro <- dados |> filter(mes>o,mes<=o+12L)
  imap_dfr(versoes_cv,function(r,nome) {
    m <- treino |> model(a=ARIMA(!!as.formula(paste('d_s30_5 ~',paste(r,collapse=' + '),'+ PDQ(0,0,0) + 1'))))
    tibble(origem=o,modelo=nome,h=seq_len(nrow(futuro)),real=futuro$d_s30_5,previsto=forecast(m,new_data=futuro)$.mean) }) |>
    bind_rows(tibble(origem=o,modelo='Variacao zero',h=seq_len(nrow(futuro)),real=futuro$d_s30_5,previsto=0)) }) |>
  group_by(origem,modelo) |> mutate(erro=cumsum(real)-cumsum(previsto)) |> ungroup()
print(cv |> filter(h %in% c(3,6,12)) |> group_by(modelo,h) |> summarise(RMSE=round(sqrt(mean(erro^2)),1),vies=round(-mean(erro),1),.groups='drop') |>
  pivot_wider(names_from=h,values_from=c(RMSE,vies)) |> arrange(RMSE_12))

# Projecao na trajetoria do mercado (OIS2Y e UST30 a termo).
x <- cenarios[['Trajetoria do mercado']]
fluxo <- tibble(mes=x$mes,liq_estr=forecast(e1,new_data=x)$.mean)
serie <- bind_rows(hist_liq,fluxo)
x <- x |> mutate(liq_l2=serie$liq_estr[match(mes,serie$mes)-2L],ois_pos=d_ois2)
b <- tidy(final_m); bb <- set_names(b$estimate,b$term)
sim <- generate(final_m,new_data=x,times=2000) |> as_tibble() |> arrange(.rep,mes) |>
  group_by(.rep) |> mutate(nivel=inicio+cumsum(.sim)) |> ungroup() |>
  group_by(mes) |> summarise(p10=quantile(nivel,.1),p90=quantile(nivel,.9),.groups='drop')
final <- as_tibble(x) |> transmute(mes,liq_estr=fluxo$liq_estr,nivel=inicio+cumsum(forecast(final_m,new_data=x)$.mean),
  OIS=cumsum(bb[['ois_pos']]*ois_pos),Fluxo=cumsum(bb[['liq_l2']]*liq_l2),Constante=cumsum(rep(bb[['intercept']],n()))) |>
  left_join(sim,by='mes')
print(final |> filter(mes %in% yearmonth(c('2026 Oct','2027 Mar','2027 Oct','2028 Mar','2028 Oct'))) |>
  mutate(across(where(is.double),~round(.x,2))))

hist <- base |> as_tibble() |> filter(is.finite(s30_5),mes>=yearmonth('2018 Jun')) |> select(mes,nivel=s30_5)
ult <- final |> slice_max(mes,n=1)
g <- ggplot()+
  geom_ribbon(data=final,aes(as.Date(mes),ymin=p10,ymax=p90),fill='#2a78d6',alpha=.15)+
  geom_line(data=hist,aes(as.Date(mes),nivel),colour='grey15',linewidth=.8)+
  geom_line(data=final,aes(as.Date(mes),nivel),colour='#2a78d6',linewidth=1.1)+
  geom_line(data=bvar,aes(as.Date(mes),nivel),colour='grey45',linetype='dashed',linewidth=.9)+
  annotate('text',x=as.Date(ult$mes),y=ult$nivel,label=round(ult$nivel),hjust=-.3,colour='#2a78d6',size=4)+
  annotate('text',x=as.Date(max(bvar$mes)),y=last(bvar$nivel),label=paste('BVAR',round(last(bvar$nivel))),hjust=-.1,vjust=1.6,colour='grey40',size=3.6)+
  scale_x_date(date_breaks='1 year',date_labels='%Y',expand=expansion(mult=c(.01,.08)))+
  theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank())+
  labs(title='JGB 30Y-5Y (pb): modelo satelite final',
    subtitle='Spread ~ OIS2Y (so pos-YCC) + compra estrangeira (lag 2) + constante  |  Fluxo ~ UST30 (nivel, lag 2)',
    x=NULL,y=NULL,
    caption='Premissas: OIS2Y e UST30 a termo (Bloomberg FWCV, 30/10/2026). Faixa p10-p90 (so a incerteza do spread). Tracejado: BVAR (Base).')
ggsave(file.path(trabalho,'forecast 30-5 modelo final.png'),g,width=12,height=7,dpi=160,bg='white')
write_csv(final |> mutate(mes=as.Date(mes)),file.path(trabalho,'forecast 30-5 modelo final.csv'))
cat('CONCLUIDO\n')
