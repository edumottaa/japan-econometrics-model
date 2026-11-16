# SUBSTITUIDO pelo 21- (dummy de nivel projetava ~278 pb). MODELO FINAL antigo do JGB 30Y-5Y (opcao 1, modelo satelite).
# Estimacao (historico, jun/2018-set/2026):
#   Estagio 1: compra liquida estrangeira ~ UST30 em nivel (lag 2) + erros ARIMA
#   Estagio 2: d_s30_5 ~ OIS2Y + OIS2Y x dummy YCC + compra liquida (lag 2) + dummy YCC + constante + erros ARIMA
#              (dummy YCC = 1 de jun/2018 a mar/2024; projecao no regime pos-YCC, dummy = 0)
# Validacao: ex post (OIS e fluxo realizados), 17- validacao cruzada dummy YCC 30-5.R.
# Premissas: OIS2Y a termo (Bloomberg FWCV, 30/10/2026); fluxo pelo estagio 1 com UST30 a termo.
# Reaproveita a estimacao e os caminhos do 14- (versao 'OIS x YCC + dummy de nivel').
source('C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/14- forecast 30-5 com regime YCC.R')
versao_final <- 'OIS x YCC + dummy de nivel'
cat('\n===== MODELO FINAL\n--- Estagio 1\n'); report(e1)
print(tidy(e1) |> select(term,estimate,std.error,p.value) |> mutate(across(where(is.double),~signif(.x,3))))
cat('--- Estagio 2\n'); report(e2[[versao_final]])
print(tidy(e2[[versao_final]]) |> select(term,estimate,std.error,p.value) |> mutate(across(where(is.double),~signif(.x,3))))
cat('Ljung-Box estagio 2 (12 lags) p =',signif(augment(e2[[versao_final]]) |> features(.innov,ljung_box,lag=12) |> pull(lb_pvalue),3),'\n')

final <- projecao |> filter(versao==versao_final,cenario=='Trajetoria do mercado')
print(final |> filter(mes %in% yearmonth(c('2026 Oct','2027 Mar','2027 Oct','2028 Mar','2028 Oct'))) |>
  transmute(mes,fluxo=round(liq_estr,2),nivel=round(nivel),p10=round(p10),p90=round(p90),
    OIS=round(c_ois),Fluxo=round(c_fluxo),Constante=round(c_const)))

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
    subtitle='Spread ~ OIS2Y (efeito proprio pos-YCC) + compra estrangeira (lag 2) + dummy YCC  |  Fluxo ~ UST30 (nivel, lag 2)',
    x=NULL,y=NULL,
    caption='Premissas: OIS2Y e UST30 a termo (Bloomberg FWCV, 30/10/2026). Faixa p10-p90 (so a incerteza do spread). Tracejado: BVAR (Base).')
ggsave(file.path(trabalho,'forecast 30-5 modelo final.png'),g,width=12,height=7,dpi=160,bg='white')
write_csv(final |> mutate(mes=as.Date(mes)),file.path(trabalho,'forecast 30-5 modelo final.csv'))
cat('CONCLUIDO\n')
