Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
trabalho <- file.path(raiz,'src/chat gpt/spreads JGB modelo satelite/resultados')
entrada <- readRDS(file.path(trabalho,'dados.rds'))
base <- entrada$base; dados <- entrada$dados; K <- 3L

# Modelo "Fluxo | ARIMA, UST so via fluxo" (especificacao e projecao no 8- premissa estrangeira 30-5.R):
#   Estagio 1: compra liquida estrangeira ~ UST30 (nivel, lag 2) + erros ARIMA
#   Estagio 2: d_s30_5 ~ OIS2Y + compra liquida (lag 2) + constante + erros ARIMA
# Primeiros K meses fora so do alvo de cada estagio (regressores defasados usam esses meses).
e1 <- dados |> mutate(liq_estr=if_else(row_number()<=K,NA_real_,liq_estr)) |> model(a=ARIMA(liq_estr ~ lag(ust30, 2) + pdq(p=0:3,d=0:1,q=0:3) + PDQ(0,0,0),stepwise=FALSE,approximation=FALSE))
e2 <- dados |> mutate(d_s30_5=if_else(row_number()<=K,NA_real_,d_s30_5)) |> model(a=ARIMA(d_s30_5 ~ d_ois2 + lag(liq_estr, 2) + pdq(p=0:3,d=0,q=0:3) + PDQ(0,0,0) + 1,stepwise=FALSE,approximation=FALSE))
lb <- function(m) { ordem <- m$a[[1]]$fit$spec; dof <- ordem$p+ordem$q
  augment(m) |> features(.innov,ljung_box,lag=12,dof=dof) |> pull(lb_pvalue) }
for(m in list(`Estagio 1: compra liquida estrangeira`=e1,`Estagio 2: spread 30-5`=e2) |> imap(~ list(.x,.y))) {
  cat('\n=====',m[[2]],'\n'); report(m[[1]])
  print(tidy(m[[1]]) |> select(term,estimate,std.error,p.value) |> mutate(across(where(is.double),~signif(.x,3))))
  cat('Ljung-Box (12 lags) p-valor:',signif(lb(m[[1]]),3),'| n =',sum(!is.na(augment(m[[1]])$.innov)),'\n') }
cat('Amostra:',format(min(dados$mes)),'-',format(max(dados$mes)),'| media UST30',round(mean(dados$ust30),2),
  '| media compra',round(mean(dados$liq_estr,na.rm=TRUE),2),'\n')

# Grafico do forecast (projecao do 8-): historico desde 2018, tres trajetorias de OIS e BVAR.
proj <- read_csv(file.path(trabalho,'premissa estrangeira - forecast 30-5.csv'),show_col_types=FALSE) |>
  filter(variante=='Fluxo | ARIMA, UST so via fluxo') |> mutate(mes=yearmonth(mes))
bvar <- read_csv(file.path(raiz,'src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao/curva_jgb_2_5_10_30_cenarios.csv'),
  show_col_types=FALSE) |> filter(cenario=='Base') |> transmute(mes=yearmonth(date),nivel=100*spread30_5,cenario='BVAR (Base)')
hist <- base |> as_tibble() |> filter(is.finite(s30_5),mes>=yearmonth('2018 Jun')) |> select(mes,nivel=s30_5)
cores <- c(`Trajetoria do mercado`='#2a78d6',`Nossa trajetoria do OIS (BVAR)`='#e07b39',
  `OIS no CAGR do juro curto BVAR`='#1baf9a',`BVAR (Base)`='grey30')
fim <- proj |> group_by(cenario) |> slice_max(mes,n=1)
g <- ggplot()+
  geom_ribbon(data=proj |> filter(cenario=='Trajetoria do mercado'),aes(as.Date(mes),ymin=p10,ymax=p90),fill='#2a78d6',alpha=.12)+
  geom_line(data=hist,aes(as.Date(mes),nivel),colour='grey15',linewidth=.8)+
  geom_line(data=bind_rows(proj |> select(mes,nivel,cenario),bvar),
    aes(as.Date(mes),nivel,colour=cenario,linetype=cenario=='BVAR (Base)'),linewidth=1)+
  geom_text(data=fim,aes(as.Date(mes),nivel,label=round(nivel),colour=cenario),hjust=-.25,size=3.6,show.legend=FALSE)+
  annotate('text',x=as.Date(max(bvar$mes)),y=last(bvar$nivel),label=round(last(bvar$nivel)),hjust=-.25,vjust=1.4,size=3.6,colour='grey30')+
  scale_colour_manual(values=cores,breaks=names(cores))+scale_linetype_manual(values=c(`FALSE`='solid',`TRUE`='dashed'),guide='none')+
  scale_x_date(date_breaks='1 year',date_labels='%Y',expand=expansion(mult=c(.01,.06)))+
  theme_minimal(base_size=12)+theme(legend.position='bottom',panel.grid.minor=element_blank())+
  labs(title='JGB 30Y-5Y (pb): modelo satelite com o UST so via fluxo estrangeiro',
    subtitle='Fluxo ~ UST30 (nivel, lag 2) + erros ARIMA  |  Spread ~ OIS2Y + fluxo (lag 2) + constante + erros ARIMA',
    x=NULL,y=NULL,colour=NULL,
    caption='UST30 e OIS2Y a termo Bloomberg (30/10/2026). Faixa: p10-p90 na trajetoria do mercado (so a incerteza do spread, nao a do fluxo).')
ggsave(file.path(trabalho,'forecast 30-5 UST via fluxo.png'),g,width=12,height=7,dpi=160,bg='white')
print(proj |> filter(mes %in% yearmonth(c('2026 Oct','2027 Oct','2028 Oct'))) |> select(cenario,mes,nivel,p10,p90,liq_estr) |>
  mutate(across(where(is.double),~round(.x,2))),n=Inf)
cat('CONCLUIDO\n')
