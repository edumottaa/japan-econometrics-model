Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/resultados'
dados <- readRDS(file.path(trabalho,'dados.rds'))$dados
K <- 3L; H <- 12L

# CV do 30-5 por premissa para a variavel estrangeira (ver 8- premissa estrangeira 30-5.R):
#   VAR   : VAR(compra, fatia | UST30, OIS2Y) projeta a estrangeira
#   ARIMA : estrangeira ~ UST30 em nivel (lag 2) + erros ARIMA (ordens do 8-: MA(2) compra, MA(1) fatia)
# Spread 30-5 com erro AR(1) e constante de tendencia mantida:
#   direto   : UST30 + OIS2Y + estrangeira (lag 2)
#   indireto : OIS2Y + estrangeira (UST so via estrangeira)
# ATENCAO (ex post): UST30 e OIS2Y futuros sao os REALIZADOS em cada origem.
origens <- seq(yearmonth('2022 Dec'),max(dados$mes)-H,by=1)
estrangeira <- c(liq_estr='lag(liq_estr, 2)',d_fatia='lag(d_fatia, 2)')
ordem_arima <- c(liq_estr='pdq(0,0,2)',d_fatia='pdq(0,0,1)')
especs <- tribble(~modelo,~estr,~premissa,~uso_ust,
  'Fluxo | VAR','liq_estr','VAR','direto',
  'Fluxo | ARIMA','liq_estr','ARIMA','direto',
  'Fluxo | ARIMA, UST so via fluxo','liq_estr','ARIMA','indireto',
  'Fatia | VAR','d_fatia','VAR','direto',
  'Fatia | ARIMA','d_fatia','ARIMA','direto',
  'Fatia | ARIMA, UST so via fatia','d_fatia','ARIMA','indireto')

cv <- map_dfr(origens,function(o) {
  treino <- dados |> filter(mes<=o); futuro <- dados |> filter(mes>o,mes<=o+H)
  var <- treino |> model(v=VAR(vars(liq_estr,d_fatia) ~ AR(p=1:2) + xreg(1,d_ust30,d_ois2),ic='aicc'))
  m_var <- forecast(var,new_data=futuro)$.mean
  premissas <- list(VAR=futuro |> mutate(liq_estr=m_var[,'liq_estr'],d_fatia=m_var[,'d_fatia']))
  premissas$ARIMA <- futuro
  erro_estr <- list()
  for(y in names(estrangeira)) {
    a <- treino |> mutate(!!y:=if_else(row_number()<=K,NA_real_,.data[[y]])) |>
      model(a=ARIMA(!!as.formula(paste(y,'~ lag(ust30, 2) +',ordem_arima[[y]],'+ PDQ(0,0,0) + 1'))))
    premissas$ARIMA[[y]] <- forecast(a,new_data=futuro)$.mean
    erro_estr[[y]] <- tibble(estr=y,premissa=c('VAR','ARIMA'),
      rmse_estr=c(sqrt(mean((m_var[,y]-futuro[[y]])^2)),sqrt(mean((premissas$ARIMA[[y]]-futuro[[y]])^2))))
  }
  real <- futuro$d_s30_5
  pmap_dfr(especs,function(modelo,estr,premissa,uso_ust) {
    regs <- c(if(uso_ust=='direto') 'd_ust30','d_ois2',estrangeira[[estr]])
    f <- as.formula(paste('d_s30_5 ~',paste(regs,collapse=' + '),'+ pdq(1,0,0) + PDQ(0,0,0) + 1'))
    ajuste <- treino |> mutate(d_s30_5=if_else(row_number()<=K,NA_real_,d_s30_5)) |> model(a=ARIMA(!!f))
    tibble(origem=o,h=seq_len(H),real=real,modelo=modelo,
      previsto=forecast(ajuste,new_data=premissas[[premissa]])$.mean,
      teto=forecast(ajuste,new_data=futuro)$.mean)
  }) |> bind_rows(tibble(origem=o,h=seq_len(H),real=real,modelo='Variacao zero',previsto=0,teto=0)) |>
    mutate(erro_estr=list(bind_rows(erro_estr)))
})
erros_estr <- cv |> distinct(origem,.keep_all=TRUE) |> select(origem,erro_estr) |> unnest(erro_estr) |>
  group_by(estr,premissa) |> summarise(RMSE_12m_media=mean(rmse_estr),.groups='drop')
print(erros_estr)

cv <- cv |> select(-erro_estr) |> group_by(modelo,origem) |> arrange(h,.by_group=TRUE) |>
  mutate(erro=cumsum(real)-cumsum(previsto),erro_teto=cumsum(real)-cumsum(teto),
    vies=cumsum(previsto)-cumsum(real)) |> ungroup()
metricas <- cv |> filter(h %in% c(1,3,6,12)) |> group_by(modelo,h) |>
  summarise(RMSE_pb=sqrt(mean(erro^2)),vies_pb=mean(vies),RMSE_teto_pb=sqrt(mean(erro_teto^2)),.groups='drop')
print(metricas |> mutate(across(where(is.double),~round(.x,1))) |>
  pivot_wider(names_from=h,values_from=c(RMSE_pb,vies_pb,RMSE_teto_pb)) |> arrange(RMSE_pb_12),width=Inf)

cores <- c(`Fluxo | VAR`='#2a78d6',`Fluxo | ARIMA`='#7fb0ec',`Fluxo | ARIMA, UST so via fluxo`='#1f4f8a',
  `Fatia | VAR`='#e07b39',`Fatia | ARIMA`='#f2b48a',`Fatia | ARIMA, UST so via fatia`='#9c4a17',`Variacao zero`='grey40')
g <- cv |> group_by(modelo,h) |> summarise(RMSE=sqrt(mean(erro^2)),.groups='drop') |>
  ggplot(aes(h,RMSE,colour=modelo))+geom_line(linewidth=.9)+geom_point(size=1.4)+
  scale_colour_manual(values=cores)+scale_x_continuous(breaks=c(1,3,6,9,12))+
  theme_minimal(base_size=11)+theme(legend.position='bottom')+guides(colour=guide_legend(nrow=3))+
  labs(title='Validacao cruzada do JGB 30Y-5Y por premissa estrangeira: RMSE do nivel (pb)',
    subtitle=paste('Origens mensais de',format(min(origens)),'a',format(max(origens)),'| com a constante de tendencia'),
    x='Horizonte (meses)',y='RMSE (pb)',colour=NULL,
    caption='Ex post: UST30 e OIS2Y futuros realizados. So a variavel estrangeira e projetada.')

write_csv(cv |> mutate(origem=as.Date(origem)),file.path(trabalho,'cv premissa estrangeira 30-5 previsoes.csv'))
write_csv(metricas,file.path(trabalho,'cv premissa estrangeira 30-5 metricas.csv'))
ggsave(file.path(trabalho,'cv premissa estrangeira 30-5.png'),g,width=11,height=6.5,dpi=160,bg='white')
cat('CV_CONCLUIDA\n')
