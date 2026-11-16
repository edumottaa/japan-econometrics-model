Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
pasta <- file.path(raiz,'src/chat gpt/spreads JGB modelo satelite')
trabalho <- file.path(pasta,'resultados')
entrada <- readRDS(file.path(trabalho,'dados.rds'))
base <- entrada$base; dados <- entrada$dados; K <- 3L
set.seed(20261030)

# Forecast do 30-5 no modelo "UST so via fluxo" (10-) com o regime do YCC no estagio 2 (13-).
#   Estagio 1 (igual ao 10-): liq_estr ~ UST30 (lag 2) + erros ARIMA.
#   Estagio 2, versoes:
#     Atual          : d_s30_5 ~ OIS + fluxo(lag 2) + constante + AR(1)
#     OIS x YCC      : + OIS x dummy YCC (efeito do OIS diferente no YCC; constante unica)
#     OIS x YCC + dummy: + dummy YCC de nivel (constante propria pos-YCC), como no 13-
# Dummy YCC = 1 de jun/2018 a mar/2024. Na projecao (pos-YCC) dummy = 0.
dados <- dados |> mutate(liq_l2=lag(liq_estr,2),ycc=as.numeric(mes<=yearmonth('2024 Mar')),ois_x_ycc=d_ois2*ycc)

e1 <- dados |> mutate(liq_estr=if_else(row_number()<=K,NA_real_,liq_estr)) |>
  model(a=ARIMA(liq_estr ~ lag(ust30, 2) + pdq(p=0:3,d=0:1,q=0:3) + PDQ(0,0,0),stepwise=FALSE,approximation=FALSE))
versoes <- list(`Atual`=c('d_ois2','liq_l2'),`OIS x YCC`=c('d_ois2','ois_x_ycc','liq_l2'),
  `OIS x YCC + dummy de nivel`=c('d_ois2','ois_x_ycc','liq_l2','ycc'))
e2 <- map(versoes,~ dados |> mutate(d_s30_5=if_else(row_number()<=K,NA_real_,d_s30_5)) |>
  model(a=ARIMA(!!as.formula(paste('d_s30_5 ~',paste(.x,collapse=' + '),'+ pdq(p=0:3,d=0,q=0:3) + PDQ(0,0,0) + 1')),
    stepwise=FALSE,approximation=FALSE)))
iwalk(e2,~ { cat('\n=====',.y,'| AICc',round(glance(.x)$AICc,1),'|',format(.x$a[[1]]),'\n')
  print(tidy(.x) |> select(term,estimate,std.error,p.value) |> mutate(across(where(is.double),~signif(.x,3)))) })

# Caminhos (mesma construcao do 8-): UST30 e OIS2Y a termo; nosso OIS; OIS no CAGR do BVAR.
curvas <- read_csv(file.path(pasta,'dados/curvas_bloomberg.csv'),col_types='ccDdd')
snap <- unique(curvas$data_snapshot)
fim_mercado <- max(base$mes[is.finite(base$d_ust30)])
meses <- seq(max(dados$mes)+1L,fim_mercado+max(curvas$horizonte_meses),by=1)
futuros <- meses[meses>fim_mercado]; j <- as.integer(futuros-fim_mercado)
diario <- list(
  ust30=read_csv(file.path(pasta,'dados/DGS30_2026-11-04.csv'),col_types='Dd',na=c('','.')) |> transmute(date=observation_date,taxa=DGS30),
  ois2=read_csv(file.path(pasta,'dados/ois2_historico.csv'),col_types='Dd') |> transmute(date,taxa=ois2_pct))
curva <- function(x) { cx <- curvas |> filter(variavel==x) |> arrange(horizonte_meses)
  approx(cx$horizonte_meses,cx$taxa_pct,xout=0:max(j))$y }
media_mes <- function(f) (head(f,-1)+f[-1])/2
nivel_mercado <- function(x) { f <- curva(x); spot <- diario[[x]] |> filter(date==snap) |> pull(taxa); spot+media_mes(f)-f[1] }
curto <- tibble(mes=futuros,curto_mercado=media_mes(curva('ois_1m'))[j]) |>
  left_join(read_csv(file.path(raiz,'src/Outputs/4- Forecast/todos cenarios oferta central/producao/resumo_forecast.csv'),
    show_col_types=FALSE) |> filter(cenario=='Base',variavel=='juro_curto') |> transmute(mes=yearmonth(date),curto_bvar=mediana),by='mes') |>
  mutate(diferenca=curto_bvar-curto_mercado) |> fill(diferenca)
diferenca <- c(curto$diferenca,rep(last(curto$diferenca),23L))
ajuste_ois <- map_dbl(j,~ mean(diferenca[.x:(.x+23L)]))
cb <- read_csv(file.path(raiz,'src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao/resumo_forecast.csv'),show_col_types=FALSE) |>
  filter(cenario=='Base',variavel=='juro_curto') |> transmute(mes=yearmonth(date),c=mediana) |> arrange(mes)
cagr <- (cb$c[cb$mes==cb$mes[1]+24L]/cb$c[1])^(1/2)-1
ultimo <- base |> as_tibble() |> filter(mes==fim_mercado)
realizado <- base |> as_tibble() |> filter(mes %in% meses,mes<=fim_mercado) |> select(mes,d_ois2,ust30)
caminho <- function(nivel_ois) bind_rows(realizado,tibble(mes=futuros,
  d_ois2=100*diff(c(ultimo$ois2,nivel_ois)),ust30=nivel_mercado('ust30')[j])) |>
  mutate(ycc=0,ois_x_ycc=0) |> as_tsibble(index=mes)
cenarios <- list(`Trajetoria do mercado`=caminho(nivel_mercado('ois2')[j]),
  `Nossa trajetoria do OIS (BVAR)`=caminho(nivel_mercado('ois2')[j]+ajuste_ois),
  `OIS no CAGR do juro curto BVAR`=caminho(ultimo$ois2*(1+cagr)^(j/12)))

# Projecao: fluxo pelo estagio 1; liq_l2 = fluxo de 2 meses antes (historico nos 2 primeiros meses).
inicio <- base$s30_5[base$mes==max(dados$mes)]
hist_liq <- as_tibble(dados) |> select(mes,liq_estr)
projecao <- imap_dfr(cenarios,function(x,cenario) {
  fluxo <- tibble(mes=x$mes,liq_estr=forecast(e1,new_data=x)$.mean)
  serie <- bind_rows(hist_liq,fluxo)
  x <- x |> mutate(liq_l2=serie$liq_estr[match(mes,serie$mes)-2L])
  imap_dfr(e2,function(m,versao) {
    sim <- generate(m,new_data=x,times=1000) |> as_tibble() |> arrange(.rep,mes) |>
      group_by(.rep) |> mutate(nivel=inicio+cumsum(.sim)) |> ungroup() |>
      group_by(mes) |> summarise(p10=quantile(nivel,.1),p90=quantile(nivel,.9),.groups='drop')
    t <- tidy(m); b <- set_names(t$estimate,t$term)
    tibble(mes=x$mes,cenario=cenario,versao=versao,liq_estr=fluxo$liq_estr,
      nivel=inicio+cumsum(forecast(m,new_data=x)$.mean),
      c_ois=cumsum(b[['d_ois2']]*x$d_ois2),c_fluxo=cumsum(b[['liq_l2']]*x$liq_l2),
      c_const=cumsum(rep(b[['intercept']],nrow(x)))) |> left_join(sim,by='mes') }) })
tab <- projecao |> filter(mes==max(mes)) |>
  transmute(versao,cenario,nivel=round(nivel),faixa=paste0(round(p10),'-',round(p90)),
    OIS=round(c_ois),Fluxo=round(c_fluxo),Constante=round(c_const))
print(tab |> arrange(versao,cenario),n=Inf,width=Inf)

bvar <- read_csv(file.path(raiz,'src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao/curva_jgb_2_5_10_30_cenarios.csv'),
  show_col_types=FALSE) |> filter(cenario=='Base') |> transmute(mes=yearmonth(date),nivel=100*spread30_5)
hist <- base |> as_tibble() |> filter(is.finite(s30_5),mes>=yearmonth('2018 Jun')) |> select(mes,nivel=s30_5)
cores <- c(`Trajetoria do mercado`='#2a78d6',`Nossa trajetoria do OIS (BVAR)`='#e07b39',`OIS no CAGR do juro curto BVAR`='#1baf9a')
fim_p <- projecao |> group_by(versao,cenario) |> slice_max(mes,n=1)
g <- ggplot()+geom_line(data=hist |> filter(mes>=yearmonth('2022 Jan')),aes(as.Date(mes),nivel),colour='grey15',linewidth=.7)+
  geom_line(data=bvar,aes(as.Date(mes),nivel),colour='grey30',linetype='dashed',linewidth=.8)+
  geom_line(data=projecao,aes(as.Date(mes),nivel,colour=cenario),linewidth=.9)+
  geom_text(data=fim_p,aes(as.Date(mes),nivel,label=round(nivel),colour=cenario),hjust=-.2,size=3.2,show.legend=FALSE)+
  facet_wrap(vars(factor(versao,names(versoes))),nrow=1)+
  scale_colour_manual(values=cores)+scale_x_date(date_breaks='1 year',date_labels='%Y',expand=expansion(mult=c(.01,.12)))+
  theme_minimal(base_size=11)+theme(legend.position='bottom',panel.grid.minor=element_blank())+
  labs(title='JGB 30Y-5Y (pb): modelo UST so via fluxo, com e sem o regime do YCC no OIS',
    subtitle='Tracejado: BVAR (Base). Dummy YCC = jun/2018 a mar/2024; projecao no regime pos-YCC.',x=NULL,y=NULL,colour=NULL)
ggsave(file.path(trabalho,'forecast 30-5 regime YCC.png'),g,width=14,height=6,dpi=160,bg='white')
write_csv(projecao |> mutate(mes=as.Date(mes)),file.path(trabalho,'forecast 30-5 regime YCC.csv'))
cat('CONCLUIDO\n')
