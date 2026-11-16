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

# Pergunta: o modelo satelite abre o 30-5 mais que o BVAR? Testa a premissa para a variavel
# estrangeira (compra liquida ou fatia):
#   VAR   = VAR(compra, fatia) condicionado a UST30 e OIS2Y (como no 4- forecast.R)
#   ARIMA = regressao da variavel estrangeira no UST30 com erros ARIMA
# Spread 30-5 = regressao com erros ARIMA, constante de tendencia mantida.
#   direto   : UST30 + OIS2Y + estrangeira
#   indireto : OIS2Y + estrangeira (UST so entra via estrangeira; so com a premissa ARIMA)

# 1. Caminhos de UST30 e OIS2Y (mesma construcao do 4- forecast.R).
curvas <- read_csv(file.path(pasta,'dados/curvas_bloomberg.csv'),col_types='ccDdd')
snap <- unique(curvas$data_snapshot)
fim_mercado <- max(base$mes[is.finite(base$d_ust30)])
meses <- seq(max(dados$mes)+1L,fim_mercado+max(curvas$horizonte_meses),by=1)
futuros <- meses[meses>fim_mercado]; j <- as.integer(futuros-fim_mercado)
diario <- list(
  ust30=read_csv(file.path(pasta,'dados/DGS30_2026-11-04.csv'),col_types='Dd',na=c('','.')) |>
    transmute(date=observation_date,taxa=DGS30),
  ois2=read_csv(file.path(pasta,'dados/ois2_historico.csv'),col_types='Dd') |> transmute(date,taxa=ois2_pct))
curva <- function(x) { cx <- curvas |> filter(variavel==x) |> arrange(horizonte_meses)
  approx(cx$horizonte_meses,cx$taxa_pct,xout=0:max(j))$y }
media_mes <- function(f) (head(f,-1)+f[-1])/2
nivel_mercado <- function(x) { f <- curva(x); spot <- diario[[x]] |> filter(date==snap) |> pull(taxa)
  spot+media_mes(f)-f[1] }
bvar_curto <- read_csv(file.path(raiz,'src/Outputs/4- Forecast/todos cenarios oferta central/producao/resumo_forecast.csv'),
  show_col_types=FALSE) |> filter(cenario=='Base',variavel=='juro_curto') |>
  transmute(mes=yearmonth(date),curto_bvar=mediana)
curto <- tibble(mes=futuros,curto_mercado=media_mes(curva('ois_1m'))[j]) |>
  left_join(bvar_curto,by='mes') |> mutate(diferenca=curto_bvar-curto_mercado) |> fill(diferenca)
diferenca <- c(curto$diferenca,rep(last(curto$diferenca),23L))
ajuste_ois <- map_dbl(j,~ mean(diferenca[.x:(.x+23L)]))
ultimo <- base |> as_tibble() |> filter(mes==fim_mercado)
realizado <- base |> as_tibble() |> filter(mes %in% meses,mes<=fim_mercado) |> select(mes,d_ust30,d_ois2,ust30)
caminho <- function(extra_ois) bind_rows(realizado,tibble(mes=futuros,
  d_ust30=100*diff(c(ultimo$ust30,nivel_mercado('ust30')[j])),
  d_ois2=100*diff(c(ultimo$ois2,nivel_mercado('ois2')[j]+extra_ois)),
  ust30=nivel_mercado('ust30')[j])) |> as_tsibble(index=mes)
# Terceiro cenario: OIS2Y cresce no CAGR do juro curto do BVAR (Base, com tendencia) nos 2 anos
# projetados (set/2026 -> set/2028), a partir do nivel do ultimo mes observado.
curto_bvar <- read_csv(file.path(raiz,'src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao/resumo_forecast.csv'),
  show_col_types=FALSE) |> filter(cenario=='Base',variavel=='juro_curto') |>
  transmute(mes=yearmonth(date),curto=mediana) |> arrange(mes)
ini_cagr <- curto_bvar |> slice(1); fim_cagr <- curto_bvar |> filter(mes==ini_cagr$mes+24L)
cagr <- (fim_cagr$curto/ini_cagr$curto)^(1/2)-1
cat('CAGR juro curto BVAR',format(ini_cagr$mes),round(ini_cagr$curto,3),'->',format(fim_cagr$mes),
  round(fim_cagr$curto,3),'=',round(100*cagr,1),'% a.a.\n')
ois_cagr <- ultimo$ois2*(1+cagr)^(j/12)
caminho_cagr <- caminho(0) |> mutate(d_ois2=if_else(mes>fim_mercado,100*diff(c(ultimo$ois2,ois_cagr))[match(mes,futuros)],d_ois2))
cat('OIS2Y: mercado',round(last(nivel_mercado('ois2')[j]),2),'| CAGR BVAR',round(last(ois_cagr),2),'no fim do horizonte\n')
cenarios <- list(`Trajetoria do mercado`=caminho(0),`Nossa trajetoria do OIS (BVAR)`=caminho(ajuste_ois),
  `OIS no CAGR do juro curto BVAR`=caminho_cagr)

# 2. Premissa VAR.
var <- dados |> model(v=VAR(vars(liq_estr,d_fatia) ~ AR(p=1:2) + xreg(1,d_ust30,d_ois2),ic='aicc'))

# 3. Premissa ARIMA: variavel estrangeira ~ UST30 + erros ARIMA. Candidatos: UST30 em nivel ou
#    em variacao, defasagem 0 a 3; escolhe o menor AICc. d decidido pelo proprio ARIMA (erro).
candidatos <- c(paste0('lag(ust30, ',0:3,')'),paste0('lag(d_ust30, ',0:3,')')) |> str_replace(fixed('lag(ust30, 0)'),'ust30') |>
  str_replace(fixed('lag(d_ust30, 0)'),'d_ust30')
ajusta_estr <- function(y,x) dados |> mutate(!!y:=if_else(row_number()<=K,NA_real_,.data[[y]])) |>
  model(a=ARIMA(!!as.formula(paste(y,'~',x,'+ pdq(p=0:3,d=0:1,q=0:3) + PDQ(0,0,0)')),stepwise=FALSE,approximation=FALSE))
teste_estr <- expand_grid(y=c('liq_estr','d_fatia'),x=candidatos) |>
  mutate(m=map2(y,x,ajusta_estr),
    AICc=map_dbl(m,~ glance(.x)$AICc),
    arima=map_chr(m,~ format(.x$a[[1]])),
    beta=map2_dbl(m,x,~ tidy(.x) |> filter(term==.y) |> pull(estimate) |> (\(b) if(length(b)) b else NA_real_)()),
    p_valor=map2_dbl(m,x,~ tidy(.x) |> filter(term==.y) |> pull(p.value) |> (\(b) if(length(b)) b else NA_real_)()))
print(teste_estr |> select(-m) |> arrange(y,AICc),n=Inf)
arima_estr <- teste_estr |> group_by(y) |> slice_min(AICc,n=1) |> ungroup()
modelo_estr <- set_names(arima_estr$m,arima_estr$y)

# 4. Modelos do spread 30-5.
estrangeira <- c(fluxo='lag(liq_estr, 2)',fatia='lag(d_fatia, 2)')
ajusta_spread <- function(regs) dados |> mutate(d_s30_5=if_else(row_number()<=K,NA_real_,d_s30_5)) |>
  model(a=ARIMA(!!as.formula(paste('d_s30_5 ~',paste(regs,collapse=' + '),'+ pdq(p=0:3,d=0,q=0:3) + PDQ(0,0,0) + 1')),
    stepwise=FALSE,approximation=FALSE))
especs <- tribble(~variante,~estr,~premissa,~uso_ust,
  'Fluxo | VAR','fluxo','VAR','direto',
  'Fluxo | ARIMA','fluxo','ARIMA','direto',
  'Fluxo | ARIMA, UST so via fluxo','fluxo','ARIMA','indireto',
  'Fatia | VAR','fatia','VAR','direto',
  'Fatia | ARIMA','fatia','ARIMA','direto',
  'Fatia | ARIMA, UST so via fatia','fatia','ARIMA','indireto') |>
  mutate(regs=map2(estr,uso_ust,~ c(if(.y=='direto') 'd_ust30','d_ois2',estrangeira[[.x]])))
modelos_spread <- especs |> distinct(estr,uso_ust,regs) |> mutate(m=map(regs,ajusta_spread))
coef_spread <- modelos_spread |> mutate(t=map(m,tidy),AICc=map_dbl(m,~ glance(.x)$AICc),arima=map_chr(m,~ format(.x$a[[1]]))) |>
  select(estr,uso_ust,arima,AICc,t) |> unnest(t) |> select(-.model)
print(coef_spread,n=Inf)

# 5. Projecao das estrangeiras e do spread. Constante de tendencia mantida.
inicio <- base$s30_5[base$mes==max(dados$mes)]
estr_futuro <- function(premissa,x) {
  if(premissa=='VAR') { m <- forecast(var,new_data=x)$.mean; return(tibble(mes=x$mes,liq_estr=m[,'liq_estr'],d_fatia=m[,'d_fatia'])) }
  tibble(mes=x$mes,liq_estr=forecast(modelo_estr$liq_estr,new_data=x)$.mean,d_fatia=forecast(modelo_estr$d_fatia,new_data=x)$.mean) }
projecao <- especs |> left_join(modelos_spread |> select(estr,uso_ust,m),by=c('estr','uso_ust')) |>
  pmap_dfr(function(variante,estr,premissa,uso_ust,regs,m) imap_dfr(cenarios,function(x,cenario) {
    insumos <- as_tibble(x) |> left_join(estr_futuro(premissa,x),by='mes')
    nd <- as_tsibble(insumos,index=mes)
    variacao <- forecast(m,new_data=nd)$.mean
    sim <- generate(m,new_data=nd,times=1000) |> as_tibble() |> arrange(.rep,mes) |>
      group_by(.rep) |> mutate(nivel=inicio+cumsum(.sim)) |> ungroup() |>
      group_by(mes) |> summarise(p10=quantile(nivel,.1),p90=quantile(nivel,.9),.groups='drop')
    insumos |> select(mes,liq_estr,d_fatia) |> mutate(variante=variante,premissa=premissa,cenario=cenario,
      nivel=inicio+cumsum(variacao)) |> left_join(sim,by='mes') }))
resumo <- projecao |> filter(mes %in% c(min(meses),yearmonth('2027 Sep'),max(meses))) |>
  transmute(cenario,variante,mes=format(mes),nivel=round(nivel),faixa=paste0(round(p10),'-',round(p90))) |>
  pivot_wider(names_from=mes,values_from=c(nivel,faixa))
print(resumo,n=Inf,width=Inf)
media_estr <- projecao |> filter(mes>fim_mercado) |> group_by(premissa,cenario) |>
  summarise(compra_media=mean(liq_estr),fatia_media=mean(d_fatia),.groups='drop')
print(media_estr); cat('Historico: compra media',round(mean(dados$liq_estr,na.rm=TRUE),2),
  '| ultimos 12m',round(mean(tail(dados$liq_estr,12)),2),'| fatia media',round(mean(dados$d_fatia,na.rm=TRUE),2),'\n')

# 6. BVAR (Base, com tendencia) para comparar.
bvar <- read_csv(file.path(raiz,'src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao/curva_jgb_2_5_10_30_cenarios.csv'),
  show_col_types=FALSE) |> filter(cenario=='Base') |> transmute(mes=yearmonth(date),nivel=100*spread30_5)
cat('BVAR Base 30-5:',paste(format(bvar$mes[c(1,nrow(bvar))]),round(bvar$nivel[c(1,nrow(bvar))])),'\n')

# 7. Grafico (trajetoria do mercado).
hist <- base |> as_tibble() |> filter(mes>=yearmonth('2022 Jan'),is.finite(s30_5)) |> select(mes,nivel=s30_5)
pm <- projecao |> filter(cenario=='Trajetoria do mercado')
cores <- c(`Fluxo | VAR`='#2a78d6',`Fluxo | ARIMA`='#7fb0ec',`Fluxo | ARIMA, UST so via fluxo`='#1f4f8a',
  `Fatia | VAR`='#e07b39',`Fatia | ARIMA`='#f2b48a',`Fatia | ARIMA, UST so via fatia`='#9c4a17',`BVAR (Base)`='grey20')
g <- ggplot()+geom_line(data=hist,aes(as.Date(mes),nivel),colour='grey25',linewidth=.8)+
  geom_line(data=bind_rows(pm |> select(mes,nivel,variante),bvar |> mutate(variante='BVAR (Base)')),
    aes(as.Date(mes),nivel,colour=variante,linetype=variante=='BVAR (Base)'),linewidth=.9)+
  scale_colour_manual(values=cores)+scale_linetype_manual(values=c(`FALSE`='solid',`TRUE`='dashed'),guide='none')+
  scale_x_date(date_breaks='1 year',date_labels='%Y')+theme_minimal(base_size=12)+
  theme(legend.position='bottom',panel.grid.minor=element_blank())+guides(colour=guide_legend(nrow=3))+
  labs(title='JGB 30Y-5Y (pb): modelo satelite por premissa estrangeira x BVAR',
    subtitle='Trajetoria do mercado (UST30 e OIS2Y a termo Bloomberg) | com constante de tendencia',
    x=NULL,y=NULL,colour=NULL)

write_csv(teste_estr |> select(-m),file.path(trabalho,'premissa estrangeira - teste ARIMA UST.csv'))
write_csv(coef_spread,file.path(trabalho,'premissa estrangeira - coeficientes 30-5.csv'))
write_csv(projecao |> mutate(mes=as.Date(mes)),file.path(trabalho,'premissa estrangeira - forecast 30-5.csv'))
ggsave(file.path(trabalho,'premissa estrangeira - forecast 30-5.png'),g,width=12,height=7.5,dpi=160,bg='white')
cat('CONCLUIDO\n')
