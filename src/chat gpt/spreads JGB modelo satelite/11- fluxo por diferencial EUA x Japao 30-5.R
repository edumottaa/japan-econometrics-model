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

# Estagio 1 do modelo "UST so via fluxo", trocando o UST30 em nivel pelo DIFERENCIAL de juros
# EUA x Japao: UST 2Y menos o juro curto japones.
#   Historico: UST 2Y = FRED DGS2 (cache do BVAR); Japao = (a) call rate (BoJ) ou (b) juro curto do
#   BVAR (taxa-sombra de Nakajima ate fev/2024, call rate depois).
#   Projecao: UST 2Y = caminho Base do BVAR (curva zero Bloomberg); Japao = juro curto do BVAR
#   (mediana Base). Desde mar/2024 o juro curto do BVAR e a propria call rate.
# Estagio 2 igual ao 10-: d_s30_5 ~ OIS2Y + compra liquida (lag 2) + constante + erros ARIMA.
bvar_dir <- file.path(raiz,'src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao')
ust2y_hist <- readRDS(file.path(raiz,'data/cache/cache_fred.rds')) |>
  transmute(mes=yearmonth(date),ust2y=DGS2) |> filter(is.finite(ust2y))
curto_hist <- readRDS(file.path(raiz,'data/cache/cache_domesticas_bvar.rds')) |>
  as_tibble() |> transmute(mes=yearmonth(date),curto_bvar=juro_curto) |> filter(is.finite(curto_bvar))
ust2y_proj <- read_csv(file.path(bvar_dir,'caminhos_externos.csv'),show_col_types=FALSE) |>
  filter(cenario=='Base') |> transmute(mes=yearmonth(date),ust2y_p=ust2y)
curto_proj <- read_csv(file.path(bvar_dir,'resumo_forecast.csv'),show_col_types=FALSE) |>
  filter(cenario=='Base',variavel=='juro_curto') |> transmute(mes=yearmonth(date),curto_p=mediana)

fim <- yearmonth('2028 Oct')
juros <- tibble(mes=seq(min(dados$mes)-3L,fim,by=1)) |>
  left_join(ust2y_hist,by='mes') |> left_join(ust2y_proj,by='mes') |>
  left_join(as_tibble(base) |> select(mes,call_jp),by='mes') |>
  left_join(curto_hist,by='mes') |> left_join(curto_proj,by='mes') |>
  mutate(fonte_ust=if_else(is.finite(ust2y),'FRED','BVAR Base'),
    ust2y=coalesce(ust2y,ust2y_p),
    call_jp=coalesce(call_jp,curto_p),curto_bvar=coalesce(curto_bvar,curto_p)) |>
  mutate(across(c(ust2y,call_jp,curto_bvar),~ zoo::na.approx(.x,na.rm=FALSE,rule=2))) |>
  transmute(mes,fonte_ust,ust2y,call_jp,curto_bvar,
    dif_call=ust2y-call_jp,dif_bvar=ust2y-curto_bvar) |>
  mutate(d_dif_call=100*difference(dif_call),d_dif_bvar=100*difference(dif_bvar))
print(juros |> filter(mes %in% yearmonth(c('2018 Jun','2022 Jun','2024 Jun','2026 Sep','2026 Oct','2027 Oct','2028 Sep','2028 Oct'))))

dados <- dados |> left_join(juros |> select(-fonte_ust),by='mes')
stopifnot(all(is.finite(dados$dif_call)),all(is.finite(dados$dif_bvar)))

# 1. Estagio 1: candidatos (nivel e variacao do diferencial, lag 0 a 3) + referencia UST30 nivel lag 2.
regs <- c(as.vector(outer(c('dif_call','dif_bvar','d_dif_call','d_dif_bvar'),0:3,
  function(v,k) if_else(k==0,v,paste0('lag(',v,', ',k,')')))),'lag(ust30, 2)')
ajusta <- function(x) dados |> mutate(liq_estr=if_else(row_number()<=K,NA_real_,liq_estr)) |>
  model(a=ARIMA(!!as.formula(paste('liq_estr ~',x,'+ pdq(p=0:3,d=0:1,q=0:3) + PDQ(0,0,0)')),stepwise=FALSE,approximation=FALSE))
lb <- function(m) { s <- m$a[[1]]$fit$spec
  augment(m) |> features(.innov,ljung_box,lag=12,dof=s$p+s$q) |> pull(lb_pvalue) }
teste <- tibble(regressor=regs) |> mutate(m=map(regressor,ajusta),
  arima=map_chr(m,~ format(.x$a[[1]])),AICc=map_dbl(m,~ glance(.x)$AICc),
  constante=map_lgl(m,~ any(tidy(.x)$term %in% c('intercept','constant'))),
  beta=map2_dbl(m,regressor,~ tidy(.x) |> filter(term==.y) |> pull(estimate) |> (\(b) if(length(b)) b else NA_real_)()),
  ep=map2_dbl(m,regressor,~ tidy(.x) |> filter(term==.y) |> pull(std.error) |> (\(b) if(length(b)) b else NA_real_)()),
  p_valor=map2_dbl(m,regressor,~ tidy(.x) |> filter(term==.y) |> pull(p.value) |> (\(b) if(length(b)) b else NA_real_)()),
  ljung_box_p=map_dbl(m,lb)) |> arrange(AICc)
print(teste |> select(-m) |> mutate(across(where(is.double),~signif(.x,3))),n=Inf,width=Inf)
write_csv(teste |> select(-m),file.path(trabalho,'fluxo por diferencial - teste.csv'))

# Melhor diferencial (menor AICc entre os candidatos de diferencial em NIVEL ou variacao).
melhor <- teste |> filter(!str_detect(regressor,'ust30')) |> slice(1)
cat('\nEscolhido:',melhor$regressor,'\n'); report(melhor$m[[1]])
print(tidy(melhor$m[[1]]) |> select(term,estimate,std.error,p.value) |> mutate(across(where(is.double),~signif(.x,3))))

# 2. Estagio 2 (igual ao 10-).
e2 <- dados |> mutate(d_s30_5=if_else(row_number()<=K,NA_real_,d_s30_5)) |>
  model(a=ARIMA(d_s30_5 ~ d_ois2 + lag(liq_estr, 2) + pdq(p=0:3,d=0,q=0:3) + PDQ(0,0,0) + 1,stepwise=FALSE,approximation=FALSE))

# 3. Caminhos de OIS2Y (mesma construcao do 8-) e projecao.
curvas <- read_csv(file.path(pasta,'dados/curvas_bloomberg.csv'),col_types='ccDdd')
snap <- unique(curvas$data_snapshot)
fim_mercado <- max(base$mes[is.finite(base$d_ust30)])
meses <- seq(max(dados$mes)+1L,fim_mercado+max(curvas$horizonte_meses),by=1)
futuros <- meses[meses>fim_mercado]; j <- as.integer(futuros-fim_mercado)
ois_diario <- read_csv(file.path(pasta,'dados/ois2_historico.csv'),col_types='Dd') |> transmute(date,taxa=ois2_pct)
curva <- function(x) { cx <- curvas |> filter(variavel==x) |> arrange(horizonte_meses)
  approx(cx$horizonte_meses,cx$taxa_pct,xout=0:max(j))$y }
media_mes <- function(f) (head(f,-1)+f[-1])/2
ois_mercado <- { f <- curva('ois2'); ois_diario$taxa[ois_diario$date==snap]+media_mes(f)-f[1] }
curto_merc <- tibble(mes=futuros,curto_mercado=media_mes(curva('ois_1m'))[j]) |>
  left_join(read_csv(file.path(raiz,'src/Outputs/4- Forecast/todos cenarios oferta central/producao/resumo_forecast.csv'),
    show_col_types=FALSE) |> filter(cenario=='Base',variavel=='juro_curto') |> transmute(mes=yearmonth(date),curto_bvar=mediana),by='mes') |>
  mutate(diferenca=curto_bvar-curto_mercado) |> fill(diferenca)
diferenca <- c(curto_merc$diferenca,rep(last(curto_merc$diferenca),23L))
ajuste_ois <- map_dbl(j,~ mean(diferenca[.x:(.x+23L)]))
cb <- curto_proj |> arrange(mes); ini <- cb |> slice(1); fim_c <- cb |> filter(mes==ini$mes+24L)
cagr <- (fim_c$curto_p/ini$curto_p)^(1/2)-1
ultimo <- base |> as_tibble() |> filter(mes==fim_mercado)
realizado <- base |> as_tibble() |> filter(mes %in% meses,mes<=fim_mercado) |> select(mes,d_ois2)
caminho <- function(nivel_ois) bind_rows(realizado,tibble(mes=futuros,d_ois2=100*diff(c(ultimo$ois2,nivel_ois)))) |>
  left_join(juros |> select(mes,dif_call,dif_bvar,d_dif_call,d_dif_bvar),by='mes') |>
  mutate(ust30=NA_real_) |> as_tsibble(index=mes)
cenarios <- list(`Trajetoria do mercado`=caminho(ois_mercado[j]),
  `Nossa trajetoria do OIS (BVAR)`=caminho(ois_mercado[j]+ajuste_ois),
  `OIS no CAGR do juro curto BVAR`=caminho(ultimo$ois2*(1+cagr)^(j/12)))

inicio <- base$s30_5[base$mes==max(dados$mes)]
projecao <- imap_dfr(cenarios,function(x,cenario) {
  x <- x |> mutate(liq_estr=forecast(melhor$m[[1]],new_data=x)$.mean)
  sim <- generate(e2,new_data=x,times=1000) |> as_tibble() |> arrange(.rep,mes) |>
    group_by(.rep) |> mutate(nivel=inicio+cumsum(.sim)) |> ungroup() |>
    group_by(mes) |> summarise(p10=quantile(nivel,.1),p90=quantile(nivel,.9),.groups='drop')
  as_tibble(x) |> transmute(mes,cenario=cenario,liq_estr,
    nivel=inicio+cumsum(forecast(e2,new_data=x)$.mean)) |> left_join(sim,by='mes') })
print(projecao |> filter(mes %in% yearmonth(c('2026 Oct','2027 Oct','2028 Oct'))) |>
  mutate(across(where(is.double),~round(.x,2))),n=Inf)
cat('Compra media projetada:',round(mean(projecao$liq_estr[projecao$mes>fim_mercado]),2),
  '| historica',round(mean(dados$liq_estr,na.rm=TRUE),2),'\n')

# 4. Grafico: historico + tres cenarios + BVAR.
bvar <- read_csv(file.path(bvar_dir,'curva_jgb_2_5_10_30_cenarios.csv'),show_col_types=FALSE) |>
  filter(cenario=='Base') |> transmute(mes=yearmonth(date),nivel=100*spread30_5,cenario='BVAR (Base)')
hist <- base |> as_tibble() |> filter(is.finite(s30_5),mes>=yearmonth('2018 Jun')) |> select(mes,nivel=s30_5)
cores <- c(`Trajetoria do mercado`='#2a78d6',`Nossa trajetoria do OIS (BVAR)`='#e07b39',
  `OIS no CAGR do juro curto BVAR`='#1baf9a',`BVAR (Base)`='grey30')
ultimos <- projecao |> group_by(cenario) |> slice_max(mes,n=1)
g <- ggplot()+
  geom_ribbon(data=projecao |> filter(cenario=='Trajetoria do mercado'),aes(as.Date(mes),ymin=p10,ymax=p90),fill='#2a78d6',alpha=.12)+
  geom_line(data=hist,aes(as.Date(mes),nivel),colour='grey15',linewidth=.8)+
  geom_line(data=bind_rows(projecao |> select(mes,nivel,cenario),bvar),
    aes(as.Date(mes),nivel,colour=cenario,linetype=cenario=='BVAR (Base)'),linewidth=1)+
  geom_text(data=ultimos,aes(as.Date(mes),nivel,label=round(nivel),colour=cenario),hjust=-.25,size=3.6,show.legend=FALSE)+
  annotate('text',x=as.Date(max(bvar$mes)),y=last(bvar$nivel),label=round(last(bvar$nivel)),hjust=-.25,vjust=1.4,size=3.6,colour='grey30')+
  scale_colour_manual(values=cores,breaks=names(cores))+scale_linetype_manual(values=c(`FALSE`='solid',`TRUE`='dashed'),guide='none')+
  scale_x_date(date_breaks='1 year',date_labels='%Y',expand=expansion(mult=c(.01,.06)))+
  theme_minimal(base_size=12)+theme(legend.position='bottom',panel.grid.minor=element_blank())+
  labs(title='JGB 30Y-5Y (pb): fluxo estrangeiro pelo diferencial de juros EUA x Japao',
    subtitle=paste0('Fluxo ~ ',melhor$regressor,' + erros ARIMA  |  Spread ~ OIS2Y + fluxo (lag 2) + constante + erros ARIMA'),
    x=NULL,y=NULL,colour=NULL,
    caption='Diferencial projetado: UST 2Y (caminho Base do BVAR) menos juro curto do BVAR. OIS2Y a termo Bloomberg (30/10/2026).\nFaixa: p10-p90 na trajetoria do mercado (so a incerteza do spread).')
ggsave(file.path(trabalho,'forecast 30-5 fluxo por diferencial.png'),g,width=12,height=7,dpi=160,bg='white')
write_csv(projecao |> mutate(mes=as.Date(mes)),file.path(trabalho,'forecast 30-5 fluxo por diferencial.csv'))
cat('CONCLUIDO\n')
