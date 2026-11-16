Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
pasta <- file.path(raiz,'src/chat gpt/spreads JGB modelo satelite')
trabalho <- file.path(pasta,'resultados')
entrada <- readRDS(file.path(trabalho,'dados.rds'))
base <- entrada$base; dados <- entrada$dados; K <- 3L
# Parametros (opcionais na linha de comando): inicio da amostra e se tira a constante de
# tendencia. Padrao: amostra completa (jun/2018) e constante mantida.
# Ex.: Rscript "4- forecast.R" "2022 Jan" TRUE  -> arquivos com sufixo " 2022 Jan sem tendencia".
args <- commandArgs(trailingOnly=TRUE)
inicio_amostra <- if(length(args)>=1) yearmonth(args[1]) else min(dados$mes)
tirar_tendencia <- if(length(args)>=2) as.logical(args[2]) else FALSE
# Terceiro parametro opcional: 'fluxo' troca a fatia pela compra liquida (lag 2) no 30-10.
fluxo_30_10 <- length(args)>=3 && args[3]=='fluxo'
sufixo <- if(length(args)) paste0(' ',args[1],if(tirar_tendencia) ' sem tendencia' else ' com tendencia',if(fluxo_30_10) ' 30-10 com fluxo' else '') else ''
dados <- dados |> filter(mes>=inicio_amostra)
stopifnot(nrow(dados)>=36)
set.seed(20261030)

# 1. Horizonte: do mes seguinte ao ultimo JSDA ate o fim da curva a termo (24 meses).
curvas <- read_csv(file.path(pasta,'dados/curvas_bloomberg.csv'),col_types='ccDdd')
snap <- unique(curvas$data_snapshot); stopifnot(length(snap)==1L)
fim_mercado <- max(base$mes[is.finite(base$d_ust30)])
stopifnot(yearmonth(snap)==fim_mercado)
meses <- seq(max(dados$mes)+1L,fim_mercado+max(curvas$horizonte_meses),by=1)
futuros <- meses[meses>fim_mercado]; j <- as.integer(futuros-fim_mercado)
stopifnot(identical(j,seq_along(futuros)))

# 2. Caminho de mercado: FWCV Horizon Curve (taxa par do prazo do modelo iniciando em cada
# horizonte). Media do mes j = media dos horizontes j-1 e j; meses nao exportados por
# interpolacao linear. So as VARIACOES da curva entram, ancoradas no nivel diario do snapshot.
diario <- list(
  ust30=read_csv(file.path(pasta,'dados/DGS30_2026-11-04.csv'),
    col_types='Dd',na=c('','.')) |> transmute(date=observation_date,taxa=DGS30),
  ois2=read_csv(file.path(pasta,'dados/ois2_historico.csv'),
    col_types='Dd') |> transmute(date,taxa=ois2_pct))
curva <- function(x) { cx <- curvas |> filter(variavel==x) |> arrange(horizonte_meses)
  stopifnot(0 %in% cx$horizonte_meses,max(cx$horizonte_meses)>=max(j))
  approx(cx$horizonte_meses,cx$taxa_pct,xout=0:max(j))$y }
media_mes <- function(f) (head(f,-1)+f[-1])/2
nivel_mercado <- function(x) { f <- curva(x); spot <- diario[[x]] |> filter(date==snap) |> pull(taxa)
  stopifnot(length(spot)==1L,abs(spot-f[1])<0.05)
  spot+media_mes(f)-f[1] }

# 3. Nossa trajetoria do OIS2Y: o juro curto do BVAR (mediana Base) contra o overnight que o
# mercado espera (OIS 1M a termo). O OIS2Y e a media do overnight esperado nos 2 anos
# seguintes; entao o ajuste do mes j e a media da diferenca BVAR - mercado nos meses j a j+23.
# Depois do fim do BVAR (ago/2028), a ultima diferenca fica constante.
bvar <- read_csv(file.path(raiz,'src/Outputs/4- Forecast/todos cenarios oferta central/producao/resumo_forecast.csv'),
  show_col_types=FALSE) |> filter(cenario=='Base',variavel=='juro_curto') |>
  transmute(mes=yearmonth(date),curto_bvar=mediana)
curto <- tibble(mes=futuros,curto_mercado=media_mes(curva('ois_1m'))[j]) |>
  left_join(bvar,by='mes') |> mutate(diferenca=curto_bvar-curto_mercado) |> fill(diferenca)
diferenca <- c(curto$diferenca,rep(last(curto$diferenca),23L))
ajuste_ois <- map_dbl(j,~ mean(diferenca[.x:(.x+23L)]))

ultimo <- base |> as_tibble() |> filter(mes==fim_mercado)
realizado <- base |> as_tibble() |> filter(mes %in% meses,mes<=fim_mercado) |> select(mes,d_ust30,d_ois2)
caminho <- function(extra_ois) bind_rows(realizado,tibble(mes=futuros,
  d_ust30=100*diff(c(ultimo$ust30,nivel_mercado('ust30')[j])),
  d_ois2=100*diff(c(ultimo$ois2,nivel_mercado('ois2')[j]+extra_ois)))) |> as_tsibble(index=mes)
cenarios <- list(`Trajetoria do mercado`=caminho(0),`Nossa trajetoria do OIS (BVAR)`=caminho(ajuste_ois))

# 4. VAR: projeta so as variaveis estrangeiras que os spreads usam (compra liquida e fatia),
# dados UST30 e OIS2Y. Sem USD/JPY (piorou AICc e validacao cruzada). Defasagens por AICc.
var <- dados |> model(v=VAR(vars(liq_estr,d_fatia) ~ AR(p=1:2) + xreg(1,d_ust30,d_ois2),ic='aicc'))
report(var)

# 5. Spreads: modelos fechados (menor AICc). 30-10 com fatia (lag 3); 30-5 com compra
# liquida (lag 2). Constante na estimacao; FORA da projecao ('zera o B').
estrangeira_10 <- if(fluxo_30_10) 'lag(liq_estr, 2)' else 'lag(d_fatia, 3)'
spreads <- list(d_s30_10=c('d_ust30','d_ois2',estrangeira_10),d_s30_5=c('d_ust30','d_ois2','lag(liq_estr, 2)'))
nivel <- c(d_s30_10='s30_10',d_s30_5='s30_5')
modelos <- imap(spreads,function(r,alvo) dados |>
  mutate(!!alvo:=if_else(row_number()<=K,NA_real_,.data[[alvo]])) |>
  model(a=ARIMA(!!as.formula(paste(alvo,'~',paste(r,collapse=' + '),'+ pdq(p=0:3,d=0,q=0:3) + PDQ(0,0,0) + 1')),
    stepwise=FALSE,approximation=FALSE)))
iwalk(modelos,~ {cat('=====',.y,'\n'); report(.x)})
# Constante de tendencia = intercepto + beta da variavel estrangeira x sua media historica.
# Padrao: MANTER na projecao (tirar_tendencia = FALSE, ver parametros no topo). Com TRUE, ela
# sai e so compra/fatia acima ou abaixo do normal move o spread. Os juros tem efeito completo.
constante <- map_dbl(modelos,function(m) { t <- tidy(m)
  est <- t |> filter(str_detect(term,'liq_estr|d_fatia'))
  media <- map_dbl(str_remove(str_remove(est$term,'^lag\\('),',.*$'),~ mean(dados[[.x]],na.rm=TRUE))
  t$estimate[t$term=='intercept']+sum(est$estimate*media) })
print(round(constante,2))
constante <- constante*tirar_tendencia

projecao <- imap_dfr(cenarios,function(x,cenario) {
  m <- forecast(var,new_data=x)$.mean
  insumos <- as_tibble(x) |> mutate(liq_estr=m[,'liq_estr'],d_fatia=m[,'d_fatia'])
  imap_dfr(modelos,function(modelo,alvo) {
    nd <- insumos |> as_tsibble(index=mes)
    inicio <- base[[nivel[[alvo]]]][base$mes==max(dados$mes)]
    c_alvo <- constante[[alvo]]; nome <- alvo
    variacao <- forecast(modelo,new_data=nd)$.mean-c_alvo
    sim <- generate(modelo,new_data=nd,times=2000) |> as_tibble() |> arrange(.rep,mes) |>
      group_by(.rep) |> mutate(nivel=inicio+cumsum(.sim-c_alvo)) |> ungroup()
    sim |> group_by(mes) |> summarise(p025=quantile(nivel,.025),p10=quantile(nivel,.1),
      p90=quantile(nivel,.9),p975=quantile(nivel,.975),.groups='drop') |>
      mutate(cenario=cenario,alvo=nome,.before=1,variacao=variacao,nivel=inicio+cumsum(variacao)) |>
      left_join(insumos |> select(mes,d_ust30,d_ois2,liq_estr,d_fatia),by='mes')
  })
})
resumo <- projecao |> filter(mes %in% c(min(meses),min(meses)+11L,max(meses))) |>
  select(cenario,alvo,mes,nivel,p10,p90) |> mutate(across(c(nivel,p10,p90),~round(.x,1)))
print(resumo,n=Inf)

# Decomposicao da variacao acumulada ate o fim do horizonte (pb): beta x soma do regressor.
# Estrangeiras com defasagem usam o historico nos primeiros meses. Erro AR = o que sobra.
historico_x <- as_tibble(dados) |> select(mes,d_ust30,d_ois2,liq_estr,d_fatia)
decomposicao <- projecao |> distinct(cenario,alvo) |> pmap_dfr(function(cenario,alvo) {
  p <- projecao |> filter(cenario==!!cenario,alvo==!!alvo)
  serie <- bind_rows(historico_x,p |> select(mes,d_ust30,d_ois2,liq_estr,d_fatia))
  t <- tidy(modelos[[alvo]]) |> filter(!str_detect(term,'^(ar|ma)[0-9]+$'))
  contrib <- map_dbl(t$term,function(tt) {
    b <- t$estimate[t$term==tt]
    if(tt=='intercept') return(if(tirar_tendencia) 0 else b*nrow(p))
    v <- str_remove(str_remove(tt,'^lag\\('),',.*$')
    k <- if(str_detect(tt,'^lag')) as.integer(str_extract(tt,'[0-9]+(?=\\)$)')) else 0L
    xs <- serie[[v]][match(p$mes,serie$mes)-k]
    if(tirar_tendencia && v %in% c('liq_estr','d_fatia')) xs <- xs-mean(dados[[v]],na.rm=TRUE)
    b*sum(xs) })
  nomes <- c(intercept='Constante',d_ust30='UST30',d_ois2='OIS 2Y')
  rotulos <- coalesce(nomes[t$term],'Estrangeiros')
  tibble(cenario=cenario,alvo=alvo,componente=c(rotulos,'Dinamica do erro (AR)','Total'),
    pb=round(c(contrib,sum(p$variacao)-sum(contrib),sum(p$variacao)),1))
})
print(decomposicao |> pivot_wider(names_from=componente,values_from=pb))

# 6. Graficos: spreads (historico desde 2016) e insumos de cada cenario.
cores <- c(`Trajetoria do mercado`='#2166AC',`Nossa trajetoria do OIS (BVAR)`='#B2182B')
rotulo <- c(d_s30_10='JGB 30Y - 10Y (pb)',d_s30_5='JGB 30Y - 5Y (pb)')
hist <- base |> as_tibble() |> select(mes,d_s30_10=s30_10,d_s30_5=s30_5) |>
  pivot_longer(-mes,names_to='alvo',values_to='nivel') |> filter(is.finite(nivel))
g1 <- ggplot()+geom_ribbon(data=projecao,aes(as.Date(mes),ymin=p10,ymax=p90,fill=cenario),alpha=.15)+
  geom_line(data=hist,aes(as.Date(mes),nivel),colour='grey25')+
  geom_line(data=projecao,aes(as.Date(mes),nivel,colour=cenario),linewidth=1)+
  facet_wrap(vars(alvo),ncol=1,scales='free_y',labeller=as_labeller(rotulo))+
  scale_colour_manual(values=cores)+scale_fill_manual(values=cores)+
  scale_x_date(date_breaks='1 year',date_labels='%Y')+theme_minimal(base_size=12)+
  theme(legend.position='bottom',panel.grid.minor=element_blank())+
  labs(title='Forecast dos spreads JGB: VAR das variaveis estrangeiras + regressao com erros ARIMA',
    subtitle=paste('Amostra',format(min(dados$mes)),'-',format(max(dados$mes)),
      '| UST30 e OIS2Y a termo Bloomberg FWCV',format(snap),if(tirar_tendencia) '| sem a constante de tendencia | faixas de 80%' else '| com a constante de tendencia | faixas de 80%'),
    x=NULL,y=NULL,colour=NULL,fill=NULL,
    caption=paste0('30-10: UST30, OIS2Y e ',if(fluxo_30_10) 'compra liquida estrangeira (lag 2)' else 'fatia estrangeira (lag 3)','. 30-5: UST30, OIS2Y e compra liquida estrangeira (lag 2).\nFaixas condicionais aos insumos; nao incluem a incerteza das curvas nem do VAR.'))
nivel_ois <- projecao |> filter(alvo=='d_s30_10') |> group_by(cenario) |> arrange(mes,.by_group=TRUE) |>
  mutate(`OIS JPY 2Y (%)`=ultimo$ois2+cumsum(if_else(mes<=fim_mercado,0,d_ois2))/100,
    `UST 30Y (%)`=ultimo$ust30+cumsum(if_else(mes<=fim_mercado,0,d_ust30))/100) |> ungroup() |>
  select(cenario,mes,`OIS JPY 2Y (%)`,`UST 30Y (%)`,
    `Compra liquida estrangeira (tri) | VAR`=liq_estr,`Variacao da fatia (p.p.) | VAR`=d_fatia) |>
  pivot_longer(-c(cenario,mes))
g2 <- ggplot(nivel_ois,aes(as.Date(mes),value,colour=cenario))+geom_line(linewidth=.9)+
  facet_wrap(vars(name),ncol=2,scales='free_y')+scale_colour_manual(values=cores)+
  theme_minimal(base_size=11)+theme(legend.position='bottom')+
  labs(title='Insumos da projecao',x=NULL,y=NULL,colour=NULL)

# Armazenamento da saida em outputs.
saveRDS(list(projecao=projecao,cenarios=cenarios,var=var,modelos=modelos,ajuste_ois=ajuste_ois),
  file.path(trabalho,paste0('forecast',sufixo,'.rds')))
write_csv(projecao |> mutate(mes=as.Date(mes)),file.path(trabalho,paste0('forecast spreads',sufixo,'.csv')))
write_csv(decomposicao,file.path(trabalho,paste0('forecast decomposicao',sufixo,'.csv')))
write_csv(curto |> mutate(mes=as.Date(mes),ajuste_ois2=ajuste_ois),file.path(trabalho,paste0('forecast trajetoria OIS',sufixo,'.csv')))
ggsave(file.path(trabalho,paste0('forecast spreads',sufixo,'.png')),g1,width=12,height=9,dpi=160,bg='white')
ggsave(file.path(trabalho,paste0('forecast insumos',sufixo,'.png')),g2,width=12,height=9,dpi=160,bg='white')
cat('FORECAST_CONCLUIDO\n')
