Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
pasta <- file.path(raiz,'src/chat gpt/spreads JGB modelo satelite')
trabalho <- file.path(pasta,'resultados')
dir.create(trabalho,recursive=TRUE,showWarnings=FALSE)
inicio <- yearmonth('2016 Jan'); fim_mercado <- yearmonth('2026 Oct')

# 1. Mercado: medias mensais dos dados diarios, mesmo criterio do USD/JPY do BVAR.
# MoF: vencimento constante (nao ISIN). UST30 e USD/JPY do FRED; OIS2Y Bloomberg JYSO2.
media_mensal <- function(d,x) d |> filter(is.finite(.data[[x]])) |>
  group_by(mes=yearmonth(date)) |> summarise(!!x:=mean(.data[[x]]),.groups='drop')
mof <- read_csv(file.path(pasta,'dados/jgbcme_all_2026-11-04.csv'),
  skip=1,na=c('-',''),show_col_types=FALSE) |>
  transmute(date=ymd(Date,quiet=TRUE),s30_10=100*(`30Y`-`10Y`),s30_5=100*(`30Y`-`5Y`),jgb30=`30Y`) |>
  filter(!is.na(date))
ust <- read_csv(file.path(pasta,'dados/DGS30_2026-11-04.csv'),
  col_types='Dd',na=c('','.')) |> transmute(date=observation_date,ust30=DGS30)
ois <- read_csv(file.path(pasta,'dados/ois2_historico.csv'),
  col_types='Dd') |> transmute(date,ois2=ois2_pct)
fx <- read_csv(file.path(pasta,'dados/DEXJPUS_FRED_2026-11-11.csv'),
  col_types='Dd',na=c('','.')) |> transmute(date=observation_date,usdjpy=DEXJPUS)
# Juros curtos (medias mensais): Fed Funds efetivo (FRED) e call rate overnight sem
# garantia do BoJ (FM02/STRACLUCON). Base do custo de hedge cambial.
boj <- jsonlite::fromJSON(file.path(pasta,'dados/BOJ_STRACLUCON_2026-11-11.json'))
stopifnot(boj$STATUS==200,boj$RESULTSET$SERIES_CODE=='STRACLUCON')
curtos <- read_csv(file.path(pasta,'dados/FEDFUNDS_FRED_2026-11-11.csv'),col_types='Dd') |>
  transmute(mes=yearmonth(observation_date),fed_funds=FEDFUNDS) |>
  inner_join(tibble(mes=yearmonth(ym(boj$RESULTSET$VALUES$SURVEY_DATES[[1]])),
    call_jp=as.numeric(boj$RESULTSET$VALUES$VALUES[[1]])),by='mes')
mercado <- media_mensal(mof,'s30_10') |> full_join(media_mensal(mof,'s30_5'),by='mes') |>
  full_join(media_mensal(ust,'ust30'),by='mes') |> full_join(media_mensal(ois,'ois2'),by='mes') |>
  full_join(media_mensal(fx,'usdjpy'),by='mes') |> full_join(media_mensal(mof,'jgb30'),by='mes') |>
  left_join(curtos,by='mes') |>
  filter(mes>=inicio-1L,mes<=fim_mercado) |> arrange(mes) |>
  # Diferenciais para o fluxo estrangeiro, em pb (medias mensais):
  # sem hedge: UST30 - JGB30. Com hedge cambial: quem esta em dolar e faz hedge do iene
  # ganha o juro curto americano menos o japones; retorno extra do JGB30 sobre o UST30
  # = JGB30 + (Fed Funds - call rate) - UST30. Positivo: JGB30 com hedge paga mais.
  mutate(diferencial=100*(ust30-jgb30),diferencial_hedge=100*(jgb30+fed_funds-call_jp-ust30))
stopifnot(all(complete.cases(mercado)),nrow(mercado)==length(seq(inicio-1L,fim_mercado,by=1)))

# 2. JSDA superlongos (20/30/40Y e 15Y flutuante), compras menos vendas, tri de ienes.
# Formato comparavel so desde mai/2018: nao unir com publicacoes anteriores.
# Fatia: liquido dos estrangeiros / soma das compras liquidas POSITIVAS dos investidores
# finais (exclui Total, linha de variacao e Bond Dealers, que intermediam). O liquido
# Total fica perto de zero ou negativo; o denominador escolhido e sempre positivo.
jsda <- read_csv(file.path(pasta,'dados/fluxos_jsda_superlongos.csv'),
  show_col_types=FALSE) |> mutate(mes=yearmonth(mes))
excluir <- c('Total','Bond Dealers')
fluxos <- jsda |> filter(!investidor %in% excluir,!str_detect(investidor,'^Change')) |>
  group_by(mes) |> summarise(compradores_tri=sum(pmax(liquido_tri,0)),
    liq_estr=sum(liquido_tri[investidor=='Foreigners']),
    n_estr=sum(investidor=='Foreigners'),.groups='drop') |>
  left_join(jsda |> filter(investidor %in% c('Foreigners','Total')) |>
    select(mes,investidor,compra_tri) |> pivot_wider(names_from=investidor,values_from=compra_tri),by='mes') |>
  transmute(mes,liq_estr,compradores_tri,fatia_estr=100*liq_estr/compradores_tri,
    fatia_bruta_estr=100*Foreigners/Total,n_estr)
stopifnot(all(fluxos$n_estr==1),min(fluxos$compradores_tri)>0.3,min(fluxos$mes)==yearmonth('2018 May'))

# 3. Variacoes mensais. Spreads e juros em pb; USD/JPY em 100*dlog (%); fluxos em tri e p.p.
base <- mercado |> left_join(fluxos |> select(-n_estr),by='mes') |>
  mutate(d_s30_10=s30_10-lag(s30_10),d_s30_5=s30_5-lag(s30_5),
    d_ust30=100*(ust30-lag(ust30)),d_ois2=100*(ois2-lag(ois2)),
    d_usdjpy=100*(log(usdjpy)-log(lag(usdjpy))),
    d_liq_estr=liq_estr-lag(liq_estr),d_fatia=fatia_estr-lag(fatia_estr),
    d_diferencial=diferencial-lag(diferencial),d_jgb30=100*(jgb30-lag(jgb30)),
    # Fim do controle da curva (YCC) em 19/03/2024: primeiro mes inteiro sem YCC.
    pos_ycc=as.integer(mes>=yearmonth('2024 Apr'))) |>
  filter(mes>=inicio) |> as_tsibble(index=mes)
alvos <- c('d_s30_10','d_s30_5')
# Estrangeiros entram pela fatia (versao B). A compra liquida em nivel (versao A) ficou
# fora dos spreads: com as duas juntas, uma roubava efeito da outra. Ela segue na base,
# usada no modelo do fluxo (2b). USD/JPY saiu: p > 0,7 nos dois spreads.
preditores <- c('d_ust30','d_ois2','d_fatia')
# Amostra do modelo: primeiro mes com variacao JSDA ate o ultimo mes JSDA publicado.
dados <- base |> filter(if_all(all_of(c(alvos,preditores)),is.finite))
stopifnot(!has_gaps(dados)$.gaps,min(dados$mes)==yearmonth('2018 Jun'))
cat('Amostra do modelo:',format(min(dados$mes)),'a',format(max(dados$mes)),'|',nrow(dados),'meses\n')

# 4. Visualizacao (FPP3 cap. 2): niveis desde 2016, variacoes e ACF/PACF dos alvos.
niveis <- base |> as_tibble() |> select(mes,`JGB 30Y-10Y (pb)`=s30_10,`JGB 30Y-5Y (pb)`=s30_5,
  `UST 30Y (%)`=ust30,`OIS JPY 2Y (%)`=ois2,`USD/JPY`=usdjpy,`UST30 - JGB30 (pb)`=diferencial,`JGB30 com hedge - UST30 (pb)`=diferencial_hedge,
  `Compra liquida estrangeiros (tri ienes)`=liq_estr,`Fatia estrangeiros (%)`=fatia_estr) |>
  pivot_longer(-mes) |> filter(is.finite(value))
g1 <- ggplot(niveis,aes(as.Date(mes),value))+geom_line(colour='#2166AC')+
  facet_wrap(vars(name),ncol=2,scales='free_y')+theme_minimal(base_size=11)+
  labs(title='Series em nivel (medias mensais; JSDA desde mai/2018)',x=NULL,y=NULL)
variacoes <- dados |> as_tibble() |> select(mes,all_of(c(alvos,preditores))) |> pivot_longer(-mes)
g2 <- ggplot(variacoes,aes(as.Date(mes),value))+geom_line(colour='#B2182B')+
  facet_wrap(vars(name),ncol=2,scales='free_y')+theme_minimal(base_size=11)+
  labs(title='Variacoes mensais usadas no modelo',x=NULL,y=NULL)

# Armazenamento da saida em outputs.
ggsave(file.path(trabalho,'series em nivel.png'),g1,width=12,height=10,dpi=160,bg='white')
ggsave(file.path(trabalho,'series em variacao.png'),g2,width=12,height=10,dpi=160,bg='white')
for(y in alvos) ggsave(file.path(trabalho,paste0('acf pacf ',y,'.png')),
  dados |> gg_tsdisplay(!!sym(y),plot_type='partial'),width=10,height=7,dpi=160,bg='white')
write_csv(base |> mutate(mes=as.Date(mes)),file.path(trabalho,'base mensal.csv'))
write_csv(fluxos |> mutate(mes=as.Date(mes)),file.path(trabalho,'fluxos estrangeiros jsda.csv'))
saveRDS(list(base=base,dados=dados,alvos=alvos,preditores=preditores),file.path(trabalho,'dados.rds'))
cat('DADOS_CONCLUIDOS\n')
