# Dados locais extraidos anteriormente. Nao baixa nem altera o BVAR principal.
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6', .libPaths()))
library(fpp3)
library(tidyverse)
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
pasta <- file.path(raiz, 'src/chat gpt/JGB fluxo')
dados <- file.path(raiz, 'src/chat gpt/spreads JGB modelo satelite/dados')
saida <- file.path(pasta, 'resultados')
dir.create(saida, recursive = TRUE, showWarnings = FALSE)
fonte_modelo <- file.path(raiz, 'data/processed/bvar_producao_tendencia_jgb30_5_20261112')
fonte_forecast <- file.path(raiz, 'src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao')
variaveis <- c('ust2y','brent','ip_mundial','atividade','cpi_core','juro_curto','inclinacao','usdjpy')

# 1. Medias mensais: taxas em %, spread em pb e fluxo em trilhoes de ienes.
mof <- read_csv(file.path(dados,'jgbcme_all_2026-11-04.csv'), skip=1,
                na=c('-',''), show_col_types=FALSE) |>
  transmute(date=ymd(Date), spread=100*(`30Y`-`5Y`))
ust <- read_csv(file.path(dados,'DGS30_2026-11-04.csv'), na=c('','.'), show_col_types=FALSE) |>
  transmute(date=observation_date, ust30=DGS30)
ois <- read_csv(file.path(dados,'ois2_historico.csv'), show_col_types=FALSE) |>
  rename(ois2=ois2_pct)
mensal <- function(x, nome) x |> filter(is.finite(.data[[nome]])) |>
  group_by(mes=yearmonth(date)) |> summarise(!!nome:=mean(.data[[nome]]), .groups='drop')
jsda <- read_csv(file.path(dados,'fluxos_jsda_superlongos.csv'), show_col_types=FALSE) |>
  filter(investidor=='Foreigners') |> transmute(mes=yearmonth(mes), fluxo=liquido_tri)
stopifnot(!anyDuplicated(jsda$mes), min(jsda$mes)==yearmonth('2018 May'))
base <- mensal(mof,'spread') |> inner_join(mensal(ust,'ust30'),by='mes') |>
  inner_join(mensal(ois,'ois2'),by='mes') |> left_join(jsda,by='mes') |>
  filter(mes>=yearmonth('2000 Jan'),mes<=yearmonth('2026 Oct')) |> arrange(mes) |>
  mutate(ust_l2=lag(ust30,2), fluxo_l2=lag(fluxo,2),
         d_spread=spread-lag(spread), d_ois=100*(ois2-lag(ois2)),
         ycc=as.integer(mes<=yearmonth('2024 Mar')), ois_pos=d_ois*(1-ycc))
inicio_amostra <- yearmonth('2018 Jun')
fim_amostra <- min(max(jsda$mes),yearmonth('2026 Sep'))

# 2. Curvas par FWCV em 30/10/2026. Interpolacao apenas entre pontos existentes.
curvas <- read_csv(file.path(dados,'curvas_bloomberg.csv'),show_col_types=FALSE)
snapshot <- unique(curvas$data_snapshot)
stopifnot(length(snapshot)==1, snapshot==as.Date('2026-10-30'))
origem_mercado <- yearmonth(snapshot)
fim_satelite <- origem_mercado+24L
meses_futuros <- seq(fim_amostra+1L,fim_satelite,by=1L)
curva_mensal <- function(nome, diario) {
  pontos <- curvas |> filter(variavel==nome) |> arrange(horizonte_meses)
  horizonte <- 0:24
  f <- approx(pontos$horizonte_meses,pontos$taxa_pct,xout=horizonte,rule=1)$y
  spot <- diario |> filter(date==snapshot) |> pull(all_of(nome))
  stopifnot(length(spot)==1,all(is.finite(f)))
  tibble(mes=origem_mercado+1:24, !!nome:=spot+(head(f,-1)+tail(f,-1))/2-f[1])
}
mercado <- full_join(curva_mensal('ust30',ust),curva_mensal('ois2',ois),by='mes')
futuro <- tibble(mes=meses_futuros) |>
  left_join(base |> select(mes,ust30,ois2),by='mes') |>
  left_join(mercado,by='mes',suffix=c('_real','_termo')) |>
  transmute(mes,ust30=coalesce(ust30_real,ust30_termo),ois2=coalesce(ois2_real,ois2_termo),
            tipo=if_else(mes<=origem_mercado,'Observado apos amostra','Curva a termo'))
futuro <- bind_rows(base |> filter(mes<=fim_amostra) |> select(mes,ust30,ois2),futuro) |>
  arrange(mes) |> mutate(ust_l2=lag(ust30,2),d_ois=100*(ois2-lag(ois2)),ois_pos=d_ois) |>
  filter(mes>fim_amostra) |> as_tsibble(index=mes)
stopifnot(all(is.finite(futuro$ust_l2)),all(is.finite(futuro$ois_pos)))
write_csv(base |> mutate(mes=as.Date(mes)),file.path(saida,'base auditada.csv'))
write_csv(futuro |> as_tibble() |> mutate(mes=as.Date(mes)),file.path(saida,'insumos satelite.csv'))
