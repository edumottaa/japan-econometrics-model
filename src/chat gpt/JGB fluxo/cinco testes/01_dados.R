# Leitura dos insumos. Nao escreve nas pastas do modelo principal.
ler_insumos <- function() {
  historico <- readRDS(file.path(fonte_modelo,'dados.rds')) |> as_tibble() |>
    mutate(date=yearmonth(date)) |> arrange(date)
  origem <- if(is.null(ORIGEM)) max(historico$date) else yearmonth(ORIGEM)
  stopifnot(origem %in% historico$date)
  jsda <- read_csv(file.path(fonte_dados,'fluxos_jsda_superlongos.csv'),show_col_types=FALSE) |>
    filter(investidor=='Foreigners') |> transmute(date=yearmonth(mes),fluxo=liquido_tri)
  stopifnot(!anyDuplicated(jsda$date))
  mensal <- function(x,nome) x |> filter(is.finite(.data[[nome]])) |>
    group_by(date=yearmonth(date)) |>
    summarise(!!nome:=mean(.data[[nome]]),.groups='drop')
  ust <- read_csv(file.path(fonte_dados,'DGS30_2026-11-04.csv'),na=c('','.'),show_col_types=FALSE) |>
    transmute(date=observation_date,ust30=DGS30)
  ois <- read_csv(file.path(fonte_dados,'ois2_historico.csv'),show_col_types=FALSE) |>
    rename(ois2=ois2_pct)
  aux <- full_join(mensal(ust,'ust30'),mensal(ois,'ois2'),by='date') |>
    full_join(jsda,by='date') |> filter(date<=yearmonth('2026 Oct')) |> arrange(date) |>
    mutate(ust_l2=lag(ust30,2),fluxo_l2=lag(fluxo,2),
      ois_pos=100*(ois2-lag(ois2))*as.integer(date>yearmonth('2024 Mar')))
  banco <- historico |> left_join(select(aux,date,fluxo),by='date') |>
    filter(date>=yearmonth(INICIO),date<=origem)
  stopifnot(!any(has_gaps(as_tsibble(banco,index=date))$.gaps),
    all(is.finite(as.matrix(select(banco,all_of(c(variaveis,'fluxo')))))),
    nrow(banco)>P+20L)
  historico_teste <- origem<max(historico$date)
  config_salva <- readRDS(file.path(fonte_forecast,'forecast_bvar.rds'))
  if(!historico_teste) {
    caminhos <- readRDS(file.path(fonte_forecast,'caminhos_novos.rds')) |> as_tibble() |>
      mutate(date=yearmonth(date)) |> filter(date>origem)
    if(!is.null(HORIZONTE)) caminhos <- filter(caminhos,date<=origem+HORIZONTE)
    datas <- sort(unique(caminhos$date))
    banda <- BANDA_JURO
    tipo_externos <- 'Cenarios salvos; snapshot Bloomberg 30/10/2026 e nowcasts salvos'
  } else {
    h <- if(is.null(HORIZONTE)) 24L else as.integer(HORIZONTE)
    datas <- seq(origem+1L,origem+h,by=1L)
    stopifnot(max(datas)<=max(historico$date))
    # Backtest condicional, nao uma simulacao de informacao disponivel em tempo real.
    caminhos <- historico |> filter(date %in% datas) |>
      select(date,all_of(variaveis)) |>
      mutate(across(all_of(variaveis[4:8]),~NA_real_),
        cenario='Historico: externos realizados')
    banda <- BANDA_JURO # Restricao solicitada, tambem em eventual origem historica.
    tipo_externos <- 'Historico condicional a externos realizados; nao real time'
  }
  stopifnot(length(datas)>0L,
    identical(datas,seq(origem+1L,max(datas),by=1L)))
  # Trajetorias UST30/OIS do satelite. No historico: realizados, explicitamente.
  futuro_aux <- tibble(date=datas) |> left_join(select(aux,date,ust30,ois2),by='date')
  if(!historico_teste) {
    curvas <- read_csv(file.path(fonte_dados,'curvas_bloomberg.csv'),show_col_types=FALSE)
    snapshot <- unique(curvas$data_snapshot)
    stopifnot(length(snapshot)==1L,snapshot==as.Date('2026-10-30'))
    for(v in c('ust30','ois2')) {
      pontos <- curvas |> filter(variavel==v) |> arrange(horizonte_meses)
      f <- approx(pontos$horizonte_meses,pontos$taxa_pct,xout=0:24,rule=1)$y
      diario <- if(v=='ust30') ust else ois
      spot <- diario[[v]][diario$date==snapshot]
      stopifnot(length(spot)==1L,all(is.finite(f)))
      termo <- tibble(date=yearmonth(snapshot)+1:24,
        valor=spot+(head(f,-1)+tail(f,-1))/2-f[1])
      ix <- match(futuro_aux$date,termo$date)
      futuro_aux[[v]] <- coalesce(futuro_aux[[v]],termo$valor[ix])
    }
  }
  stopifnot(all(is.finite(futuro_aux$ust30)),all(is.finite(futuro_aux$ois2)))
  if(MODO_CV_ATIVO) banda <- NULL
  list(banco=banco,historico=historico,aux=aux,jsda=jsda,origem=origem,datas=datas,
    caminhos=caminhos,banda=banda,futuro_aux=futuro_aux,
    tipo_externos=tipo_externos,historico_teste=historico_teste,modo_cv=MODO_CV_ATIVO)
}

estimar_satelite_teste <- function(d) {
  historia <- d$historico |> transmute(date,spread=100*inclinacao) |>
    left_join(d$aux,by='date') |> arrange(date) |>
    mutate(d_spread=spread-lag(spread)) |> filter(date<=d$origem)
  # 1. Estruturas A/B, d por diagnostico e p/q por AICc, como no IP mundial.
  treino_fluxo <- preparar_lags_satelite(filter(d$aux,date<=d$origem),'ust30') |>
    filter(date>=yearmonth(INICIO),date<=d$origem)
  sel_fluxo <- selecionar_satelite_ip(treino_fluxo,'fluxo','ust30','Fluxo')
  m1 <- sel_fluxo$modelo
  insumos_completos <- bind_rows(filter(d$aux,date<=d$origem) |>
      select(date,ust30,ois2),d$futuro_aux) |>
    arrange(date) |> mutate(
      ois_pos=100*(ois2-lag(ois2))*as.integer(date>yearmonth('2024 Mar'))) |>
    preparar_lags_satelite('ust30')
  insumos <- insumos_completos |> filter(date %in% d$datas) |> as_tsibble(index=date)
  fluxo <- forecast(m1,new_data=insumos) |> as_tibble() |>
    transmute(date,fluxo=.mean)
  completo <- bind_rows(filter(d$jsda,date<=d$origem),fluxo) |> arrange(date) |>
    mutate(fluxo_l2=lag(fluxo,2))
  # 2. Selecao tambem na equacao do spread: estruturas de OIS pos-YCC e fluxo.
  # Historico tem fluxo realizado; futuro recebe exclusivamente o fluxo projetado.
  base_spread <- bind_rows(
    historia |> select(date,d_spread,ois_pos,fluxo),
    insumos |> as_tibble() |> select(date,ois_pos) |>
      left_join(select(fluxo,date,fluxo),by='date') |> mutate(d_spread=NA_real_)) |>
    preparar_lags_satelite(c('ois_pos','fluxo'))
  treino_spread <- base_spread |> filter(date>=yearmonth(INICIO),date<=d$origem)
  sel_spread <- selecionar_satelite_ip(treino_spread,'d_spread',c('ois_pos','fluxo'),'Spread')
  m2 <- sel_spread$modelo
  novos <- base_spread |> filter(date %in% d$datas) |> as_tsibble(index=date)
  sat <- forecast(m2,new_data=novos) |> as_tibble() |>
    transmute(date,delta_pb=.mean,
      spread_pb=100*tail(d$banco$inclinacao,1)+cumsum(delta_pb))
  # A estrutura ARIMA agora e selecionada: simular para preservar a covariancia
  # dos erros futuros de Delta spread, inclusive AR/MA e eventual diferenciacao.
  stopifnot(SATELITE_SIMULACOES>length(d$datas)+1L)
  set.seed(SEMENTE)
  simulacoes <- generate(m2,new_data=novos,times=SATELITE_SIMULACOES) |>
    as_tibble() |> arrange(.rep,date)
  stopifnot(nrow(simulacoes)==length(d$datas)*SATELITE_SIMULACOES,
    all(is.finite(simulacoes$.sim)))
  matriz_delta <- matrix(simulacoes$.sim,ncol=length(d$datas),byrow=TRUE)
  list(estagio1=m1,estagio2=m2,fluxo=fluxo,completo=completo,satelite=sat,
    selecao=list(fluxo=sel_fluxo,spread=sel_spread),
    cov_delta_pb2=cov(matriz_delta),novos_spread=novos)
}

# Testes 1, 2 e 6 compartilham o MESMO satelite quando dados/opcoes coincidem.
obter_satelite_teste <- function(d) {
  assinatura <- list(origem=d$origem,datas=d$datas,inicio=INICIO,semente=SEMENTE,
    simulacoes=SATELITE_SIMULACOES,
    historia=d$historico |> filter(date<=d$origem) |> select(date,inclinacao),
    aux=d$aux |> filter(date<=d$origem),futuro=d$futuro_aux,
    codigo=tools::md5sum(file.path(pasta_testes,c('01_dados.R','09_selecao_satelite.R'))))
  pasta_cache <- file.path(pasta_testes,'resultados','cache')
  dir.create(pasta_cache,recursive=TRUE,showWarnings=FALSE)
  arquivo <- file.path(pasta_cache,paste0('satelite_',if(d$modo_cv) 'CV_' else 'forecast_',
    format(as.Date(d$origem),'%Y-%m'),'.rds'))
  if(!REESTIMAR && file.exists(arquivo)) {
    salvo <- readRDS(arquivo)
    if(identical(salvo$assinatura,assinatura)) return(salvo$satelite)
  }
  sat <- estimar_satelite_teste(d)
  saveRDS(list(assinatura=assinatura,satelite=sat),arquivo)
  sat
}
