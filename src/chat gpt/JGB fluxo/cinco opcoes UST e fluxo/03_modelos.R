# Estima copias. Original, sinais, tendencia e identificacao sao preservados.
estimar_configuracao <- function(d,tipo,chave) {
  stopifnot(tipo %in% c('original_cv','ust30','diferencial','domestico','controle_fluxo',
    'fluxo_exogeno','fluxo_ust30','fluxo_diferencial'))
  usa_fluxo <- tipo %in% c('domestico','controle_fluxo','fluxo_exogeno',
    'fluxo_ust30','fluxo_diferencial')
  banco <- if(usa_fluxo) d$banco_fluxo else d$banco
  vars <- c(variaveis,if(tipo=='domestico') 'fluxo')
  y <- as.matrix(select(banco,all_of(vars)))
  N <- ncol(y)
  # 1. Matrizes dos blocos externo e domestico, como no script original.
  matrix_externa <- banco |> select(all_of(vars[1:3])) |> as.matrix()
  matrix_interna <- banco |> select(all_of(vars[4:N])) |> as.matrix()
  tendencia <- matrix((seq_len(nrow(y))-nrow(y))/12,ncol=1)
  # 6-8: fluxo contemporaneo como exogena, sozinho ou com UST30/diferencial.
  # Nao adiciona outra variavel endogena: continuam tres externas e cinco domesticas.
  x <- switch(tipo,
    ust30=cbind(ust30=banco$ust30),
    diferencial=cbind(diferencial=banco$ust30-banco$ust2y),
    fluxo_exogeno=cbind(fluxo=banco$fluxo),
    fluxo_ust30=cbind(fluxo=banco$fluxo,ust30=banco$ust30),
    fluxo_diferencial=cbind(fluxo=banco$fluxo,diferencial=banco$ust30-banco$ust2y),NULL)
  media_x <- if(is.null(x)) 0 else colMeans(x)
  if(!is.null(x)) {
    stopifnot(all(is.finite(x)))
    x <- sweep(x,2,media_x,'-')
  }
  chave_modelo <- list(execucao=chave,tipo=tipo,y=y,x=x,datas=banco$date,
    S=S,HIPER_S=HIPER_S,HIPER_BURN=HIPER_BURN)
  cache <- file.path(pasta_testes,'resultados','cache',
    paste0(if(d$modo_cv) 'CV_' else 'forecast_',format(as.Date(d$origem),'%Y-%m')))
  dir.create(cache,recursive=TRUE,showWarnings=FALSE)
  arquivo <- file.path(cache,paste0(tipo,'.rds'))
  if(!REESTIMAR && file.exists(arquivo)) {
    salvo <- readRDS(arquivo)
    if(identical(salvo$chave_modelo,chave_modelo)) return(salvo)
  }
  ref <- readRDS(file.path(fonte_modelo,'modelo.rds'))
  sinais <- array(NA_real_,c(N,N,13))
  sinais[1:8,1:8,] <- ref$last_draw$identification$sign_irf
  narrativas <- list()
  for(ev in list(c('2024-03-01','6'),c('2025-10-01','7'))) {
    pos <- match(as.Date(ev[1]),as.Date(banco$date))
    if(!is.na(pos) && pos>P) narrativas <- c(narrativas,
      list(bsvarSIGNs::specify_narrative(start=pos,periods=1,type='S',sign=1,shock=as.integer(ev[2]))))
  }
  covid <- match(as.Date('2020-03-01'),as.Date(banco$date))-P
  if(is.na(covid) || covid<=0L || nrow(y)-P-covid+1L<4L)
    stop('Treino sem janela COVID completa.')
  # 2. Especificacao original; so mudam a exogena ou a nona variavel domestica.
  spec <- bsvarSIGNs::specify_bsvarSIGN$new(data=matrix_interna,
    foreign=matrix_externa,
    exogenous=if(is.null(x)) tendencia else cbind(tendencia,x),p=P,
    hyper_mu=FALSE,hyper_delta=FALSE,hyper_lambda=FALSE,hyper_psi=FALSE,
    hyper_covid=covid,sign_irf=sinais,sign_narrative=narrativas,
    sign_structural=matrix(NA_real_,N,N))
  spec$prior$hyper[1:3,] <- c(1,1,.2)
  if(N==9L) spec$prior$A[9,9] <- 0 # Fluxo pode ser negativo: sem log e prior estacionario.
  # 3. Pesos por precisao, ANTES de recalibrar COVID, como no BVAR original.
  pesos <- RHO^rev(seq_len(ncol(spec$data_matrices$Y))-1L)
  spec$data_matrices$Y <- sweep(spec$data_matrices$Y,2,sqrt(pesos),'*')
  spec$data_matrices$X <- sweep(spec$data_matrices$X,2,sqrt(pesos),'*')
  spec$prior$Y <- spec$data_matrices$Y; spec$prior$X <- spec$data_matrices$X
  # 4. Hiperparametros COVID e posterior: mesma ordem do original.
  set.seed(SEMENTE)
  message('Estimando ',tipo,': ',min(banco$date),' a ',d$origem,' | N=',N)
  spec$estimate_hyper(S=HIPER_S,burn_in=HIPER_BURN)
  stopifnot(identical(as.integer(spec$prior$covid),as.integer(covid)))
  spec$identification$max_tries <- 10000L
  spec$mc.cores <- CORES
  # Novas exogenas: efeito livre nas CINCO domesticas, exatamente zero nas
  # tres estrangeiras. Restricao incorporada a distribuicao; nao zera apos estimar.
  restricoes <- if(is.null(x)) NULL else
    expand_grid(equacao=1:3,
      regressor=seq.int(nrow(spec$data_matrices$X)-ncol(x)+1L,nrow(spec$data_matrices$X)))
  set.seed(SEMENTE)
  if(is.null(restricoes)) post <- bsvars::estimate(spec,S=S,show_progress=TRUE)
  else if(CORES>1L) post <- estimar_fluxo_paralelo(spec,S=S,cores=CORES,
    restricoes=restricoes,log=file.path(cache,paste0(tipo,' log.txt')))
  else post <- estimar_fluxo_restrito(spec,S=S,restricoes=restricoes)
  post <- normalizar_fluxo(post,pesos)
  dom <- unlist(lapply(0:(P-1L),function(l) l*N+4:N))
  ess <- post$posterior$ess
  distintos <- ncol(unique(matrix(post$posterior$A,ncol=S),MARGIN=2))
  diagnostico <- tibble(tipo,n=nrow(y),inicio=as.Date(min(banco$date)),fim=as.Date(d$origem),
    sorteios=S,ess,distintos,
    feedback_externo_lags=max(abs(post$posterior$A[1:3,dom,,drop=FALSE])),
    feedback_externo_impacto=max(abs(post$posterior$Theta0[1:3,4:N,,drop=FALSE])))
  write_csv(diagnostico,file.path(cache,paste0(tipo,' diagnostico.csv')))
  if(!is.finite(ess) || ess<ESS_MINIMO || distintos<50L)
    stop('Posterior insuficiente (ESS=',round(ess,2),', distintos=',distintos,
      '). Nenhum forecast valido foi publicado. Veja ',cache)
  stopifnot(diagnostico$feedback_externo_lags<1e-8,
    diagnostico$feedback_externo_impacto<1e-8,all(is.finite(post$posterior$A)),
    max(abs(t(y[-seq_len(P),,drop=FALSE])-post$last_draw$data_matrices$Y))<1e-8)
  if(!is.null(restricoes)) for(r in seq_len(nrow(restricoes)))
    stopifnot(max(abs(post$posterior$A[restricoes$equacao[r],restricoes$regressor[r],]))<1e-8)
  irf <- bsvars::compute_impulse_responses(post,horizon=12,standardise=FALSE)
  stopifnot(all(irf[6,6,1:2,]>0),all(irf[7,6,1:2,]<0),
    all(irf[2,2,1:2,]>0),all(irf[3,2,2:7,]<0))
  salvo <- list(modelo=post,y=y,vars=vars,tipo=tipo,media_x=media_x,
    inicio=min(banco$date),diagnostico=diagnostico,chave_modelo=chave_modelo)
  saveRDS(salvo,arquivo)
  salvo
}

obter_original <- function(d,chave) {
  if(d$modo_cv) return(estimar_configuracao(d,'original_cv',chave))
  carregar_bvar_original(d)
}
