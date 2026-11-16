# Mesma amostra, tendencia, seis lags, pesos e identificacao nos modelos comparados.
estimar_configuracao <- function(d,tipo,chave) {
  vars <- c(variaveis,if(tipo %in% c('domestico','restrito')) 'fluxo')
  y <- as.matrix(select(d$banco,all_of(vars)))
  N <- ncol(y)
  tendencia <- matrix((seq_len(nrow(y))-nrow(y))/12,ncol=1)
  media_fluxo <- mean(d$banco$fluxo,na.rm=TRUE)
  x <- NULL
  if(tipo=='exogeno') {
    x <- d$aux |> select(date,fluxo_l2) |>
      right_join(select(d$banco,date),by='date') |> arrange(date)
    # Sem preencher meses anteriores com zero: todos usam inicio comum com lag 2.
    stopifnot(all(is.finite(x$fluxo_l2)))
    media_fluxo <- mean(x$fluxo_l2)
    x <- x$fluxo_l2-media_fluxo
  }
  assinatura <- list(tipo=tipo,chave=chave,y=y,x=x,vars=vars,
    P=P,RHO=RHO,S=S,HIPER_S=HIPER_S,HIPER_BURN=HIPER_BURN,SEMENTE=SEMENTE,
    ESS_MINIMO=ESS_MINIMO,versao=as.character(packageVersion('bsvarSIGNs')))
  pasta_cache <- file.path(pasta_testes,'resultados','cache',
    paste0(if(d$modo_cv) 'CV_' else 'forecast_',format(as.Date(d$origem),'%Y-%m')))
  dir.create(pasta_cache,recursive=TRUE,showWarnings=FALSE)
  arquivo <- file.path(pasta_cache,paste0(tipo,'.rds'))
  if(!REESTIMAR && file.exists(arquivo)) {
    salvo <- readRDS(arquivo)
    if(identical(salvo$assinatura,assinatura)) return(salvo)
  }
  ref <- readRDS(file.path(fonte_modelo,'modelo.rds'))
  sinais <- array(NA_real_,c(N,N,13))
  sinais[1:8,1:8,] <- ref$last_draw$identification$sign_irf
  # Nona inovacao residual sem rotulo de choque de fluxo e sem novos sinais.
  narrativas <- list()
  for(ev in list(c('2024-03-01','6'),c('2025-10-01','7'))) {
    inicio <- match(as.Date(ev[1]),as.Date(d$banco$date))
    if(!is.na(inicio) && inicio>P)
      narrativas <- c(narrativas,list(bsvarSIGNs::specify_narrative(
        start=inicio,periods=1,type='S',sign=1,shock=as.integer(ev[2]))))
  }
  covid <- match(as.Date('2020-03-01'),as.Date(d$banco$date))-P
  if(is.na(covid) || covid<=0 || nrow(y)-P-covid+1L<4L)
    stop('A configuracao exige amostra com a janela COVID completa.')
  spec <- bsvarSIGNs::specify_bsvarSIGN$new(data=y[,4:N,drop=FALSE],
    foreign=y[,1:3,drop=FALSE],
    exogenous=if(is.null(x)) tendencia else cbind(tendencia,x),p=P,
    hyper_mu=FALSE,hyper_delta=FALSE,hyper_lambda=FALSE,hyper_psi=FALSE,
    hyper_covid=covid,sign_irf=sinais,sign_narrative=narrativas,
    sign_structural=matrix(NA_real_,N,N))
  spec$prior$hyper[1:3,] <- c(1,1,.2)
  # Fluxo mensal e estacionario por hipotese: prior da primeira propria defasagem 0.
  if(N==9L) spec$prior$A[9,9] <- 0
  set.seed(SEMENTE)
  spec$estimate_hyper(S=HIPER_S,burn_in=HIPER_BURN)
  stopifnot(identical(as.integer(spec$prior$covid),as.integer(covid)))
  pesos <- RHO^rev(seq_len(ncol(spec$data_matrices$Y))-1L)
  spec$data_matrices$Y <- sweep(spec$data_matrices$Y,2,sqrt(pesos),'*')
  spec$data_matrices$X <- sweep(spec$data_matrices$X,2,sqrt(pesos),'*')
  spec$prior$Y <- spec$data_matrices$Y; spec$prior$X <- spec$data_matrices$X
  spec$identification$max_tries <- 10000L
  spec$mc.cores <- CORES
  restricoes <- NULL
  if(tipo=='exogeno')
    restricoes <- tibble(equacao=setdiff(1:8,7L),regressor=nrow(spec$data_matrices$X))
  if(tipo=='restrito') {
    # Macro -> fluxo livre; fluxo -> spread livre; fluxo -> outras domesticas zero.
    restricoes <- expand_grid(equacao=c(4L,5L,6L,8L),lag=seq_len(P)) |>
      transmute(equacao,regressor=(lag-1L)*N+9L)
  }
  set.seed(SEMENTE)
  if(is.null(restricoes)) {
    post <- bsvars::estimate(spec,S=S,show_progress=TRUE)
  } else if(CORES>1L) {
    post <- estimar_fluxo_paralelo(spec,S=S,cores=CORES,restricoes=restricoes,
      log=file.path(pasta_cache,paste0(tipo,' log.txt')))
  } else post <- estimar_fluxo_restrito(spec,S=S,restricoes=restricoes)
  post <- normalizar_fluxo(post,pesos)
  dom <- unlist(lapply(0:(P-1),function(l) l*N+4:N))
  ess <- post$posterior$ess
  distintos <- ncol(unique(matrix(post$posterior$A,ncol=S),MARGIN=2))
  diag <- tibble(tipo,sorteios=S,ess,distintos,
    feedback_externo_lags=max(abs(post$posterior$A[1:3,dom,,drop=FALSE])),
    feedback_externo_impacto=max(abs(post$posterior$Theta0[1:3,4:N,,drop=FALSE])))
  write_csv(diag,sub('\\.rds$',' diagnostico.csv',arquivo))
  if(!is.finite(ess) || ess<ESS_MINIMO || distintos<50L)
    stop('Posterior insuficiente: veja diagnostico; nenhum forecast foi publicado.')
  stopifnot(diag$feedback_externo_lags<1e-8,diag$feedback_externo_impacto<1e-8,
    all(is.finite(post$posterior$A)),
    max(abs(t(y[-seq_len(P),,drop=FALSE])-post$last_draw$data_matrices$Y))<1e-8)
  if(!is.null(restricoes)) for(r in seq_len(nrow(restricoes)))
    stopifnot(max(abs(post$posterior$A[restricoes$equacao[r],restricoes$regressor[r],]))<1e-8)
  irf <- bsvars::compute_impulse_responses(post,horizon=12,standardise=FALSE)
  stopifnot(all(irf[6,6,1:2,]>0),all(irf[7,6,1:2,]<0),
    all(irf[2,2,1:2,]>0),all(irf[3,2,2:7,]<0))
  salvo <- list(modelo=post,y=y,vars=vars,tipo=tipo,media_fluxo=media_fluxo,
    diagnostico=diag,assinatura=assinatura)
  saveRDS(salvo,arquivo)
  salvo
}
