# Mesmo estimador reduzido original, especializado no BVAR sem fluxo endogeno/exogeno.
estimar_reduzido_cv <- function(d,tipo='base',chave) {
  stopifnot(tipo=='base')
  arquivo <- file.path(raiz_cv_original,'cache',paste0('base_',format(as.Date(d$origem),'%Y-%m'),'.rds'))
  salvo <- cache_compativel(arquivo,chave)
  if(!is.null(salvo)) return(salvo)
  legado <- file.path(pasta_legada,'resultados/CV original/cache',basename(arquivo))
  if(!REESTIMAR && file.exists(legado)) {
    x <- readRDS(legado)
    if(fontes_legadas_compativeis(x$chave)) return(x$valor)
  }
  banco <- d$banco; vars <- variaveis
  validar_mensal(banco,vars,'BVAR reduzido original')
  y <- as.matrix(select(banco,all_of(vars))); N <- ncol(y)
  tendencia <- matrix((seq_len(nrow(y))-nrow(y))/12,ncol=1)
  inicio_covid <- match(as.Date('2020-03-01'),as.Date(banco$date))-P
  if(is.na(inicio_covid) || inicio_covid<=0L || nrow(y)-P-inicio_covid+1L<4L) inicio_covid <- NULL
  spec_cv <- bsvarSIGNs::specify_bsvarSIGN$new(data=y[,4:N,drop=FALSE],
    foreign=y[,1:3,drop=FALSE],exogenous=tendencia,p=P,
    hyper_mu=FALSE,hyper_delta=FALSE,hyper_lambda=FALSE,hyper_psi=FALSE,
    hyper_covid=inicio_covid,sign_structural=matrix(NA_real_,N,N))
  spec_cv$prior$hyper[1:3,] <- c(1,1,.2)
  Y_cv <- spec_cv$data_matrices$Y; X_cv <- spec_cv$data_matrices$X
  pesos <- RHO^rev(seq_len(ncol(Y_cv))-1L)
  spec_cv$data_matrices$Y <- sweep(Y_cv,2,sqrt(pesos),'*')
  spec_cv$data_matrices$X <- sweep(X_cv,2,sqrt(pesos),'*')
  spec_cv$prior$Y <- spec_cv$data_matrices$Y; spec_cv$prior$X <- spec_cv$data_matrices$X
  set.seed(123L+d$id+1000L)
  if(!is.null(inicio_covid)) spec_cv$estimate_hyper(S=CV_ORIGINAL_HIPER_S,burn_in=CV_ORIGINAL_BURN)
  spec_cv$mc.cores <- CORES
  post_cv <- bsvars::estimate(spec_cv,S=CV_ORIGINAL_S,show_progress=FALSE)
  stopifnot(all(is.finite(post_cv$posterior$A)),all(is.finite(post_cv$posterior$Sigma)),
    is.finite(post_cv$posterior$ess),post_cv$posterior$ess>=ESS_MINIMO)
  post_cv$last_draw$data_matrices$Y <- Y_cv; post_cv$last_draw$data_matrices$X <- X_cv
  post_cv$last_draw$prior$Y <- Y_cv; post_cv$last_draw$prior$X <- X_cv
  guardar_cache(arquivo,chave,list(modelo=post_cv,y=y,vars=vars,media_x=0,
    inicio=min(banco$date),ess=post_cv$posterior$ess))
}
