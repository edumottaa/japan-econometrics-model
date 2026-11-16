# CV no procedimento do BVAR original. Source define funcoes; nao executa o CV.
# Forecasts prospectivos e seus caches permanecem inalterados.
source(file.path(pasta_testes,'amostragem_cv_original.R'),encoding='UTF-8')
CV_ORIGINAL_H <- 12L             # Horizonte solicitado pelo usuario.
CV_ORIGINAL_INIT <- 180L         # stretch_tsibble do CV original.
CV_ORIGINAL_PASSO <- 3L
CV_ORIGINAL_S <- 2500L
CV_ORIGINAL_HIPER_S <- 5000L
CV_ORIGINAL_BURN <- 2500L
domesticas_cv <- variaveis[4:8]
raiz_cv_original <- file.path(pasta_testes,'resultados','CV original')

chave_cv_original <- function(chave=chave_execucao()) {
  arquivos <- file.path(pasta_testes,c('cv_igual_bvar_original.R','amostragem_cv_original.R'))
  list(base=chave,codigo=tools::md5sum(arquivos),procedimento='BVAR reduzido original por periodo',
    init=CV_ORIGINAL_INIT,passo=CV_ORIGINAL_PASSO,h=CV_ORIGINAL_H,S=CV_ORIGINAL_S,
    hiper_S=CV_ORIGINAL_HIPER_S,burn=CV_ORIGINAL_BURN,
    sinais=FALSE,narrativas=FALSE,benchmark=c('Naive','Drift'))
}

ler_bases_cv_original <- function() {
  historico <- readRDS(file.path(fonte_modelo,'dados.rds')) |> as_tibble() |>
    select(date,all_of(variaveis)) |> mutate(date=yearmonth(date)) |> arrange(date)
  mensal <- function(x,nome) x |> filter(is.finite(.data[[nome]])) |>
    group_by(date=yearmonth(date)) |> summarise(!!nome:=mean(.data[[nome]]),.groups='drop')
  ust <- read_csv(file.path(fonte_dados,'DGS30_2026-11-04.csv'),na=c('','.'),show_col_types=FALSE) |>
    transmute(date=observation_date,ust30=DGS30)
  ois <- read_csv(file.path(fonte_dados,'ois2_historico.csv'),show_col_types=FALSE) |>
    transmute(date=as.Date(date),ois2=ois2_pct)
  jsda <- read_csv(file.path(fonte_dados,'fluxos_jsda_superlongos.csv'),show_col_types=FALSE) |>
    filter(investidor=='Foreigners') |> transmute(date=yearmonth(mes),fluxo=liquido_tri)
  aux <- full_join(mensal(ust,'ust30'),mensal(ois,'ois2'),by='date') |>
    full_join(jsda,by='date') |> arrange(date)
  # 1. Mesma grade de janelas do script original, inclusive janelas finais parciais.
  cv_data <- historico |> as_tsibble(index=date) |>
    stretch_tsibble(.init=CV_ORIGINAL_INIT,.step=CV_ORIGINAL_PASSO)
  origens <- cv_data |> as_tibble() |> group_by(.id) |>
    summarise(origem=max(date),.groups='drop') |> filter(origem<max(historico$date))
  list(historico=historico,aux=aux,jsda=jsda,cv_data=cv_data,origens=origens)
}

origens_cv_original <- function(numero,bases=ler_bases_cv_original()) {
  grade <- bases$origens
  # 1/2 nao usam fluxo: toda a grade original. 3-8 precisam da premissa pos-YCC.
  # Nao se usa a media observada depois de uma origem antiga para prever o passado.
  if(numero %in% 3:8) grade <- grade |> filter(origem>=yearmonth(POS_YCC))
  grade
}

janela_cv_original <- function(origem,bases) {
  banco <- bases$historico |> filter(date<=origem) |>
    left_join(select(bases$aux,date,ust30,ois2,fluxo),by='date')
  datas <- bases$historico$date[bases$historico$date>origem &
    bases$historico$date<=origem+CV_ORIGINAL_H]
  media <- bases$jsda |> filter(date>=yearmonth(POS_YCC),date<=origem)
  caminhos <- bases$historico |> filter(date %in% datas) |>
    mutate(across(all_of(domesticas_cv),~NA_real_),cenario='Ex post')
  list(id=bases$origens$.id[match(origem,bases$origens$origem)],
    banco=banco,banco_fluxo=filter(banco,date>=min(bases$jsda$date)),
    historico=bases$historico,aux=bases$aux,jsda=bases$jsda,origem=origem,
    datas=datas,caminhos=caminhos,modo_cv=TRUE,banda=NULL,
    futuro_aux=tibble(date=datas) |> left_join(select(bases$aux,date,ust30,ois2),by='date'),
    media_fluxo=if(nrow(media)) mean(media$fluxo) else NA_real_,n_media_fluxo=nrow(media))
}

estimar_reduzido_cv <- function(d,tipo,chave) {
  usa_fluxo <- tipo %in% c('domestico','fluxo_exogeno','fluxo_ust30','fluxo_diferencial')
  banco <- if(usa_fluxo) d$banco_fluxo else d$banco
  vars <- c(variaveis,if(tipo=='domestico') 'fluxo')
  validar_mensal(banco,c(vars,if(tipo %in% c('ust30','diferencial','fluxo_ust30','fluxo_diferencial')) 'ust30'),tipo)
  y <- as.matrix(select(banco,all_of(vars))); N <- ncol(y)
  x <- switch(tipo,ust30=cbind(ust30=banco$ust30),
    diferencial=cbind(diferencial=banco$ust30-banco$ust2y),
    fluxo_exogeno=cbind(fluxo=banco$fluxo),
    fluxo_ust30=cbind(fluxo=banco$fluxo,ust30=banco$ust30),
    fluxo_diferencial=cbind(fluxo=banco$fluxo,diferencial=banco$ust30-banco$ust2y),NULL)
  media_x <- if(is.null(x)) 0 else colMeans(x)
  if(!is.null(x)) x <- sweep(x,2,media_x,'-')
  arquivo <- file.path(raiz_cv_original,'cache',paste0(tipo,'_',format(as.Date(d$origem),'%Y-%m'),'.rds'))
  salvo <- cache_compativel(arquivo,chave)
  if(!is.null(salvo)) return(salvo)
  # 2. Mesmas matrizes, tendencia, priors, pesos e perfil COVID do CV original.
  matrix_externa <- y[,1:3,drop=FALSE]
  matrix_interna <- y[,4:N,drop=FALSE]
  tendencia <- matrix((seq_len(nrow(y))-nrow(y))/12,ncol=1)
  inicio_covid <- match(as.Date('2020-03-01'),as.Date(banco$date))-P
  if(is.na(inicio_covid) || inicio_covid<=0L || nrow(y)-P-inicio_covid+1L<4L)
    inicio_covid <- NULL
  spec_cv <- bsvarSIGNs::specify_bsvarSIGN$new(data=matrix_interna,foreign=matrix_externa,
    exogenous=if(is.null(x)) tendencia else cbind(tendencia,x),p=P,
    hyper_mu=FALSE,hyper_delta=FALSE,hyper_lambda=FALSE,hyper_psi=FALSE,
    hyper_covid=inicio_covid,sign_structural=matrix(NA_real_,N,N))
  # BVAR REDUZIDO: nao passa sinais ou narrativas ao construtor.
  spec_cv$prior$hyper[1:3,] <- c(1,1,.2)
  if(N==9L) spec_cv$prior$A[9,9] <- 0
  Y_cv <- spec_cv$data_matrices$Y; X_cv <- spec_cv$data_matrices$X
  pesos <- RHO^rev(seq_len(ncol(Y_cv))-1L)
  spec_cv$data_matrices$Y <- sweep(Y_cv,2,sqrt(pesos),'*')
  spec_cv$data_matrices$X <- sweep(X_cv,2,sqrt(pesos),'*')
  spec_cv$prior$Y <- spec_cv$data_matrices$Y; spec_cv$prior$X <- spec_cv$data_matrices$X
  id <- d$id
  set.seed(123L+id+1000L)
  if(!is.null(inicio_covid)) spec_cv$estimate_hyper(S=CV_ORIGINAL_HIPER_S,burn_in=CV_ORIGINAL_BURN)
  spec_cv$mc.cores <- CORES
  if(is.null(x)) post_cv <- bsvars::estimate(spec_cv,S=CV_ORIGINAL_S,show_progress=FALSE)
  else {
    restricoes <- expand_grid(equacao=1:3,
      regressor=seq.int(nrow(X_cv)-ncol(x)+1L,nrow(X_cv)))
    dir.create(dirname(arquivo),recursive=TRUE,showWarnings=FALSE)
    post_cv <- if(CORES>1L) estimar_cv_paralelo(spec_cv,S=CV_ORIGINAL_S,cores=CORES,
      log=sub('\\.rds$',' log.txt',arquivo),restricoes=restricoes) else
      estimar_cv_restrito(spec_cv,S=CV_ORIGINAL_S,restricoes=restricoes)
    for(r in seq_len(nrow(restricoes))) stopifnot(
      max(abs(post_cv$posterior$A[restricoes$equacao[r],restricoes$regressor[r],]))<1e-8)
  }
  stopifnot(all(is.finite(post_cv$posterior$A)),all(is.finite(post_cv$posterior$Sigma)),
    is.finite(post_cv$posterior$ess),post_cv$posterior$ess>=ESS_MINIMO)
  # Recuperar as unidades historicas antes de chamar forecast(), como no original.
  post_cv$last_draw$data_matrices$Y <- Y_cv; post_cv$last_draw$data_matrices$X <- X_cv
  post_cv$last_draw$prior$Y <- Y_cv; post_cv$last_draw$prior$X <- X_cv
  guardar_cache(arquivo,chave,list(modelo=post_cv,y=y,vars=vars,media_x=media_x,
    inicio=min(banco$date),ess=post_cv$posterior$ess))
}

forecast_reduzido_cv <- function(fit,d,id,xfuturo=NULL,spread_fixo=NULL,sinal_suave=NULL,fluxo_fixo=FALSE) {
  post <- fit$modelo; y <- fit$y; N <- ncol(y); H <- length(d$datas); p <- P
  tendencia_futura <- matrix(seq_len(H)/12,ncol=1)
  exogenas <- if(is.null(xfuturo)) tendencia_futura else cbind(tendencia_futura,xfuturo)
  condicoes <- cbind(as.matrix(select(d$caminhos,all_of(variaveis[1:3]))),matrix(NA_real_,H,N-3))
  if(!is.null(spread_fixo)) condicoes[,7] <- spread_fixo
  if(fluxo_fixo) condicoes[,9] <- d$media_fluxo
  set.seed(200000L+id+1000L)
  previsao_cv <- bsvarSIGNs::forecast(post,horizon=H,conditional_forecast=condicoes,
    exogenous_forecast=exogenas)
  # 3. Mesma correcao recursiva do script original; inclui os alvos adicionais
  # apenas nas opcoes que explicitamente condicionam spread ou fluxo.
  F <- previsao_cv$forecasts; medias <- previsao_cv$forecast_mean
  choques_cv <- F-medias
  fixos <- which(is.finite(condicoes[1,]))
  stopifnot(all(vapply(seq_len(H),function(j)
    identical(which(is.finite(condicoes[j,])),fixos),logical(1))))
  for(s in seq_len(dim(F)[3])) {
    estado_cv <- c(t(y[nrow(y)-(0:(p-1)),,drop=FALSE]),1,exogenas[1,])
    sigma <- post$posterior$Sigma[,,s]
    ganho <- sigma[,fixos,drop=FALSE]%*%solve(sigma[fixos,fixos,drop=FALSE])
    for(j in seq_len(H)) {
      media <- drop(post$posterior$A[,,s]%*%estado_cv)
      ajuste <- media[fixos]-medias[fixos,j,s]
      F[,j,s] <- media+choques_cv[,j,s]-drop(ganho%*%ajuste)
      F[fixos,j,s] <- condicoes[j,fixos]
      if(j<H) estado_cv <- c(F[,j,s],head(estado_cv,N*(p-1)),1,exogenas[j+1,])
    }
    if(!is.null(sinal_suave)) {
      # Sinal suave como observacao ruidosa da trajetoria, com covariancia
      # acumulada do satelite. Sorteios incluem inovacoes, alem de parametros.
      selecao <- diag(N)[fixos,,drop=FALSE]
      Tcond <- diag(N)-ganho%*%selecao
      A <- lapply(seq_len(p),function(l)
        Tcond%*%post$posterior$A[,(l-1)*N+seq_len(N),s,drop=FALSE][,,1])
      phi <- list(diag(N))
      if(H>1L) for(j in 1:(H-1L)) phi[[j+1L]] <- Reduce('+',lapply(seq_len(min(j,p)),
        function(l) A[[l]]%*%phi[[j-l+1L]]))
      G <- matrix(0,N*H,N*H)
      for(j in seq_len(H)) for(k in seq_len(j))
        G[(j-1L)*N+seq_len(N),(k-1L)*N+seq_len(N)] <- phi[[j-k+1L]]
      Q <- sigma-ganho%*%sigma[fixos,,drop=FALSE]; Q <- (Q+t(Q))/2
      escalas <- rep(1,H); covid <- post$last_draw$prior$covid
      if(!is.null(covid)) {
        hiper <- post$posterior$hyper[,s]
        escalas <- 1+(hiper[N+6]-1)*hiper[N+7]^(nrow(y)-p+seq_len(H)-covid-2)
      }
      V <- G%*%kronecker(diag(escalas^2),Q)%*%t(G)
      ix <- (seq_len(H)-1L)*N+7L; R <- sinal_suave$covariancia
      K <- V[,ix,drop=FALSE]%*%chol2inv(chol(V[ix,ix,drop=FALSE]+R))
      erro_sinal <- drop(t(chol(R))%*%rnorm(H))
      v <- as.vector(F[,,s])
      v <- v+drop(K%*%(sinal_suave$media-v[ix]-erro_sinal))
      F[,,s] <- matrix(v,N,H)
    }
  }
  stopifnot(all(is.finite(F)),
    max(abs(sweep(F[fixos,,,drop=FALSE],c(1,2),t(condicoes[,fixos,drop=FALSE]),'-')))<1e-7)
  F
}

distribuicoes_cv <- function(F,d,id,modelo) {
  map_dfr(domesticas_cv,function(alvo) {
    sorteios <- matrix(F[match(alvo,variaveis),,],nrow=length(d$datas))
    tibble(.id=id,.model=modelo,date=d$datas,origem=d$origem,h=seq_along(d$datas),
      variavel=alvo,valor=distributional::dist_sample(split(sorteios,row(sorteios)))) |>
      mutate(.mean=mean(valor))
  })
}

benchmarks_cv_original <- function(d,id) {
  treino <- d$banco |> select(date,all_of(domesticas_cv)) |> as_tsibble(index=date)
  map_dfr(domesticas_cv,function(alvo) {
    ajuste <- treino |> transmute(date,valor=.data[[alvo]]) |>
      model(Naive=NAIVE(valor),Drift=RW(valor~drift()))
    ajuste |> forecast(h=length(d$datas)) |> as_tibble() |>
      transmute(.id=id,.model,date,origem=d$origem,h=as.integer(date-d$origem),
        variavel=alvo,valor=unname(valor),.mean)
  })
}

executar_janela_original <- function(numero,d,id,chave) {
  tipo <- c('ust30','diferencial','base','base','domestico',
    'fluxo_exogeno','fluxo_ust30','fluxo_diferencial')[numero]
  fit <- estimar_reduzido_cv(d,tipo,chave)
  x <- NULL; alvo <- NULL; suave <- NULL
  if(numero %in% c(1:2,6:8)) {
    fluxo <- rep(d$media_fluxo,length(d$datas))
    x <- switch(as.character(numero),'1'=cbind(ust30=d$futuro_aux$ust30),
      '2'=cbind(diferencial=d$futuro_aux$ust30-d$caminhos$ust2y),
      '6'=cbind(fluxo=fluxo),'7'=cbind(fluxo=fluxo,ust30=d$futuro_aux$ust30),
      '8'=cbind(fluxo=fluxo,diferencial=d$futuro_aux$ust30-d$caminhos$ust2y))
    x <- sweep(x,2,fit$media_x,'-')
  }
  if(numero %in% 3:4) {
    sat <- estimar_satelite_teste(d)
    sinal <- montar_satelite_suave(sat,as.matrix(select(d$caminhos,all_of(variaveis))),d)
    if(numero==3L) suave <- sinal else alvo <- sinal$media
  }
  F <- forecast_reduzido_cv(fit,d,id,xfuturo=x,spread_fixo=alvo,
    sinal_suave=suave,fluxo_fixo=numero==5L)
  arquivo_base <- file.path(raiz_cv_original,'cache',paste0('previsao_base_',format(as.Date(d$origem),'%Y-%m'),'.rds'))
  referencia <- cache_compativel(arquivo_base,chave)
  if(is.null(referencia)) {
    base <- estimar_reduzido_cv(d,'base',chave)
    referencia <- distribuicoes_cv(forecast_reduzido_cv(base,d,id),d,id,'BVAR original ex post')
    guardar_cache(arquivo_base,chave,referencia)
  }
  bind_rows(distribuicoes_cv(F,d,id,nomes_testes[numero]),referencia,benchmarks_cv_original(d,id))
}

escores_cv_original <- function(previsoes,bases) {
  observados <- bases$historico |> select(date,all_of(domesticas_cv)) |>
    pivot_longer(-date,names_to='variavel',values_to='observado')
  escores <- previsoes |> left_join(observados,by=c('date','variavel')) |>
    mutate(erro=.mean-observado,periodo=case_when(year(date)<=2019~'2015-2019',
      year(date)<=2021~'2020-2021',TRUE~'2022-2026'))
  escores$crps <- NA_real_
  # Mesma identidade eficiente usada em graficos e benchmarks CV modelo novo.R.
  for(m in unique(escores$.model)) {
    ix <- which(escores$.model==m)
    pars <- distributional::parameters(escores$valor[ix])
    if(m %in% c('Naive','Drift')) {
      z <- (escores$observado[ix]-pars$mu)/pars$sigma
      escores$crps[ix] <- ifelse(pars$sigma>0,
        pars$sigma*(z*(2*pnorm(z)-1)+2*dnorm(z)-1/sqrt(pi)),
        abs(escores$observado[ix]-pars$mu))
    } else escores$crps[ix] <- map2_dbl(pars$x,escores$observado[ix],function(x,y) {
      n <- length(x)
      mean(abs(x-y))-sum((2*seq_len(n)-n-1)*sort(x))/n^2
    })
  }
  stopifnot(all(is.finite(escores$erro)),all(is.finite(escores$crps)))
  escores
}

metricas_cv_original <- function(escores) {
  resumir <- function(x,grupos) x |> group_by(across(all_of(grupos))) |>
    summarise(origens=n_distinct(origem),casos=n(),RMSE=sqrt(mean(erro^2)),
      MAE=mean(abs(erro)),CRPS=mean(crps),.groups='drop')
  bind_rows(resumir(escores,c('teste','variavel','.model')) |> mutate(periodo='Geral'),
    resumir(escores,c('teste','variavel','periodo','.model'))) |>
    arrange(variavel,periodo,RMSE)
}

# Sobrescreve apenas os pontos de entrada do CV. O forecast prospectivo e intacto.
executar_cv <- function(numero,chave=chave_execucao()) {
  chave <- chave_cv_original(chave); bases <- ler_bases_cv_original()
  grade <- origens_cv_original(numero,bases)
  destino <- file.path(raiz_cv_original,paste0('opcao_',numero))
  dir.create(destino,recursive=TRUE,showWarnings=FALSE)
  salvo <- cache_compativel(file.path(destino,'resultado.rds'),chave)
  if(!is.null(salvo) && isTRUE(salvo$meta$completo)) return(invisible(salvo))
  previsoes <- status <- list()
  for(k in seq_len(nrow(grade))) {
    o <- grade$origem[k]; id <- grade$.id[k]
    arquivo <- file.path(destino,paste0('origem_',format(as.Date(o),'%Y-%m'),'.rds'))
    valor <- cache_compativel(arquivo,chave)
    erro <- NA_character_
    if(is.null(valor)) erro <- tryCatch({
      d <- janela_cv_original(o,bases)
      valor <- executar_janela_original(numero,d,id,chave)
      guardar_cache(arquivo,chave,valor)
      NA_character_
    },error=conditionMessage)
    if(is.na(erro)) previsoes[[k]] <- valor
    status[[k]] <- tibble(origem=as.Date(o),status=if(is.na(erro)) 'Concluido' else 'Falhou',erro=erro)
    write_csv(bind_rows(status),file.path(destino,'status por origem.csv'))
    message('CV original | opcao ',numero,' | ',k,'/',nrow(grade),' | ',o)
  }
  prev <- bind_rows(previsoes) |> mutate(teste=numero)
  if(!nrow(prev)) stop('Nenhuma janela concluida no CV original da opcao ',numero)
  escores <- escores_cv_original(prev,bases)
  meta <- list(teste=as.integer(numero),origens_esperadas=as.Date(grade$origem),
    completo=all(bind_rows(status)$status=='Concluido'),status=bind_rows(status),
    procedimento='BVAR reduzido, sem sinais/narrativas; janelas expansivas trimestrais',
    horizonte=CV_ORIGINAL_H)
  guardar_cache(file.path(destino,'resultado.rds'),chave,
    list(meta=meta,previsoes=prev,escores=escores,metricas=metricas_cv_original(escores)))
  write_csv(select(escores,-valor),file.path(destino,'previsoes e escores.csv'))
  write_csv(metricas_cv_original(escores),file.path(destino,'metricas por periodo.csv'))
  if(!meta$completo) stop('CV original incompleto da opcao ',numero,': veja status por origem.csv.')
  invisible(escores)
}

comparar_cv <- function(chave=chave_execucao()) {
  chave <- chave_cv_original(chave)
  arquivos <- file.path(raiz_cv_original,paste0('opcao_',seq_along(nomes_testes)),'resultado.rds')
  resultados <- lapply(arquivos,function(a) cache_compativel(a,chave,TRUE)) |> compact()
  if(!length(resultados)) return(invisible(NULL))
  destino <- file.path(raiz_cv_original,'comparacao')
  dir.create(destino,recursive=TRUE,showWarnings=FALSE)
  escores <- bind_rows(lapply(resultados,`[[`,'escores'))
  write_csv(metricas_cv_original(escores),file.path(destino,'metricas por periodo - disponiveis.csv'))
  cobertura <- bind_rows(lapply(resultados,function(x) tibble(teste=x$meta$teste,
    completo=x$meta$completo,esperadas=length(x$meta$origens_esperadas),
    concluidas=sum(x$meta$status$status=='Concluido'))))
  write_csv(cobertura,file.path(destino,'cobertura.csv'))
  # Comparacao por periodo em pares identicos de origem/data/horizonte/variavel.
  # Nao cria outro CV nem reestima os modelos sem fluxo: recorta os escores salvos.
  presentes <- sort(unique(escores$teste))
  chaves <- escores |> filter(.model==nomes_testes[teste]) |>
    distinct(teste,.id,date,h,variavel) |> count(.id,date,h,variavel,name='modelos') |>
    filter(modelos==length(presentes)) |> select(-modelos)
  comum <- escores |> semi_join(chaves,by=c('.id','date','h','variavel'))
  if(!nrow(comum)) return(invisible(NULL))
  # Tirar duplicatas da referencia e benchmarks compartilhados entre opcoes.
  comum <- comum |> select(-teste) |> distinct(.id,.model,date,variavel,.keep_all=TRUE) |>
    mutate(teste=if_else(.model %in% nomes_testes,match(.model,nomes_testes),0L))
  metricas <- metricas_cv_original(comum)
  write_csv(metricas,file.path(destino,'metricas por periodo - mesmas janelas.csv'))
  write_csv(select(comum,-valor),file.path(destino,'escores mesmas janelas.csv'))
  for(alvo in domesticas_cv) {
    unidade <- if(alvo %in% c('inclinacao','juro_curto')) 'p.p.' else '100 x log do nivel'
    g <- metricas |> filter(variavel==alvo) |>
      mutate(periodo=factor(periodo,levels=c('Geral','2015-2019','2020-2021','2022-2026'))) |>
      ggplot(aes(RMSE,reorder(.model,-RMSE),fill=.model))+geom_col(width=.6)+
      geom_text(aes(label=formatC(RMSE,digits=2,format='f',decimal.mark=',')),hjust=-.1,size=3)+
      facet_wrap(vars(periodo),ncol=2)+scale_x_continuous(expand=expansion(mult=c(0,.25)))+
      theme_minimal()+theme(legend.position='none',panel.grid.major.y=element_blank())+
      labs(title=paste('CV por periodo |',alvo),x=paste('RMSE |',unidade),y=NULL,
        subtitle=paste('BVAR reduzido original | horizontes observados 1-12 |',length(presentes),'opcoes'),
        caption='Mesmas origens e datas entre modelos. Variaveis em unidades do modelo; nao taxas de crescimento anual. Menor = melhor.')
    ggsave(file.path(destino,paste0('CV ',alvo,' por periodo.png')),g,width=12,height=8,dpi=160,bg='white')
    if(MOSTRAR_GRAFICOS) print(g)
  }
  invisible(metricas)
}
