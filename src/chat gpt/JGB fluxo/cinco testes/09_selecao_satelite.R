# Mesma estrutura de selecao do IP mundial, em FPP3.
# Familia A: contemporanea e lags. Familia B: apenas defasagens.
specs_satelite <- list(A_lag0=0,A_lag03=c(0,3),A_lag036=c(0,3,6),
  B_1=1,B_3=3,B_36=c(3,6),B_6=6,B_69=c(6,9),B_9=9,B_12=12)

preparar_lags_satelite <- function(dados,regressores) {
  dados <- dados |> as_tibble() |> arrange(date)
  for(x in regressores) for(j in 0:12)
    dados[[paste0(x,'_l',j)]] <- if(j==0L) dados[[x]] else lag(dados[[x]],j)
  dados
}

diagnosticar_satelite <- function(modelo) {
  residuos <- augment(modelo) |> filter(is.finite(.innov))
  dof <- sum(str_detect(tidy(modelo)$term,'^(ar|ma|sar|sma)[0-9]+$'))
  map_dfr(c(12L,24L),function(k) tibble(lag_teste=k,dof=dof,n=nrow(residuos),
    p_ljung_box=Box.test(residuos$.innov,lag=k,type='Ljung-Box',fitdf=dof)$p.value))
}

selecionar_satelite_ip <- function(dados,resposta,regressores,etapa) {
  # Os lags ja foram calculados com a historia disponivel antes do corte de treino.
  # Selecionar sempre com as MESMAS linhas, inclusive ao comparar regressores.
  todas_colunas <- unlist(lapply(regressores,function(x) paste0(x,'_l',0:12)))
  comum <- dados |> filter(if_all(all_of(c(resposta,todas_colunas)),is.finite)) |>
    as_tsibble(index=date)
  stopifnot(nrow(comum)>30L)
  testes_d <- map_dfr(c(resposta,regressores),function(x)
    tibble(variavel=x,ndiffs=feasts::unitroot_ndiffs(comum[[x]])[[1]]))
  testes_D <- map_dfr(c(resposta,regressores),function(x)
    tibble(variavel=x,nsdiffs=feasts::unitroot_nsdiffs(comum[[x]])[[1]]))
  d <- max(testes_d$ndiffs)
  stopifnot(d %in% 0:2)
  # Como no IP: d pelo diagnostico conjunto; p e q por AICc, sem sazonalidade.
  # A selecao de constante fica a cargo do ARIMA, nao se fixa beta ou intercepto.
  modelos <- list()
  testes <- map_dfr(regressores,function(x) {
    tabela <- map_dfr(names(specs_satelite),function(nome) {
      termos <- paste0(x,'_l',specs_satelite[[nome]])
      formula <- as.formula(paste(resposta,'~',paste(termos,collapse=' + '),
        '+ pdq(p=0:3,d=',d,',q=0:3) + PDQ(0,0,0)'))
      erro <- NULL
      m <- tryCatch(comum |> model(arima=ARIMA(!!formula,
        stepwise=FALSE,approximation=FALSE)),error=function(e) {
          erro <<- conditionMessage(e); NULL
        })
      valido <- !is.null(m) && !any(vapply(m$arima,inherits,logical(1),'null_mdl'))
      if(!valido) return(tibble(regressor=x,spec=nome,n_termos=length(termos),
        menor_lag=min(specs_satelite[[nome]]),AICc=Inf,BIC=Inf,
        p_lb12=NA_real_,p_lb24=NA_real_,erro=if(is.null(erro)) 'Ajuste invalido' else erro))
      modelos[[paste(x,nome)]] <<- m
      diag <- diagnosticar_satelite(m)
      message(etapa,' | ',x,' | ',nome)
      tibble(regressor=x,spec=nome,n_termos=length(termos),
        menor_lag=min(specs_satelite[[nome]]),AICc=glance(m)$AICc,BIC=glance(m)$BIC,
        p_lb12=diag$p_ljung_box[diag$lag_teste==12L],
        p_lb24=diag$p_ljung_box[diag$lag_teste==24L],erro=NA_character_)
    })
    if(!any(is.finite(tabela$AICc))) stop('Nenhuma especificacao valida: ',etapa,' / ',x)
    tabela |> mutate(delta_AICc=AICc-min(AICc,na.rm=TRUE),delta_BIC=BIC-min(BIC,na.rm=TRUE))
  })
  # Regra do IP/EUA: delta AICc < 2; menos termos; maior menor lag; menor AICc.
  escolhas <- testes |> filter(is.finite(AICc),delta_AICc<2) |> group_by(regressor) |>
    arrange(n_termos,desc(menor_lag),AICc,.by_group=TRUE) |>
    slice_head(n=1) |> ungroup()
  stopifnot(nrow(escolhas)==length(regressores))
  termos <- unlist(map2(escolhas$regressor,escolhas$spec,
    ~paste0(.x,'_l',specs_satelite[[.y]])))
  formula <- as.formula(paste(resposta,'~',paste(termos,collapse=' + '),
    '+ pdq(p=0:3,d=',d,',q=0:3) + PDQ(0,0,0)'))
  # Reajuste conjunto, recuperando as linhas com os lags efetivamente escolhidos.
  # AICc das estruturas acima permanece comparavel na amostra comum.
  final <- dados |> filter(if_all(all_of(c(resposta,termos)),is.finite)) |>
    as_tsibble(index=date)
  modelo <- final |> model(arima=ARIMA(!!formula,stepwise=FALSE,approximation=FALSE))
  stopifnot(!any(vapply(modelo$arima,inherits,logical(1),'null_mdl')))
  diagnosticos <- diagnosticar_satelite(modelo)
  if(any(diagnosticos$p_ljung_box<.05))
    warning(etapa,': modelo selecionado ainda apresenta autocorrelacao. Veja os diagnosticos; selecao por AICc nao garante residuos brancos.')
  list(modelo=modelo,formula=formula,escolhas=escolhas,testes=testes,
    testes_d=testes_d,testes_D=testes_D,d=d,diagnosticos=diagnosticos,
    amostra=tibble(etapa,n_selecao=nrow(comum),inicio_selecao=as.Date(min(comum$date)),
      n_final=nrow(final),inicio_final=as.Date(min(final$date)),fim=as.Date(max(final$date))))
}

grafico_residuos_satelite <- function(modelo,etapa) {
  diag <- diagnosticar_satelite(modelo)
  titulo <- if(etapa=='fluxo') 'Residuos: fluxo estrangeiro em superlongos' else
    'Residuos: spread JGB30Y-5Y do satelite'
  unidade <- if(etapa=='fluxo') 'trilhoes de ienes por mes' else 'pb por mes'
  pvalores <- paste0('Ljung-Box ',diag$lag_teste,'m: p = ',
    formatC(diag$p_ljung_box,format='f',digits=3,decimal.mark=','),collapse=' | ')
  # Inovacoes FINAIS: apos retirar regressores e estrutura ARIMA selecionada.
  g <- ggtime::gg_tsresiduals(modelo,type='innovation',plot_type='histogram',lag_max=24)
  g[[1]] <- g[[1]] + labs(title=titulo,
    subtitle=paste('Inovacoes apos o ARIMA |',pvalores),x=NULL,y=unidade)
  g[[2]] <- g[[2]] + labs(title='ACF das inovacoes',x='Defasagem',y='Autocorrelacao')
  g[[3]] <- g[[3]] + labs(title='Histograma das inovacoes',x=unidade,y='Frequencia')
  g
}

exportar_selecao_satelite <- function(sat,destino,graficos=TRUE,exibir=interactive()) {
  for(etapa in c('fluxo','spread')) {
    s <- sat$selecao[[etapa]]
    for(n in c('testes','escolhas','testes_d','testes_D','diagnosticos','amostra'))
      write_csv(s[[n]],file.path(destino,paste('selecao',etapa,n,'.csv')))
    capture.output(report(s$modelo),file=file.path(destino,paste0('modelo selecionado ',etapa,'.txt')))
    if(graficos) {
      g <- grafico_residuos_satelite(s$modelo,etapa)
      ggsave(file.path(destino,paste0('residuos ',etapa,'.png')),g,
        width=12,height=8,dpi=160,bg='white')
      if(exibir) print(g)
    }
  }
}
