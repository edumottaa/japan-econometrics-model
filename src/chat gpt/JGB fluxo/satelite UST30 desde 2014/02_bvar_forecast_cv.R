# Mesmo BVAR original; apenas troca o satelite usado no condicionamento suave.
# Nao exclui janelas em que o satelite nao passa a triagem: usa o BVAR original.
# Logo, o CV avalia a regra completa: adicionar o satelite somente quando aprovado.
executar_janela_original <- function(numero,d,id,chave) {
  usado <- TRUE; motivo <- NA_character_
  valor <- tryCatch(janela_func_original_ust(numero,d,id,chave),error=function(e) {
    if(!startsWith(conditionMessage(e),'Nenhuma configuracao aprovada nesta origem:')) stop(e)
    usado <<- FALSE; motivo <<- conditionMessage(e)
    fit <- estimar_reduzido_cv(d,'base',chave)
    referencia <- distribuicoes_cv(forecast_reduzido_cv(fit,d,id),d,id,'BVAR original ex post')
    bind_rows(referencia |> mutate(.model=nomes_testes[3]),referencia,benchmarks_cv_original(d,id))
  })
  destino <- file.path(pasta_testes,'resultados','uso do satelite por origem')
  dir.create(destino,recursive=TRUE,showWarnings=FALSE)
  write_csv(tibble(origem=as.Date(d$origem),satelite_usado=usado,motivo=motivo),
    file.path(destino,paste0(format(as.Date(d$origem),'%Y-%m'),'.csv')))
  valor
}
executar_forecast_ust <- function() {
  chave <- chave_execucao(); destino <- file.path(pasta_testes,'resultados','forecast suave')
  arq <- file.path(destino,'resultado.rds')
  valor <- cache_compativel(arq,chave)
  if(is.null(valor)) {
    d <- ler_insumos()
    valor <- calcular_opcao(3L,d,chave)
    # Elimina o rotulo de fluxo herdado da interface antiga: nao entra neste teste.
    valor$insumos <- valor$insumos |> select(-any_of('fluxo')) |>
      left_join(select(d$futuro_aux,date,ust30),by='date')
    valor$meta$media_fluxo <- NULL; valor$meta$n_media_fluxo <- NULL
    guardar_cache(arq,chave,valor)
    write_csv(valor$tabela |> mutate(date=as.Date(date)),file.path(destino,'forecasts.csv'))
    write_csv(valor$insumos |> mutate(date=as.Date(date)),file.path(destino,'premissas.csv'))
    exportar_satelite(valor$satelite,destino)
    write_csv(valor$satelite$configuracao,file.path(destino,'configuracao selecionada.csv'))
  }
  x <- valor$tabela |> filter(variavel %in% domesticas_cv)
  historico <- readRDS(file.path(fonte_modelo,'dados.rds')) |> as_tibble() |>
    select(date,all_of(domesticas_cv)) |>
    pivot_longer(-date,names_to='variavel',values_to='valor') |>
    mutate(across(valor,identity))
  gdata <- x |> group_by(modelo,cenario) |> group_modify(function(a,k) {
    bind_rows(historico,select(a,date,variavel,valor)) |>
      arrange(variavel,date) |> group_by(variavel) |>
      mutate(valor=case_when(variavel %in% c('atividade','cpi_core')~
        100*expm1((valor-lag(valor,12))/100),variavel=='usdjpy'~exp(valor/100),
        variavel=='inclinacao'~100*valor,TRUE~valor)) |>
      ungroup() |> filter(date %in% a$date)
  }) |> ungroup() |> mutate(date=as.Date(date))
  nomes <- c(atividade='Atividade | 12 meses (%)',cpi_core='Inflacao | 12 meses (%)',
    juro_curto='Juro curto | %',inclinacao='30Y - 5Y | pb',usdjpy='USD/JPY')
  for(cen in unique(gdata$cenario)) {
    a <- filter(gdata,cenario==cen)
    labels <- a |> group_by(modelo,variavel) |> slice_max(date,n=1,with_ties=FALSE)
    g <- ggplot(a,aes(date,valor,colour=modelo))+geom_line(linewidth=.8)+
      geom_text(data=labels,aes(label=round(valor,2)),hjust=-.08,size=3)+
      facet_wrap(vars(variavel),ncol=2,scales='free_y',labeller=as_labeller(nomes))+
      scale_x_date(date_labels='%m/%Y',expand=expansion(mult=c(.02,.18)))+
      theme_minimal()+theme(legend.position='bottom')+
      labs(title=paste('Satelite UST30 + BVAR suave |',cen),x=NULL,y=NULL,colour=NULL,
        caption='Centros condicionais. Satelite treinado desde 2014. BoJ terminal entre 1,75% e 2,25%.\nCovariancia suave inclui inovacoes ARIMA; nao incerteza de parametros ou das curvas.')
    ggsave(file.path(destino,paste0('forecast ',gsub('[^[:alnum:]]','_',cen),'.png')),
      g,width=13,height=10,dpi=150,bg='white')
  }
  invisible(valor)
}

executar_cv_ust <- function() {
  # Usa a mesma grade trimestral e horizonte de 12 meses do CV atual.
  # Mantem as nove origens comuns jun/2024-jun/2026 para comparar com as opcoes 1-4.
  # A grade de candidatos e a regra de selecao sao fixadas antes do CV.
  # AICc, significancia e residuos sao recalculados so no treino de cada origem.
  # Se nenhum satelite passa, a janela permanece na avaliacao com o BVAR original.
  executar_cv(3L,chave_execucao())
  chave <- chave_cv_original(chave_execucao())
  arq <- file.path(raiz_cv_original,'opcao_3','resultado.rds')
  z <- cache_compativel(arq,chave)
  if(is.null(z)) stop('CV ainda nao completo.')
  metricas <- z$metricas |> filter(periodo=='Geral')
  g <- ggplot(metricas,aes(RMSE,.model,fill=.model))+geom_col()+
    geom_text(aes(label=round(RMSE,3)),hjust=-.1)+
    facet_wrap(vars(variavel),ncol=2,scales='free')+
    scale_x_continuous(expand=expansion(mult=c(0,.22)))+
    theme_minimal()+theme(legend.position='none')+
    labs(title='CV BVAR suave com satelite UST30 desde 2014',x='RMSE | unidades do modelo',y=NULL,
      caption='Mesmas origens/datas/horizontes. Atividade, CPI e cambio em 100 x log; taxas em p.p.\nExternos e OIS/UST realizados ex post. Sem imposicao da banda BoJ futura no CV.')
  ggsave(file.path(raiz_cv_original,'CV cinco variaveis RMSE.png'),g,
    width=13,height=10,dpi=150,bg='white')
  # Previsoes x reais, incluindo referencia/Naive/Drift, em cada janela salva.
  e <- z$escores |> mutate(date=as.Date(date),origem=as.Date(origem))
  for(alvo in domesticas_cv) {
    a <- filter(e,variavel==alvo)
    obs <- distinct(a,origem,date,observado)
    g <- ggplot(a,aes(date,.mean,colour=.model))+geom_line()+
      geom_line(data=obs,aes(date,observado),inherit.aes=FALSE,colour='black',linewidth=.9)+
      facet_wrap(vars(origem),ncol=3,scales='free_x')+theme_minimal()+
      theme(legend.position='bottom')+
      labs(title=paste('CV previsto x realizado |',alvo),x=NULL,y='Unidades originais do modelo',
        colour=NULL,caption='Preto: realizado. Trajetorias centrais salvas; parametros/selecoes somente do treino.')
    ggsave(file.path(raiz_cv_original,paste0('CV previsto x realizado ',alvo,'.png')),
      g,width=14,height=10,dpi=150,bg='white')
  }
  invisible(z)
}
