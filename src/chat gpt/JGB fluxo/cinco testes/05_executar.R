# Orquestracao. So executa quando chamada por um dos seis code_run.
executar_teste <- function(numero) {
  .libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
  library(fpp3)
  library(tidyverse)
  library(bsvarSIGNs)
  for(arq in c('01_dados.R','02_amostragem.R','03_modelos.R','04_forecast.R',
    '06_comparar.R','08_satelite_suave.R','09_selecao_satelite.R'))
    source(file.path(pasta_testes,arq),encoding='UTF-8')
  nomes <- c('Satelite suave no BVAR original','Fluxo exogeno projetado pelo satelite',
    'Fluxo domestico ex ante','Fluxo domestico ex post disponivel',
    'Macro para fluxo para spread','Satelite rigido no BVAR original')
  stopifnot(numero %in% 1:6,P==6L,S>=50L,CORES>=1L,
    identical(as.numeric(BANDA_JURO),c(1.75,2.25)))
  for(p in c('cache',paste0('teste_',numero)))
    dir.create(file.path(pasta_testes,'resultados',p),recursive=TRUE,showWarnings=FALSE)
  destino <- file.path(pasta_testes,'resultados',paste0('teste_',numero))
  d <- ler_insumos()
  observados_fluxo <- tibble(date=d$datas) |> left_join(d$jsda,by='date')
  n_observados <- sum(is.finite(observados_fluxo$fluxo))
  if(numero==4L && n_observados<length(d$datas)) {
    aviso <- paste0('EX POST PARCIAL: fluxo realizado em ',n_observados,'/',length(d$datas),
      ' meses. Demais meses projetados endogenamente; nao sao realizados. ',
      'Para ex post completo, escolha origem historica comum aos seis testes.')
    if(EXIGIR_FLUXO_EX_POST_COMPLETO) stop(aviso) else warning(aviso,immediate.=TRUE)
  }
  scripts_nucleo <- list.files(pasta_testes,pattern='^0[1-9]_.*\\.R$',full.names=TRUE)
  arquivos_fonte <- c(scripts_nucleo,
    file.path(fonte_modelo,c('dados.rds','modelo.rds')),
    file.path(fonte_forecast,'caminhos_novos.rds'),
    file.path(fonte_dados,c('DGS30_2026-11-04.csv','ois2_historico.csv',
      'fluxos_jsda_superlongos.csv','curvas_bloomberg.csv')))
  chave <- tools::md5sum(arquivos_fonte)
  protegidos <- c(file.path(fonte_modelo,c('dados.rds','modelo.rds')),
    file.path(fonte_forecast,c('forecast_bvar.rds','caminhos_novos.rds','trajetorias modelos.rds')))
  hash_antes <- tools::md5sum(protegidos)
  message('Teste ',numero,': ',nomes[numero],'. Amostra ',min(d$banco$date),' a ',d$origem,
    '; juro terminal obrigatoriamente entre 1,75% e 2,25%.')
  controle <- if(numero %in% c(1L,6L)) carregar_bvar_original(d) else estimar_configuracao(d,'controle',chave)
  nome_controle <- if(numero %in% c(1L,6L)) 'BVAR original salvo: amostra completa' else 'BVAR sem fluxo: amostra comum'
  sat <- NULL
  if(numero %in% c(1L,2L,6L)) {
    sat <- obter_satelite_teste(d)
    saveRDS(sat,file.path(destino,'satelite.rds'))
    exportar_selecao_satelite(sat,destino,graficos=TRUE,exibir=MOSTRAR_RESIDUOS)
    write_csv(bind_rows(tidy(sat$estagio1) |> mutate(estagio='Fluxo'),
      tidy(sat$estagio2) |> mutate(estagio='Spread')),file.path(destino,'coeficientes satelite.csv'))
    diag_sat <- bind_rows(
      diagnosticar_satelite(sat$estagio1) |> mutate(estagio='Fluxo'),
      diagnosticar_satelite(sat$estagio2) |> mutate(estagio='Spread'))
    write_csv(diag_sat,file.path(destino,'diagnosticos satelite.csv'))
    if(any(diag_sat$p_ljung_box<.05)) warning('Autocorrelacao no satelite: veja diagnosticos.')
  }
  tipo <- c('controle','exogeno','domestico','domestico','restrito','controle')[numero]
  fit <- if(numero %in% c(1L,6L)) controle else estimar_configuracao(d,tipo,chave)
  resultados <- list(); centros <- list(); insumos <- list()
  for(cen in unique(d$caminhos$cenario)) {
    caminho <- d$caminhos |> filter(cenario==cen) |> arrange(date)
    stopifnot(identical(caminho$date,d$datas))
    cond0 <- as.matrix(select(caminho,all_of(variaveis)))
    cond <- cond0; xfut <- NULL; sinal_suave <- NULL
    if(ncol(fit$y)==9L) cond <- cbind(cond,fluxo=NA_real_)
    if(numero %in% c(1L,6L)) {
      sinal_suave <- montar_satelite_suave(sat,cond0,d)
      # cond permanece identico ao controle: o spread do satelite nao vira alvo exato.
      write_csv(tibble(date=as.Date(d$datas),cenario=cen,
        alvo_satelite_pb=100*sinal_suave$media,
        desvio_satelite_pb=100*sqrt(diag(sinal_suave$covariancia))),
        file.path(destino,paste0(if(numero==6L) 'alvo rigido ' else 'sinal suave ',cen,'.csv')))
      if(numero==6L) {
        cond[,7] <- sinal_suave$media
        sinal_suave <- NULL # Trajetoria exata, nao sinal com erro.
      }
    }
    if(numero %in% c(1L,2L,6L)) {
      lag_c <- sat$completo$fluxo_l2[match(d$datas,sat$completo$date)]-fit$media_fluxo
      if(numero==2L) xfut <- lag_c
      insumos[[cen]] <- tibble(date=d$datas,cenario=cen,
        fluxo= sat$fluxo$fluxo[match(d$datas,sat$fluxo$date)],
        fluxo_l2_centrado=lag_c,fonte='Estagio 1 selecionado pela estrutura IP; nao usa fluxo futuro realizado')
    }
    if(numero==4L) cond[,9] <- observados_fluxo$fluxo
    ref <- projetar_configuracao(controle,cond0,d)
    alt <- projetar_configuracao(fit,cond,d,xfuturo=xfut,satelite_suave=sinal_suave)
    resultados[[paste0(cen,'_controle')]] <- ref$centro |>
      mutate(cenario=cen,modelo=nome_controle)
    resultados[[paste0(cen,'_teste')]] <- alt$centro |>
      mutate(cenario=cen,modelo=nomes[numero])
    centros[[cen]] <- list(controle=ref$centros_modelo,teste=alt$centros_modelo,
      variaveis=fit$vars,condicoes=cond,sinal_suave=sinal_suave)
    if(ncol(fit$y)==9L) insumos[[cen]] <- alt$centro |> select(date,fluxo) |>
      mutate(cenario=cen,fonte=if(numero==4L)
        if_else(is.finite(observados_fluxo$fluxo),'Realizado: condicionamento ex post',
          'Projetado endogenamente: NAO ex post') else 'Projetado endogenamente')
    message('Forecast concluido: ',cen,' | ',nomes[numero])
  }
  tabela <- bind_rows(resultados) |>
    pivot_longer(any_of(c(variaveis,'fluxo')),names_to='variavel',values_to='valor') |>
    filter(is.finite(valor))
  tabela <- anualizar_teste(tabela,d)
  ref <- tabela |> filter(modelo==nome_controle) |>
    select(date,cenario,variavel,referencia=valor,referencia_plot=valor_plot)
  tabela <- tabela |> left_join(ref,by=c('date','cenario','variavel')) |>
    mutate(efeito_pb=if_else(variavel %in% c('inclinacao','juro_curto','ust2y'),
      100*(valor-referencia),NA_real_),teste=numero)
  fluxo_tabela <- bind_rows(insumos)
  if(!numero %in% c(1L,6L)) {
    cols <- if(numero==2L) dim(fit$modelo$posterior$A)[2] else (0:(P-1L))*9L+9L
    betas <- purrr::map_dfr(seq_along(cols),function(j) {
      b <- 100*fit$modelo$posterior$A[7,cols[j],]
      tibble(termo=if(numero==2L) 'Fluxo exogeno lag 2' else paste0('Fluxo domestico lag ',j),
        media_pb=mean(b),p025_pb=quantile(b,.025),p975_pb=quantile(b,.975),
        prob_positivo=mean(b>0))
    })
    write_csv(betas,file.path(destino,'coeficientes fluxo spread.csv'))
  }
  if(nrow(fluxo_tabela)) write_csv(fluxo_tabela |> mutate(date=as.Date(date)),
    file.path(destino,'trajetoria fluxo.csv'))
  write_csv(tabela |> mutate(date=as.Date(date)),file.path(destino,'forecasts.csv'))
  write_csv(tabela |> filter(date==max(date)) |> mutate(date=as.Date(date)),
    file.path(destino,'valores finais.csv'))
  meta <- list(teste=numero,nome=nomes[numero],origem=d$origem,datas=d$datas,
    inicio=min(d$banco$date),n=nrow(d$banco),chave=chave,
    banda=BANDA_JURO,tipo_externos=d$tipo_externos,controle=nome_controle,
    inicio_bvar=if(numero %in% c(1L,6L)) controle$inicio else min(d$banco$date),
    meses_fluxo_ex_post=if(numero==4L) n_observados else 0L,
    ex_post_completo=numero==4L && n_observados==length(d$datas),
    parametros=list(P=P,RHO=RHO,S=S,HIPER_S=HIPER_S,HIPER_BURN=HIPER_BURN,
      SORTEIOS_FORECAST=SORTEIOS_FORECAST,SATELITE_VARIANCIA_MULT=SATELITE_VARIANCIA_MULT,
      SATELITE_SIMULACOES=SATELITE_SIMULACOES))
  saveRDS(list(meta=meta,forecast=tabela,centros=centros,fluxo=fluxo_tabela),
    file.path(destino,'resultado.rds'))
  writeLines(c(paste('Teste:',numero,nomes[numero]),
    paste('Amostra do satelite/modelos novos:',meta$inicio,'a',meta$origem,'| n =',meta$n),
    paste('Controle:',nome_controle,'| inicio BVAR:',meta$inicio_bvar),
    if(numero==1L) 'Posterior BVAR original preservada; condicionamento suave pela covariancia do satelite.' else
      if(numero==6L) 'Posterior BVAR original preservada; mesma trajetoria do satelite fixada rigidamente.' else 'BVAR novo reestimado na amostra comum.',
    paste('Externos:',meta$tipo_externos),
    'Juro terminal condicionado em [1,75%; 2,25%] em todos os cenarios e modelos.',
    paste('Fluxo realizado fixado:',meta$meses_fluxo_ex_post,'/',length(d$datas),'meses'),
    'Centros condicionais; nao intervalos preditivos. CV nao executado.',
    'Sinais e narrativas sao hipoteses; spread nao identifica premio fiscal puro.'),
    file.path(destino,'metadados.txt'),useBytes=TRUE)
  stopifnot(identical(hash_antes,tools::md5sum(protegidos)))
  comparar_testes()
  invisible(tabela)
}
