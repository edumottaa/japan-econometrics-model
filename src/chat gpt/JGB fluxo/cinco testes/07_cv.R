# CV expandido: reestima o modelo, hiperparametros COVID e satelite em CADA origem.
# Apenas fluxo do teste 4 e realizado futuro. Nos outros, e previsto pelo metodo.
# Externos (e OIS no satelite) realizados: avaliacao condicional, nao real time.
executar_cv_teste <- function(numero) {
  stopifnot(numero %in% 1:6)
  # Um unico treino por origem gera as duas previsoes e duas metricas finais.
  if(numero==6L) return(executar_cv_teste(1L))
  .libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
  library(fpp3)
  library(tidyverse)
  library(bsvarSIGNs)
  for(arq in c('01_dados.R','02_amostragem.R','03_modelos.R','04_forecast.R',
    '08_satelite_suave.R','09_selecao_satelite.R'))
    source(file.path(pasta_testes,arq),encoding='UTF-8')
  env <- environment(ler_insumos)
  opcoes <- list(ORIGEM=NULL,HORIZONTE=CV_HORIZONTE,S=CV_S,
    HIPER_S=CV_HIPER_S,HIPER_BURN=CV_HIPER_BURN,
    SORTEIOS_FORECAST=CV_SORTEIOS_FORECAST,MODO_CV_ATIVO=TRUE)
  antes <- mget(names(opcoes),envir=env,inherits=FALSE)
  on.exit(list2env(antes,envir=env),add=TRUE)
  list2env(opcoes,envir=env)
  historico <- readRDS(file.path(fonte_modelo,'dados.rds')) |> as_tibble() |>
    mutate(date=yearmonth(date))
  origens <- seq(yearmonth(CV_INICIO),max(historico$date)-CV_HORIZONTE,by=CV_PASSO)
  if(!length(origens)) stop('Sem origens completas para o CV.')
  destinos <- file.path(pasta_testes,'resultados',paste0('teste_',numero),'CV')
  dir.create(destinos,recursive=TRUE,showWarnings=FALSE)
  scripts_nucleo <- list.files(pasta_testes,pattern='^0[1-9]_.*\\.R$',full.names=TRUE)
  arquivos_fonte <- c(scripts_nucleo,
    file.path(fonte_modelo,c('dados.rds','modelo.rds')),
    file.path(fonte_dados,c('DGS30_2026-11-04.csv','ois2_historico.csv',
      'fluxos_jsda_superlongos.csv','curvas_bloomberg.csv')))
  chave <- tools::md5sum(arquivos_fonte)
  parametros_cv <- list(P=P,RHO=RHO,S=S,HIPER_S=HIPER_S,HIPER_BURN=HIPER_BURN,
    SORTEIOS_FORECAST=SORTEIOS_FORECAST,SEMENTE=SEMENTE,
    SATELITE_VARIANCIA_MULT=SATELITE_VARIANCIA_MULT,SATELITE_SIMULACOES=SATELITE_SIMULACOES)
  previsoes <- list(); previsoes_rigido <- list(); sat_ex_post <- list()
  destino_rigido <- file.path(pasta_testes,'resultados','teste_6','CV')
  if(numero==1L) dir.create(destino_rigido,recursive=TRUE,showWarnings=FALSE)
  for(i in seq_along(origens)) {
    assign('ORIGEM',format(origens[i]),envir=env)
    d <- ler_insumos()
    stopifnot(d$modo_cv,is.null(d$banda),max(d$banco$date)==origens[i])
    if(numero==1L) {
      # CV exige posterior propria por origem: nunca reutiliza a posterior de 2026.
      # Mantem a amostra original desde 2000; satelite continua separado desde 2018.
      d_original <- d
      d_original$banco <- d$historico |> filter(date<=d$origem) |>
        left_join(d$jsda,by='date')
      controle <- estimar_configuracao(d_original,'controle_original_cv',chave)
    } else controle <- estimar_configuracao(d,'controle',chave)
    nome_controle <- if(numero==1L) 'BVAR sem fluxo: amostra original por origem' else 'BVAR sem fluxo: amostra comum'
    tipo <- c('controle','exogeno','domestico','domestico','restrito')[numero]
    fit <- if(numero==1L) controle else estimar_configuracao(d,tipo,chave)
    cond0 <- as.matrix(select(d$caminhos,all_of(variaveis)))
    cond <- if(ncol(fit$y)==9L) cbind(cond0,fluxo=NA_real_) else cond0
    xfut <- NULL; sinal_suave <- NULL
    if(numero %in% c(1L,2L)) {
      sat <- obter_satelite_teste(d)
      pasta_selecao <- file.path(destinos,paste0('selecao_',format(as.Date(d$origem),'%Y-%m')))
      dir.create(pasta_selecao,recursive=TRUE,showWarnings=FALSE)
      exportar_selecao_satelite(sat,pasta_selecao,graficos=FALSE,exibir=FALSE)
      if(numero==1L) sinal_suave <- montar_satelite_suave(sat,cond0,d)
      if(numero==2L) xfut <- sat$completo$fluxo_l2[
        match(d$datas,sat$completo$date)]-fit$media_fluxo
      if(numero==1L) {
        # Diagnostico auxiliar do antigo estagio 2 EX POST, separado da cadeia completa.
        novos <- d$aux |> filter(date<=max(d$datas)) |>
          preparar_lags_satelite(c('ois_pos','fluxo')) |>
          filter(date %in% d$datas) |> as_tsibble(index=date)
        sat_ep <- forecast(sat$estagio2,new_data=novos) |> as_tibble()
        sat_ex_post[[i]] <- tibble(origem=as.Date(d$origem),
          previsto_pb=100*tail(d$banco$inclinacao,1)+sum(sat_ep$.mean),
          observado_pb=100*d$historico$inclinacao[match(max(d$datas),d$historico$date)])
      }
    }
    if(numero==4L) {
      realizados <- d$jsda$fluxo[match(d$datas,d$jsda$date)]
      stopifnot(all(is.finite(realizados)))
      cond[,9] <- realizados
    }
    ref <- projetar_configuracao(controle,cond0,d)$centro
    alt <- projetar_configuracao(fit,cond,d,xfuturo=xfut,satelite_suave=sinal_suave)$centro
    if(numero==1L) {
      cond_rigido <- cond0
      cond_rigido[,7] <- sinal_suave$media
      alt_rigido <- projetar_configuracao(fit,cond_rigido,d)$centro
    }
    inicial <- 100*tail(d$banco$inclinacao,1)
    observado <- 100*d$historico$inclinacao[match(max(d$datas),d$historico$date)]
    previsoes[[i]] <- tibble(teste=numero,origem=as.Date(d$origem),
      fim=as.Date(max(d$datas)),h=length(d$datas),
      modelo=c('Configuracao',nome_controle,'Naive','Drift'),
      previsto_pb=c(100*tail(alt$inclinacao,1),100*tail(ref$inclinacao,1),inicial,
        inicial+length(d$datas)*mean(diff(100*d$banco$inclinacao))),
      observado_pb=observado)
    if(numero==1L) {
      previsoes_rigido[[i]] <- previsoes[[i]] |> mutate(teste=6L,
        previsto_pb=if_else(modelo=='Configuracao',100*tail(alt_rigido$inclinacao,1),previsto_pb))
      write_csv(bind_rows(previsoes_rigido),file.path(destino_rigido,'previsoes parciais.csv'))
      saveRDS(list(origem=d$origem,controle=ref,teste=alt_rigido),
        file.path(destino_rigido,paste0('forecast_',format(as.Date(d$origem),'%Y-%m'),'.rds')))
    }
    # Salvar a cada origem: um erro posterior nao elimina origens ja concluidas.
    write_csv(bind_rows(previsoes),file.path(destinos,'previsoes parciais.csv'))
    saveRDS(list(origem=d$origem,controle=ref,teste=alt),
      file.path(destinos,paste0('forecast_',format(as.Date(d$origem),'%Y-%m'),'.rds')))
    message('CV teste ',if(numero==1L) '1 e 6 (treino compartilhado)' else numero,
      ': ',i,'/',length(origens),' | ',format(d$origem))
  }
  cv <- bind_rows(previsoes)
  metricas <- cv |> group_by(teste,modelo) |>
    summarise(origens=n(),rmse_pb=sqrt(mean((previsto_pb-observado_pb)^2)),
      mae_pb=mean(abs(previsto_pb-observado_pb)),vies_pb=mean(previsto_pb-observado_pb),
      .groups='drop')
  write_csv(cv,file.path(destinos,'previsoes.csv'))
  write_csv(metricas,file.path(destinos,'metricas.csv'))
  saveRDS(list(chave=chave,origens=origens,parametros=parametros_cv,
    previsoes=cv,metricas=metricas),
    file.path(destinos,'resultado CV.rds'))
  if(numero==1L) {
    cv_rigido <- bind_rows(previsoes_rigido)
    metricas_rigido <- cv_rigido |> group_by(teste,modelo) |>
      summarise(origens=n(),rmse_pb=sqrt(mean((previsto_pb-observado_pb)^2)),
        mae_pb=mean(abs(previsto_pb-observado_pb)),vies_pb=mean(previsto_pb-observado_pb),
        .groups='drop')
    write_csv(cv_rigido,file.path(destino_rigido,'previsoes.csv'))
    write_csv(metricas_rigido,file.path(destino_rigido,'metricas.csv'))
    saveRDS(list(chave=chave,origens=origens,parametros=parametros_cv,
      previsoes=cv_rigido,metricas=metricas_rigido),file.path(destino_rigido,'resultado CV.rds'))
    write_csv(bind_rows(metricas,metricas_rigido),file.path(destinos,'comparacao suave rigido.csv'))
  }
  if(length(sat_ex_post)) {
    ep <- bind_rows(sat_ex_post)
    write_csv(ep,file.path(destinos,'satelite estagio 2 ex post auxiliar.csv'))
    write_csv(tibble(rmse_pb=sqrt(mean((ep$previsto_pb-ep$observado_pb)^2)),origens=nrow(ep)),
      file.path(destinos,'satelite estagio 2 ex post auxiliar metricas.csv'))
  }
  # Agrega apenas resultados com mesmos insumos, configuracao e origens.
  arqs <- file.path(pasta_testes,'resultados',paste0('teste_',1:6),'CV','resultado CV.rds')
  completos <- lapply(arqs[file.exists(arqs)],readRDS)
  completos <- Filter(function(x) identical(x$chave,chave) && identical(x$origens,origens) &&
    identical(x$parametros,parametros_cv),completos)
  todas <- bind_rows(lapply(completos,`[[`,'metricas'))
  pasta_comp <- file.path(pasta_testes,'resultados','comparacao')
  dir.create(pasta_comp,recursive=TRUE,showWarnings=FALSE)
  write_csv(todas,file.path(pasta_comp,'CV spread.csv'))
  writeLines(c('CV condicional a externos realizados, com reestimacao por origem.',
    'Teste 4 usa fluxo realizado futuro; testes 1/2/6 projetam fluxo pelo ARIMA selecionado; 3/5 pelo BVAR.',
    'Testes 1/6 compartilham treinos e satelite: previsoes e metricas suave/rigido separadas.',
    'Nao usa a meta de juro terminal de 2028 nas janelas historicas.',
    'Horizontes de 12 meses sobrepostos; poucas origens. Nao e backtest real time.',
    'Criterio: desempenho do spread contra o controle comum, naive e drift.'),
    file.path(pasta_comp,'LEIA-ME CV.txt'))
  print(metricas,n=Inf,width=Inf)
  if(numero==1L) print(metricas_rigido,n=Inf,width=Inf)
  invisible(metricas)
}
