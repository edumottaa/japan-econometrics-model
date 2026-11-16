# Somente a configuracao escolhida. Externos/OIS ex post; fluxo futuro media do treino.
executar_janela_original <- function(numero,d,id,chave) {
  stopifnot(numero==3L)
  fit <- estimar_reduzido_cv(d,'base',chave)
  sat <- estimar_satelite_teste(d)
  sinal <- montar_satelite_suave(sat,as.matrix(select(d$caminhos,all_of(variaveis))),d)
  F <- forecast_reduzido_cv(fit,d,id,sinal_suave=sinal)
  arquivo <- file.path(raiz_cv_original,'cache',paste0('previsao_base_',format(as.Date(d$origem),'%Y-%m'),'.rds'))
  referencia <- cache_compativel(arquivo,chave)
  if(is.null(referencia)) {
    referencia <- distribuicoes_cv(forecast_reduzido_cv(fit,d,id),d,id,'BVAR original ex post')
    guardar_cache(arquivo,chave,referencia)
  }
  bind_rows(distribuicoes_cv(F,d,id,nomes_testes[3]),referencia,benchmarks_cv_original(d,id))
}

# Sinal suave do CV (versao Kalman): alvo do spread e covariancia acumulada do satelite
montar_satelite_suave <- function(sat,cond,d) {
  h <- nrow(cond)
  delta <- sat$satelite$delta_pb[match(d$datas,sat$satelite$date)]
  cov_delta <- sat$cov_delta_pb2
  stopifnot(identical(dim(cov_delta),c(h,h)),all(is.finite(cov_delta)),
    is.finite(SATELITE_VARIANCIA_MULT),SATELITE_VARIANCIA_MULT>0,
    length(delta)==h,all(is.finite(delta)))
  # Acumular a covariancia futura da equacao selecionada, respeitando as ancoras.
  # Nao pressupoe erro branco e nao trata os horizontes como independentes.
  L <- matrix(0,h,h); alvo_pb <- numeric(h)
  anterior <- 100*tail(d$banco$inclinacao,1)
  for(t in seq_len(h)) {
    if(is.finite(cond[t,7])) {
      alvo_pb[t] <- 100*cond[t,7]
    } else {
      alvo_pb[t] <- anterior+delta[t]
      if(t>1L) L[t,] <- L[t-1L,]
      L[t,t] <- 1
    }
    anterior <- alvo_pb[t]
  }
  list(media=alvo_pb/100,
    covariancia=SATELITE_VARIANCIA_MULT*L%*%cov_delta%*%t(L)/10000,
    nota='Cenario suave; covariancia acumulada das inovacoes da equacao do spread, condicional aos insumos. Nao inclui incerteza da media do fluxo ou dos parametros; nao presume informacao independente do BVAR.')
}
