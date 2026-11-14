.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse));suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/todos cenarios oferta central'
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
pasta <- file.path(trabalho,'producao')
post <- readRDS(file.path(raiz,'data/processed/cenarios_propios_20261104/modelo_producao/modelo.rds'))
resultado <- readRDS(file.path(pasta,'forecast_bvar.rds'))
trajetorias <- readRDS(file.path(pasta,'trajetorias modelos.rds'))
modelo <- resultado$dados_modelo |> as_tibble() |> select(-date) |> as.matrix()
p <- post$last_draw$p;S <- dim(post$posterior$A)[3];n <- ncol(modelo)
F <- trajetorias[['Base']];h <- dim(F)[2]
previsao <- readRDS(file.path(pasta,'previsao_base_pacote.rds'))
erro_recursao <- 0
for(s in seq_len(S)) {
  historico <- modelo
  for(mes in seq_len(h)) {
    estado <- c(t(historico[nrow(historico)-(0:(p-1L)),,drop=FALSE]),1,mes/12)
    esperado <- drop(post$posterior$A[,,s]%*%estado)
    erro_recursao <- max(erro_recursao,max(abs(previsao$forecast_mean[,mes,s]-esperado)))
    historico <- rbind(historico,F[,mes,s])
  }
}
stopifnot(erro_recursao<1e-10,all(resultado$diagnosticos$erro_condicoes<1e-8),
  inherits(resultado$previsoes,'fbl_ts'),dim(F)[3]==S)
respostas <- readRDS(file.path(raiz,'src/Outputs/4- Forecast/todos cenarios oferta central/producao/resposta oferta.rds'))
for(cen in names(trajetorias)) {
  delta <- sweep(trajetorias[[cen]],c(1,2),t(respostas[[cen]]),'-')
  stopifnot(max(abs(delta-F))<1e-10,all(is.finite(trajetorias[[cen]])))
}
cat('VERIFICADO: chamada nativa, recursao mensal correta, fable original, mesmos choques e todos os',S,'sorteios.\n')
cat('ERRO_RECURSAO',erro_recursao,'\n')
