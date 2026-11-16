# BVAR-X: mesma amostra, tendencia, lags, priors e identificacao do principal.
# Beta do fluxo estimado em NIVEL de spread (%), nao importado do satelite em Delta.
# Fluxo = compras menos vendas dos estrangeiros em superlongos (JSDA).
# E uma exogena observada/condicionada, nao um novo choque estrutural.
# O sistema continua com oito endogenas e os mesmos oito choques do principal.
library(bsvarSIGNs)
original <- readRDS(file.path(fonte_modelo,'modelo.rds'))
dados_boj <- readRDS(file.path(fonte_modelo,'dados.rds'))
y <- dados_boj |> as_tibble() |> select(all_of(variaveis)) |> as.matrix()
p <- as.integer(original$last_draw$p)
tendencia <- matrix((seq_len(nrow(y))-nrow(y))/12,ncol=1)

# 1. Centralizar apenas onde o lag 2 e observado; antes disso x=0 significa indisponivel.
# A media e estimada so com a amostra de estimacao do BVAR, sem usar set/2026.
fx <- jsda |> arrange(mes) |> mutate(fluxo_l2=lag(fluxo,2))
x <- tibble(mes=yearmonth(dados_boj$date)) |> left_join(select(fx,mes,fluxo_l2),by='mes')
media_fluxo <- mean(x$fluxo_l2,na.rm=TRUE)
x <- x |> mutate(disponivel=is.finite(fluxo_l2),
                  fluxo_c=if_else(disponivel,fluxo_l2-media_fluxo,0))
stopifnot(is.finite(media_fluxo),all(is.finite(x$fluxo_c)))

# 2. Configuracao preservada. Variancia do novo beta difusa (100), media zero.
# As sete exclusoes sao exatas no amostrador; nao se usa prior apertado.
spec <- specify_bsvarSIGN$new(data=y[,4:8],foreign=y[,1:3],
  exogenous=cbind(tendencia,x$fluxo_c),p=p,
  sign_irf=original$last_draw$identification$sign_irf,
  sign_narrative=original$last_draw$identification$sign_narrative,
  sign_structural=original$last_draw$identification$sign_structural,
  hyper_mu=FALSE,hyper_delta=FALSE,hyper_lambda=FALSE,hyper_psi=FALSE,
  hyper_covid=original$last_draw$prior$covid)
# Reutiliza a calibracao COVID do principal para isolar a inclusao do fluxo.
# Posterior condicional a essa calibracao, nao nova estimacao dos hiperparametros.
spec$prior$hyper <- original$last_draw$prior$hyper
spec$prior$covid <- original$last_draw$prior$covid
stopifnot(identical(spec$prior$get_prior()$covid,original$last_draw$prior$covid))
pesos <- .995^rev(seq_len(ncol(spec$data_matrices$Y))-1)
spec$data_matrices$Y <- sweep(spec$data_matrices$Y,2,sqrt(pesos),'*')
spec$data_matrices$X <- sweep(spec$data_matrices$X,2,sqrt(pesos),'*')
spec$prior$Y <- spec$data_matrices$Y; spec$prior$X <- spec$data_matrices$X
source(file.path(pasta,'funcoes_restricao.R'),encoding='UTF-8')
set.seed(123)
cores <- get0('cores',ifnotfound=1L)
post_fluxo <- if(cores>1L) estimar_fluxo_paralelo(spec,S=S,cores=cores,
  log=file.path(saida,'amostragem BVAR log.txt')) else estimar_fluxo_restrito(spec,S=S)
post_fluxo <- normalizar_fluxo(post_fluxo,pesos)

# 3. Diagnosticos antes de publicar o modelo. Intervalos bayesianos, nao p-valores.
K <- dim(post_fluxo$posterior$A)[2]
beta <- post_fluxo$posterior$A[7,K,]
diag <- attr(post_fluxo,'diagnostico_restricoes')
coef_fluxo <- tibble(beta_media_pct=mean(beta),beta_mediana_pct=median(beta),
  inferior95_pct=quantile(beta,.025),superior95_pct=quantile(beta,.975),
  prob_beta_positivo=mean(beta>0),media_fluxo=media_fluxo,
  ess=diag$ess,sorteios=S,distintos=length(unique(beta)),
  max_beta_outras_equacoes=max(abs(post_fluxo$posterior$A[-7,K,])))
write_csv(coef_fluxo,file.path(saida,if(S<500) 'coeficiente fluxo BVAR piloto.csv' else 'coeficiente fluxo BVAR.csv'))
write_csv(x |> mutate(mes=as.Date(mes)),file.path(saida,'fluxo exogeno historico.csv'))
stopifnot(coef_fluxo$max_beta_outras_equacoes<1e-10,
          all(is.finite(post_fluxo$posterior$A)))
dom <- unlist(lapply(0:(p-1),function(j) 4:8+8*j))
stopifnot(max(abs(post_fluxo$posterior$A[1:3,dom,]))<1e-8,
          max(abs(post_fluxo$posterior$Theta0[1:3,4:8,]))<1e-8)
irfs <- bsvars::compute_impulse_responses(post_fluxo,horizon=12,standardise=FALSE)
stopifnot(all(irfs[6,6,1:2,]>0),all(irfs[7,6,1:2,]<0),
          all(irfs[2,2,1:2,]>0),all(irfs[3,2,2:7,]<0))
if(S>=500 && (diag$ess<50 || coef_fluxo$distintos<50))
  stop('Baixo tamanho efetivo. Diagnostico salvo; nao publicar forecast como final.')
saveRDS(list(modelo=post_fluxo,media_fluxo=media_fluxo,x=x,dados_boj=dados_boj),
        file.path(saida,if(S<500) 'bvar fluxo piloto.rds' else 'bvar fluxo.rds'))
print(coef_fluxo)
