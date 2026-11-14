Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
suppressPackageStartupMessages(library(bsvarSIGNs))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/choque unico Treasury'
tipo <- commandArgs(trailingOnly=TRUE)[1]
pasta <- file.path(trabalho,tipo)
dir.create(pasta,recursive=TRUE,showWarnings=FALSE)
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
source('C:/Users/dudup/OneDrive/japan-econometrics-model/src/7- forecast/bvar estrutural/experimento choque unico Treasury/funcoes gaussianas.R',encoding='UTF-8')
if(tipo=='original') {
  fonte<-file.path(raiz,'data/processed/forecast_pesos_exponenciais_20261026')
  post<-readRDS(file.path(fonte,'modelo.rds'));dados<-readRDS(file.path(fonte,'base.rds'))
  nomes<-c('ust2y','brent','atividade','cpi_core','juro_curto','inclinacao','usdjpy')
} else {
  fonte<-'C:/Users/dudup/OneDrive/japan-econometrics-model/data/processed/cenarios_propios_20261104/modelo_producao'
  post<-readRDS(file.path(fonte,'modelo.rds'));dados<-readRDS(file.path(fonte,'dados.rds'))
  nomes<-c('ust2y','brent','ip_mundial','atividade','cpi_core','juro_curto','inclinacao','usdjpy')
}
ids<-unique(as.integer(seq(1,dim(post$posterior$A)[3],length.out=500)))
irf_total<-bsvars::compute_impulse_responses(post,horizon=24,standardise=FALSE)
irf<-irf_total[,,,ids,drop=FALSE]
stopifnot(dim(irf)[1]==length(nomes))
for(q in seq_along(ids)) stopifnot(max(abs(irf[,2,1,q]-post$posterior$Theta0[,2,ids[q]]))<1e-10)
write_csv(tibble(modelo=tipo,n_variaveis=length(nomes),fonte_modelo=file.path(fonte,'modelo.rds'),
 md5_modelo=unname(tools::md5sum(file.path(fonte,'modelo.rds'))),irf_do_proprio_modelo=TRUE),
 file.path(pasta,'proveniencia_irf.csv'))
original<-readRDS(file.path(raiz,'src/Outputs/Forecast BVAR estrutural/pesos exponenciais/forecast_bvar.rds'))
Y <- dados |> as_tibble() |> select(all_of(nomes)) |> as.matrix()
caminhos <- original$caminhos |> as_tibble() |> arrange(cenario,date)
base <- caminhos |> filter(cenario=='Base') |> arrange(date)
h <- nrow(base); S <- length(ids)
montar_modelo <- function(post,Y,s,h) {
  n <- ncol(Y); p <- as.integer(post$last_draw$p); A <- post$posterior$A[,,s]
  list(y=Y,p=p,variaveis=colnames(Y),centro=setNames(rep(0,n),colnames(Y)),
    A=lapply(seq_len(p),function(l) A[,(l-1)*n+seq_len(n),drop=FALSE]),
    constante=A[,n*p+1],constantes_futuras=matrix(A[,n*p+1],h,n,byrow=TRUE)+
      tcrossprod(seq_len(h)/12,A[,n*p+2]),sigma=post$posterior$Sigma[,,s])
}
saveRDS(montar_modelo,file.path(pasta,'montar_modelo.rds'))
# Base externa: Bloomberg e Brent existentes; IP mundial livre no modelo de oito variaveis.
cond <- matrix(NA_real_,h,length(nomes),dimnames=list(NULL,nomes))
cond[,] <- as.matrix(base |> mutate(ip_mundial=NA_real_) |> select(all_of(nomes)))
medias <- array(NA_real_,c(h,length(nomes),S))
for(s in seq_len(S)) {
  fc <- prever_var_condicionado(montar_modelo(post,Y,ids[s],h),cond)
  medias[,,s] <- fc$media
  if(s%%100==0) cat('BASE',s,'/',S,'\n')
}
base_media <- apply(medias,c(1,2),mean); colnames(base_media)<-nomes
base <- base |> mutate(ust2y=coalesce(ust2y,base_media[,'ust2y']),
  brent=coalesce(brent,base_media[,'brent']),ip_mundial=if('ip_mundial'%in%nomes) base_media[,'ip_mundial'] else NA_real_)
# Um unico choque, somente no adverso estrutural. Brent permanece fixado.
modelo_ip <- readRDS(file.path(trabalho,'modelo producao ARIMA.rds'))
res <- list(); choques <- list(); incerteza <- list()
for(cen in unique(caminhos$cenario)) {
  c <- caminhos |> filter(cenario==cen) |> arrange(date)
  delta <- c$brent-base$brent; delta[is.na(delta)] <- 0
  du <- eps <- matrix(0,h,S)
  inicio <- if(cen=='Adverso estrutural') which(abs(delta)>1e-10)[1] else NA_integer_
  if(!is.na(inicio)) for(s in seq_len(S)) {
    # Calibrar pela diferenca inicial de Brent em 100*log, sem novos choques mensais.
    intensidade <- delta[inicio]/irf[2,2,1,s]
    eps[inicio,s] <- intensidade
    du[inicio:h,s] <- intensidade*irf[1,2,seq_len(h-inicio+1L),s]
  }
  stopifnot(all(is.finite(du)), all(colSums(abs(eps)>0)<=1L))
  # Preservar NA dos meses originalmente livres; Bloomberg nos demais meses.
  c <- c |> mutate(ust2y=if_else(is.na(ust2y),NA_real_,base$ust2y+apply(du,1,median)))
  if('ip_mundial'%in%nomes) {
    x <- c |> transmute(date,ust2y=coalesce(ust2y,base$ust2y),
      brent=coalesce(brent,base$brent)) |> as_tsibble(index=date)
    previsto_ip <- forecast(modelo_ip,new_data=x)
    c$ip_mundial <- as.numeric(previsto_ip$.mean)
    saveRDS(previsto_ip,file.path(pasta,paste0('producao ARIMA ',cen,'.rds')))
  } else c$ip_mundial <- NA_real_
  # Conferir caminhos do Brent e dos tres primeiros cenarios de Treasury.
  antigo <- caminhos |> filter(cenario==cen) |> arrange(date)
  stopifnot(identical(c$brent,antigo$brent))
  if(cen!='Adverso estrutural') stopifnot(identical(c$ust2y,antigo$ust2y))
  res[[cen]] <- c
  choques[[cen]] <- tibble(cenario=cen,mes_choque=if(is.na(inicio)) NA_character_ else as.character(c$date[inicio]),
    meses_com_novo_choque=max(colSums(abs(eps)>0)),
    diferenca_inicial_brent_log=if(is.na(inicio)) 0 else delta[inicio],
    mediana_intensidade=median(colSums(eps)),
    p05_intensidade=quantile(colSums(eps),.05),p95_intensidade=quantile(colSums(eps),.95),
    ajuste_inicial_treasury_pb=if(is.na(inicio)) 0 else 100*median(du[inicio,]),
    ajuste_final_treasury_pb=100*median(du[h,]))
  incerteza[[cen]] <- tibble(cenario=cen,date=c$date,
    delta_treasury_mediana=apply(du,1,median),delta_treasury_p05=apply(du,1,quantile,.05),
    delta_treasury_p95=apply(du,1,quantile,.95))
}
novos <- bind_rows(res); diagnostico <- bind_rows(choques)
saveRDS(novos,file.path(pasta,'caminhos_novos.rds'))
write_csv(novos |> mutate(date=as.Date(date)),file.path(pasta,'caminhos_novos.csv'))
write_csv(diagnostico,file.path(pasta,'diagnostico_choques.csv'))
write_csv(bind_rows(incerteza) |> mutate(date=as.Date(date)),file.path(pasta,'incerteza_calibracao.csv'))
cores <- c(Base='#2166AC',Benigno='#1B9E77',Adverso='#E69F00','Adverso estrutural'='#B2182B')
d <- novos |> left_join(base |> select(date,ust_base=ust2y,brent_base=brent),by='date') |>
  transmute(date=as.Date(date),cenario,`Treasury de 2 anos (% a.a.)`=coalesce(ust2y,ust_base),
  `Producao industrial mundial (indice)`=exp(ip_mundial/100),`Brent (US$/barril)`=exp(coalesce(brent,brent_base)/100)) |>
  pivot_longer(-c(date,cenario),names_to='variavel',values_to='valor') |> filter(is.finite(valor))
rotulos <- d |> group_by(cenario,variavel) |> filter(lubridate::month(date)==12|date==max(date)) |>
  ungroup() |> mutate(rotulo=formatC(valor,format='f',digits=1,decimal.mark=','))
g <- ggplot(d,aes(date,valor,colour=cenario))+geom_line(linewidth=.9)+
  geom_point(data=rotulos,size=1.5)+geom_text(data=rotulos,aes(label=rotulo),vjust=-.7,size=3,check_overlap=TRUE,show.legend=FALSE)+
  facet_wrap(vars(variavel),ncol=1,scales='free_y')+scale_colour_manual(values=cores)+
  scale_x_date(date_breaks='6 months',date_labels='%m/%Y')+theme_minimal(base_size=12)+
  theme(legend.position='bottom',panel.grid.minor=element_blank(),strip.text=element_text(face='bold'))+
  labs(title=paste('Choque unico no Treasury |',tipo),
    subtitle=if(tipo=='original') 'Modelo original de sete variaveis | choque de Brent, sem identificacao exclusiva de oferta' else 'Modelo de oito variaveis | so oferta identificada; atividade japonesa livre',x=NULL,y=NULL,colour=NULL,
    caption='Brent fixado. Treasury Bloomberg nos tres primeiros cenarios; no estrutural, ajuste por uma unica surpresa inicial.\nProducao mundial: regressao com Treasury e Brent, erros ARIMA. Sem forcar convergencia ou sinal dos juros.')
ggsave(file.path(pasta,'cenarios externos conjuntos.png'),g,width=13,height=12,dpi=160,bg='white')
print(diagnostico);cat('CENARIOS_CONCLUIDOS\n')
