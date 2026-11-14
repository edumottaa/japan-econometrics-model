Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
suppressPackageStartupMessages(library(bsvarSIGNs))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/cenarios por modelo'
tipo <- 'producao'
pasta <- file.path(trabalho,tipo)
dir.create(pasta,recursive=TRUE,showWarnings=FALSE)
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
source(file.path('C:/Users/dudup/OneDrive/japan-econometrics-model/src/7- forecast/bvar estrutural/experimentos cenarios por modelo','funcoes gaussianas.R'),encoding='UTF-8')
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
H <- function(v,s) {
  M <- matrix(0,h,h)
  for(t in seq_len(h)) for(j in seq_len(t)) M[t,j] <- irf[v,2,t-j+1,s]
  M
}
res <- list(); choques <- list(); incerteza <- list()
for(cen in unique(caminhos$cenario)) {
  c <- caminhos |> filter(cenario==cen) |> arrange(date)
  livre_treasury <- is.na(c$ust2y)
  delta <- c$brent-base$brent; delta[is.na(delta)] <- 0
  du <- di <- eps <- matrix(NA_real_,h,S)
  erros <- numeric(S); condicao <- numeric(S)
  inicio <- which(abs(delta)>1e-10)[1]
  for(s in seq_len(S)) {
    e <- rep(0,h)
    if(!is.na(inicio)) {
      perfil <- as.numeric(seq_len(h)>=inicio)
      psi <- drop(H(2,s)%*%perfil)
      e <- perfil*sum(psi*delta)/sum(psi^2)
    }
    HB <- H(2,s); condicao[s] <- rcond(HB)
    eps[,s] <- e; du[,s] <- H(1,s)%*%e; di[,s] <- if('ip_mundial'%in%nomes) H(3,s)%*%e else rep(0,h)
    erros[s] <- max(abs(HB%*%e-delta))
  }
  stopifnot(all(is.finite(du)),all(is.finite(di)))
  c <- c |> mutate(ust2y=if_else(livre_treasury,NA_real_,base$ust2y+apply(du,1,median)),
    ip_mundial=base$ip_mundial+apply(di,1,median))
  res[[cen]] <- c
  choques[[cen]] <- tibble(cenario=cen,mediana_pico_choque=median(apply(abs(eps),2,max)),
    p95_pico_choque=quantile(apply(abs(eps),2,max),.95),
    mediana_rms=median(sqrt(colMeans(eps^2))),erro_maximo=max(erros),
    menor_rcond=min(condicao),mediana_erro_aproximacao_brent=median(erros))
  incerteza[[cen]] <- tibble(cenario=cen,date=c$date,
    delta_treasury_mediana=apply(du,1,median),delta_treasury_p05=apply(du,1,quantile,.05),
    delta_treasury_p95=apply(du,1,quantile,.95),delta_ip_mediana=apply(di,1,median))
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
  labs(title=paste('Cenarios externos | IRFs do proprio modelo:',tipo),
    subtitle=if(tipo=='original') 'Modelo original de sete variaveis | choque de Brent, sem identificacao exclusiva de oferta' else 'Modelo de oito variaveis | so oferta identificada; atividade japonesa livre',x=NULL,y=NULL,colour=NULL,
    caption='Cenarios condicionantes: nao sao previsoes independentes nem garantem que somente o choque de oferta ocorrera no forecast.\nMedianas da calibracao por sorteio; sem impor sinal ao Treasury ou a atividade japonesa. Curva Bloomberg de 22/10/2026.')
ggsave(file.path(pasta,'cenarios externos conjuntos.png'),g,width=13,height=12,dpi=160,bg='white')
print(diagnostico);cat('CENARIOS_CONCLUIDOS\n')
