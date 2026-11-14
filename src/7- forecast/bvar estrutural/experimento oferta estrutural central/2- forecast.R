Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse));suppressPackageStartupMessages(library(fpp3))
suppressPackageStartupMessages(library(bsvarSIGNs))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/cenario oferta estrutural central'
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
tipo <- commandArgs(trailingOnly=TRUE)[1]
pasta <- file.path(trabalho,tipo)
source('C:/Users/dudup/OneDrive/japan-econometrics-model/src/7- forecast/bvar estrutural/experimento oferta estrutural central/funcoes gaussianas.R',encoding='UTF-8')
montar_modelo <- readRDS(file.path(pasta,'montar_modelo.rds'))
if(tipo=='original') {
  local <- file.path(raiz,'data/processed/forecast_pesos_exponenciais_20261026')
  post <- readRDS(file.path(local,'modelo.rds'));dados <- readRDS(file.path(local,'base.rds'))
  nomes <- c('ust2y','brent','atividade','cpi_core','juro_curto','inclinacao','usdjpy')
} else {
  local <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/data/processed/cenarios_propios_20261104/modelo_producao'
  post <- readRDS(file.path(local,'modelo.rds'));dados <- readRDS(file.path(local,'dados.rds'))
  nomes <- c('ust2y','brent','ip_mundial','atividade','cpi_core','juro_curto','inclinacao','usdjpy')
}
saida <- pasta;dir.create(saida,showWarnings=FALSE)
Y <- dados |> as_tibble() |> select(all_of(nomes)) |> as.matrix()
caminhos <- readRDS(file.path(pasta,'caminhos_novos.rds'))
indices <- unique(as.integer(seq(1,dim(post$posterior$A)[3],length.out=500)))
alvos <- c('atividade','cpi_core','juro_curto','inclinacao','usdjpy')
# 1. Reutilizar os tres cenarios comuns e o Base, sem recalcular ou mudar o BVAR.
anterior <- file.path(raiz,'src/Outputs/4- Forecast/choque unico Treasury/producao')
r_anterior <- readRDS(file.path(anterior,'resumo.rds'))
atividade_anterior <- readRDS(file.path(anterior,'atividade niveis.rds'))
base <- caminhos |> filter(cenario=='Base') |> arrange(date)
c <- caminhos |> filter(cenario=='Adverso estrutural') |> arrange(date)
h <- nrow(c); n <- length(nomes)
choques <- readRDS(file.path(pasta,'choques oferta.rds'))
resposta <- readRDS(file.path(pasta,'resposta oferta.rds'))
cond <- as.matrix(select(base,all_of(nomes)))
amostras <- medias <- array(NA_real_,c(5,h,length(indices)))
atividade_nivel <- matrix(NA_real_,h,length(indices))
trajetorias <- array(NA_real_,c(h,n,length(indices)))

# 2. Mudar apenas a media do Base pela resposta ao choque de oferta identificado.
# Nao voltar a condicionar a atividade aos novos niveis de Brent/Treasury/IP.
for(q in seq_along(indices)) {
  s<-indices[q];m<-montar_modelo(post,Y,s,h)
  fc<-prever_var_condicionado(m,cond)
  fc$media<-fc$media+resposta
  trajetorias[,,q]<-fc$media
  j_atividade<-match('atividade',nomes)
  pos_atividade<-(seq_len(h)-1L)*n+j_atividade
  atividade_nivel[,q]<-exp(fc$media[,j_atividade]/100+diag(fc$covariancia)[pos_atividade]/20000)
  ev<-eigen(fc$covariancia,symmetric=TRUE)
  set.seed(123+q)
  z<-matrix(as.vector(t(fc$media))+drop(ev$vectors%*%(sqrt(pmax(ev$values,0))*rnorm(n*h))),h,n,byrow=TRUE)
  for(k in seq_along(alvos)) {
    j<-match(alvos[k],nomes)
    for(t in seq_len(h)) {
      pos<-(t-1)*n+j;mu<-fc$media[t,j];v<-fc$covariancia[pos,pos]
      if(k<=2) {
        if(t<=12) {ancora<-Y[nrow(Y)+t-12,j];dif<-mu-ancora;vd<-v;realizado<-z[t,j]-ancora}
        else {pos0<-(t-13)*n+j;dif<-mu-fc$media[t-12,j]
          vd<-v+fc$covariancia[pos0,pos0]-2*fc$covariancia[pos,pos0];realizado<-z[t,j]-z[t-12,j]}
        medias[k,t,q]<-100*expm1(dif/100+vd/20000)
        amostras[k,t,q]<-100*expm1(realizado/100)
      } else if(k==5) {medias[k,t,q]<-exp(mu/100+v/20000);amostras[k,t,q]<-exp(z[t,j]/100)}
      else {medias[k,t,q]<-mu;amostras[k,t,q]<-z[t,j]}
    }
  }
  if(q%%100==0) cat('ESTRUTURAL',q,'/',length(indices),'\n')
}
novo <- map_dfr(1:5,function(k) tibble(modelo=tipo,cenario='Adverso estrutural',variavel=alvos[k],
  date=c$date,media=rowMeans(medias[k,,]),mediana=apply(amostras[k,,],1,median),
  inferior95=NA_real_,superior95=NA_real_,inferior80=NA_real_,superior80=NA_real_))
r <- bind_rows(filter(r_anterior,cenario!='Adverso estrutural'),novo)
niveis <- bind_rows(filter(atividade_anterior,cenario!='Adverso estrutural'),
  tibble(cenario='Adverso estrutural',date=c$date,atividade=rowMeans(atividade_nivel)))
saveRDS(niveis,file.path(saida,'atividade niveis.rds'))
saveRDS(r,file.path(saida,'resumo.rds'))
write_csv(r |> mutate(date=as.Date(date)),file.path(saida,'resumo.csv'))
saveRDS(list(medias=medias,amostras=amostras,resposta=resposta,choques=choques,
  trajetorias=trajetorias,metodo='Contrafactual com IRF mediana fixa; nao e a posterior conjunta do cenario'),
  file.path(saida,'Adverso estrutural.rds'))
for(cen in c('Base','Benigno','Adverso')) file.copy(file.path(anterior,paste0(cen,'.rds')),
  file.path(saida,paste0(cen,'.rds')),overwrite=TRUE)
# Trajetorias externas resultantes: Treasury e IP livres para responder ao choque.
novos_caminhos <- as_tibble(apply(trajetorias,c(1,2),mean),.name_repair='minimal')
names(novos_caminhos)<-nomes
novos_caminhos <- novos_caminhos |> mutate(date=c$date,cenario='Adverso estrutural',.before=1)
stopifnot(max(abs(novos_caminhos$brent[!is.na(c$brent)]-c$brent[!is.na(c$brent)]))<1e-8)
saveRDS(bind_rows(filter(caminhos,cenario!='Adverso estrutural'),novos_caminhos),
  file.path(saida,'caminhos resultantes.rds'))
write_csv(novos_caminhos |> mutate(date=as.Date(date)),file.path(saida,'trajetorias estruturais.csv'))
titulos <- c(atividade='Atividade | variacao em 12 meses (%)',cpi_core='CPI core | inflacao em 12 meses (%)',
  juro_curto='Juro curto (% a.a.)',inclinacao='JGB 10a - 2a (p.p.)',usdjpy='USD/JPY')
d<-r |> mutate(date=as.Date(date),variavel=factor(variavel,levels=alvos,labels=titulos[alvos]))
lab<-d |> group_by(cenario,variavel) |> filter(lubridate::month(date)==12|date==max(date)) |> ungroup() |>
  mutate(rotulo=if_else(grepl('USD',as.character(variavel)),formatC(media,format='f',digits=1,decimal.mark=','),formatC(media,format='f',digits=2,decimal.mark=',')))
separacao <- d |> group_by(variavel) |> summarise(sep=max(diff(range(media))*.03,.025),.groups='drop')
lab <- lab |> left_join(separacao,by='variavel') |> group_by(variavel,date) |> group_modify(~{ x<-arrange(.x,media); yy<-x$media; if(nrow(x)>1) for(i in 2:nrow(x)) yy[i]<-max(yy[i],yy[i-1]+x$sep[i]); x$rotulo_y<-yy;x }) |> ungroup()
cores<-c(Base='#2166AC',Benigno='#1B9E77',Adverso='#E69F00','Adverso estrutural'='#B2182B')
g<-ggplot(d,aes(date,media,colour=cenario))+geom_line(linewidth=.8)+
  geom_point(data=lab,size=1)+geom_segment(data=lab,aes(xend=date,yend=rotulo_y),alpha=.5,show.legend=FALSE)+geom_text(data=lab,aes(y=rotulo_y,label=rotulo),vjust=-.3,size=2.7,show.legend=FALSE)+
  facet_wrap(vars(variavel),ncol=2,scales='free_y')+scale_colour_manual(values=cores)+
  scale_x_date(date_breaks='1 year',date_labels='%Y')+theme_minimal(base_size=11)+
  theme(legend.position='bottom',strip.text=element_text(face='bold'),panel.grid.minor=element_blank())+
  labs(title=paste('Forecast |',if(tipo=='original') 'BVAR original de sete variaveis' else 'BVAR com producao mundial | so oferta identificada'),
    subtitle='Adverso estrutural via oferta | Brent fixado; demais variaveis respondem ao choque',x=NULL,y=NULL,colour=NULL,
    caption='Adverso estrutural: contrafactual pontual pela IRF mediana. Outros tres cenarios preservados.\nCaminho do Brent reproduzido por choques de oferta de intensidades calculadas; sem impor queda da atividade japonesa.')
ggsave(file.path(saida,'forecast cenarios.png'),g,width=15,height=12,dpi=160,bg='white')
gb<-ggplot(filter(d,cenario=='Base'),aes(date,media))+geom_ribbon(aes(ymin=inferior95,ymax=superior95),fill='#2166AC',alpha=.13)+
  geom_ribbon(aes(ymin=inferior80,ymax=superior80),fill='#2166AC',alpha=.23)+geom_line(colour='#2166AC',linewidth=.8)+
  geom_text(data=filter(lab,cenario=='Base'),aes(label=rotulo),vjust=-.7,size=3.2)+
  facet_wrap(vars(variavel),ncol=2,scales='free_y')+scale_x_date(date_breaks='1 year',date_labels='%Y')+
  theme_minimal(base_size=11)+labs(title=paste('Cenario base |',tipo),subtitle='Medias e intervalos de 80% e 95%',x=NULL,y=NULL)
ggsave(file.path(saida,'forecast base.png'),gb,width=15,height=12,dpi=160,bg='white')
cat('FORECAST_CONCLUIDO',tipo,'\n')
