Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse));suppressPackageStartupMessages(library(fpp3))
suppressPackageStartupMessages(library(bsvarSIGNs))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/choque unico Treasury'
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
tipo <- commandArgs(trailingOnly=TRUE)[1]
pasta <- file.path(trabalho,tipo)
source('C:/Users/dudup/OneDrive/japan-econometrics-model/src/7- forecast/bvar estrutural/experimento choque unico Treasury/funcoes gaussianas.R',encoding='UTF-8')
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
resultados <- list(); niveis_atividade <- list()
for(cen in unique(caminhos$cenario)) {
  c <- caminhos |> filter(cenario==cen) |> arrange(date);h <- nrow(c);n<-length(nomes)
  cond <- as.matrix(select(c,all_of(nomes)))
  amostras <- medias <- array(NA_real_,c(5,h,length(indices)))
  atividade_nivel <- matrix(NA_real_,h,length(indices))
  for(q in seq_along(indices)) {
    s<-indices[q];m<-montar_modelo(post,Y,s,h);fc<-prever_var_condicionado(m,cond)
    j_atividade <- match('atividade',nomes)
    pos_atividade <- (seq_len(h)-1L)*n+j_atividade
    atividade_nivel[,q] <- exp(fc$media[,j_atividade]/100+diag(fc$covariancia)[pos_atividade]/20000)
    ev <- eigen(fc$covariancia,symmetric=TRUE)
    set.seed(123+q)
    z <- as.vector(t(fc$media))+drop(ev$vectors%*%(sqrt(pmax(ev$values,0))*rnorm(n*h)))
    z <- matrix(z,h,n,byrow=TRUE)
    for(k in seq_along(alvos)) {
      j <- match(alvos[k],nomes)
      for(t in seq_len(h)) {
        pos <- (t-1)*n+j;mu<-fc$media[t,j];v<-fc$covariancia[pos,pos]
        if(k<=2) {
          if(t<=12) { ancora<-Y[nrow(Y)+t-12,j];dif<-mu-ancora;vd<-v;realizado<-z[t,j]-ancora }
          else { pos0<-(t-13)*n+j;dif<-mu-fc$media[t-12,j]
            vd<-v+fc$covariancia[pos0,pos0]-2*fc$covariancia[pos,pos0];realizado<-z[t,j]-z[t-12,j] }
          medias[k,t,q] <- 100*expm1(dif/100+vd/20000)
          amostras[k,t,q] <- 100*expm1(realizado/100)
        } else if(k==5) {
          medias[k,t,q]<-exp(mu/100+v/20000);amostras[k,t,q]<-exp(z[t,j]/100)
        } else { medias[k,t,q]<-mu;amostras[k,t,q]<-z[t,j] }
      }
    }
    if(q%%100==0) cat(tipo,cen,q,'/',length(indices),'\n')
  }
  niveis_atividade[[cen]] <- tibble(cenario=cen,date=c$date,atividade=rowMeans(atividade_nivel))
  resultados[[cen]] <- map_dfr(1:5,function(k) tibble(modelo=tipo,cenario=cen,variavel=alvos[k],
    date=c$date,media=rowMeans(medias[k,,]),mediana=apply(amostras[k,,],1,median),
    inferior95=apply(amostras[k,,],1,quantile,.025),superior95=apply(amostras[k,,],1,quantile,.975),
    inferior80=apply(amostras[k,,],1,quantile,.1),superior80=apply(amostras[k,,],1,quantile,.9)))
  saveRDS(list(amostras=amostras,medias=medias,condicoes=cond),file.path(saida,paste0(cen,'.rds')))
}
saveRDS(bind_rows(niveis_atividade),file.path(saida,'atividade niveis.rds'))
r <- bind_rows(resultados);saveRDS(r,file.path(saida,'resumo.rds'))
write_csv(r |> mutate(date=as.Date(date)),file.path(saida,'resumo.csv'))
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
    subtitle='Brent fixado | Treasury de mercado; ajuste por choque unico apenas no adverso estrutural',x=NULL,y=NULL,colour=NULL,
    caption='Medias condicionais. Cada modelo utiliza sua propria IRF. Producao mundial projetada por regressao com erros ARIMA.\nTrajetorias externas fixadas em seus valores centrais; intervalos nao incluem a incerteza dessas trajetorias.')
ggsave(file.path(saida,'forecast cenarios.png'),g,width=15,height=12,dpi=160,bg='white')
gb<-ggplot(filter(d,cenario=='Base'),aes(date,media))+geom_ribbon(aes(ymin=inferior95,ymax=superior95),fill='#2166AC',alpha=.13)+
  geom_ribbon(aes(ymin=inferior80,ymax=superior80),fill='#2166AC',alpha=.23)+geom_line(colour='#2166AC',linewidth=.8)+
  geom_text(data=filter(lab,cenario=='Base'),aes(label=rotulo),vjust=-.7,size=3.2)+
  facet_wrap(vars(variavel),ncol=2,scales='free_y')+scale_x_date(date_breaks='1 year',date_labels='%Y')+
  theme_minimal(base_size=11)+labs(title=paste('Cenario base |',tipo),subtitle='Medias e intervalos de 80% e 95%',x=NULL,y=NULL)
ggsave(file.path(saida,'forecast base.png'),gb,width=15,height=12,dpi=160,bg='white')
cat('FORECAST_CONCLUIDO',tipo,'\n')
