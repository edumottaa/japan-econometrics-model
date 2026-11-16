Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse));suppressPackageStartupMessages(library(fpp3))
suppressPackageStartupMessages(library(bsvarSIGNs))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/Outputs/4- Forecast/jgb30y_5y com tendencia'
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
tipo <- 'producao'
pasta <- file.path(trabalho,'producao')
saida <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/bvar fluxo estrangeiro 30-5/resultados'
alvos <- c('atividade','cpi_core','juro_curto','inclinacao','usdjpy')
r <- readRDS(file.path(saida,'resumo.rds'))
titulos <- c(atividade='Atividade | variacao em 12 meses (%)',cpi_core='CPI core | inflacao em 12 meses (%)',
  juro_curto='Juro curto (% a.a.)',inclinacao='JGB 30a - 5a (p.p.)',usdjpy='USD/JPY')
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
  labs(title='Forecast | BVAR com fluxo estrangeiro no 30a-5a (teste)',
    subtitle='Setembro/2026 a setembro/2028 | cenarios via oferta e condicao terminal do juro',x=NULL,y=NULL,colour=NULL,
    caption='Juro em setembro/2028 condicionado entre 1,75% e 2,25% em cada cenario. A banda nao e intervalo de confianca.\nBrent reproduzido pelos choques de oferta; demais variaveis tambem respondem ao condicionamento do juro.')
ggsave(file.path(saida,'forecast cenarios.png'),g,width=15,height=12,dpi=160,bg='white')
gb<-ggplot(filter(d,cenario=='Base'),aes(date,media))+geom_ribbon(aes(ymin=inferior95,ymax=superior95),fill='#2166AC',alpha=.13)+
  geom_ribbon(aes(ymin=inferior80,ymax=superior80),fill='#2166AC',alpha=.23)+geom_line(colour='#2166AC',linewidth=.8)+
  geom_text(data=filter(lab,cenario=='Base'),aes(label=rotulo),vjust=-.7,size=3.2)+
  facet_wrap(vars(variavel),ncol=2,scales='free_y')+scale_x_date(date_breaks='1 year',date_labels='%Y')+
  theme_minimal(base_size=11)+labs(title='Cenario base | BVAR com fluxo estrangeiro no 30a-5a (teste)',
    subtitle='Setembro/2026 a setembro/2028 | medias e intervalos condicionais de 80% e 95%',x=NULL,y=NULL,
    caption='Juro terminal entre 1,75% e 2,25%. A media dentro da banda e determinada pelo modelo, nao fixada em 2%.')
ggsave(file.path(saida,'forecast base.png'),gb,width=15,height=12,dpi=160,bg='white')
if (interactive()) { print(g); print(gb) }
cat('FORECAST_CONCLUIDO',tipo,'\n')
