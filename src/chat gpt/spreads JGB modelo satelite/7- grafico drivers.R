Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/resultados'
# Le o forecast principal (amostra completa, com a constante de tendencia). Opcional: sufixo.
sufixo <- commandArgs(trailingOnly=TRUE)[1]; if(is.na(sufixo)) sufixo <- ''
fc <- readRDS(file.path(trabalho,paste0('forecast',sufixo,'.rds')))
entrada <- readRDS(file.path(trabalho,'dados.rds'))
dados <- entrada$dados; base <- entrada$base
projecao <- fc$projecao; modelos <- fc$modelos
referencia <- yearmonth('2018 Sep')   # primeiro mes com todas as defasagens do modelo
nivel <- c(d_s30_10='s30_10',d_s30_5='s30_5')
componentes <- c('OIS JPY 2Y','UST 30Y','Compra estrangeira','Residuo')

# Parcelas acumuladas desde a referencia: beta x regressor (historico observado e, depois,
# projetado). Residuo = variacao do spread - parcelas: no historico, constante + choques;
# no forecast, constante + dinamica do erro AR.
parcelas <- function(cen,alvo) {
  p <- projecao |> filter(cenario==cen,alvo==!!alvo) |> arrange(mes)
  serie <- bind_rows(as_tibble(dados) |> select(mes,d_ust30,d_ois2,liq_estr,d_fatia,dy=all_of(alvo)),
    p |> select(mes,d_ust30,d_ois2,liq_estr,d_fatia,dy=variacao)) |> arrange(mes)
  t <- tidy(modelos[[alvo]]) |> filter(!str_detect(term,'^(ar|ma)[0-9]+$|intercept'))
  meses <- serie$mes[serie$mes>referencia]
  x <- map(t$term,function(tt) { v <- str_remove(str_remove(tt,'^lag\\('),',.*$')
    k <- if(str_detect(tt,'^lag')) as.integer(str_extract(tt,'[0-9]+(?=\\)$)')) else 0L
    t$estimate[t$term==tt]*serie[[v]][match(meses,serie$mes)-k] })
  names(x) <- coalesce(c(d_ust30='UST 30Y',d_ois2='OIS JPY 2Y')[t$term],'Compra estrangeira')
  inicio <- base[[nivel[[alvo]]]][base$mes==referencia]
  tibble(alvo=alvo,mes=meses,dy=serie$dy[match(meses,serie$mes)],!!!x) |>
    mutate(Residuo=dy-rowSums(across(all_of(setdiff(componentes,'Residuo')))),
      projetado=mes>max(dados$mes)) |>
    mutate(across(all_of(componentes),cumsum),inicio=inicio,spread=inicio+cumsum(dy),
      explicado=inicio+rowSums(across(all_of(setdiff(componentes,'Residuo')))))
}

# Faixas empilhadas: parcelas positivas acima do nivel inicial, negativas abaixo.
faixas <- function(d) d |> pivot_longer(all_of(componentes),names_to='parcela',values_to='pb') |>
  mutate(parcela=factor(parcela,levels=componentes)) |> arrange(alvo,mes,parcela) |>
  group_by(alvo,mes) |>
  mutate(pos=pmax(pb,0),neg=pmin(pb,0),
    topo_pos=inicio+cumsum(pos),base_pos=topo_pos-pos,
    fundo_neg=inicio+cumsum(neg),teto_neg=fundo_neg-neg) |> ungroup()

desenha <- function(cen) {
  d <- map_dfr(names(nivel),~ parcelas(cen,.x))
  f <- faixas(d)
  ponte <- d |> filter(mes==max(dados$mes)) |> mutate(projetado=TRUE)   # liga historico e forecast
  linhas <- bind_rows(d,ponte)
  datas <- yearmonth(c('2020 Dec','2022 Dec','2024 Dec')) |> c(max(dados$mes),max(d$mes))
  rotulos <- f |> filter(parcela=='Residuo',mes %in% datas) |>
    mutate(y=if_else(pb>=0,(base_pos+topo_pos)/2,(fundo_neg+teto_neg)/2),
      texto=paste0(format(as.Date(mes),'%m/%Y'),': ',ifelse(pb>0,'+',''),round(pb),' pb'),
      ajuste=if_else(mes==max(mes),1.05,.5))
  rotulo_alvo <- c(d_s30_10='JGB 30Y - 10Y',d_s30_5='JGB 30Y - 5Y')
  cores <- c(`Nivel inicial`='#e3e6ea',`OIS JPY 2Y`='#e8a59c',`UST 30Y`='#a9bfee',
    `Compra estrangeira`='#f2cf8a',Residuo='#93cca7')
  ggplot()+
    geom_rect(data=distinct(d,alvo,inicio),aes(xmin=as.Date(referencia),xmax=as.Date(max(d$mes)),
      ymin=pmin(0,inicio),ymax=inicio,fill='Nivel inicial'))+
    geom_ribbon(data=f,aes(as.Date(mes),ymin=base_pos,ymax=topo_pos,fill=parcela),alpha=.85)+
    geom_ribbon(data=f,aes(as.Date(mes),ymin=fundo_neg,ymax=teto_neg,fill=parcela),alpha=.85)+
    annotate('rect',xmin=as.Date(max(dados$mes)),xmax=as.Date(max(d$mes)),ymin=-Inf,ymax=Inf,
      fill='grey40',alpha=.06)+
    geom_vline(xintercept=as.Date(max(dados$mes)),colour='grey45',linewidth=.35)+
    geom_line(data=linhas |> filter(!projetado | mes==max(dados$mes)),aes(as.Date(mes),explicado),
      colour='grey30',linetype='dashed',linewidth=.45)+
    geom_line(data=linhas |> filter(projetado),aes(as.Date(mes),explicado),
      colour='grey30',linetype='dashed',linewidth=.45)+
    geom_line(data=linhas |> filter(!projetado),aes(as.Date(mes),spread),colour='black',linewidth=.8)+
    geom_line(data=linhas |> filter(projetado),aes(as.Date(mes),spread),colour='black',linewidth=.8,linetype='22')+
    geom_label(data=rotulos,aes(as.Date(mes),y,label=texto,hjust=ajuste),size=3,colour='grey20',
      fill='#eef7f1',linewidth=.25,label.padding=unit(.15,'lines'))+
    facet_wrap(vars(alvo),ncol=1,scales='free_y',labeller=as_labeller(rotulo_alvo))+
    scale_fill_manual(values=cores,breaks=names(cores))+
    scale_x_date(date_breaks='1 year',date_labels='%Y',expand=expansion(mult=c(.01,.01)))+
    theme_minimal(base_size=12)+
    theme(legend.position='bottom',panel.grid.minor=element_blank(),
      strip.text=element_text(face='bold',hjust=0,size=12),plot.title=element_text(size=14))+
    labs(title='Spreads JGB: nivel e parcelas acumuladas',
      subtitle=paste0('Referencia: ',format(as.Date(referencia),'%m/%Y'),'  |  Linha preta: spread observado (cheia) e projetado (pontilhada)  |  ',
        'Forecast: ',cen),
      x=NULL,y='pb',fill='Parcela',
      caption=paste('Parcela = beta x variavel acumulada. Tracejado: nivel inicial + parcelas explicadas.',
        '\nResiduo = constante de tendencia + choques (historico) ou dinamica do erro AR (forecast). Area sombreada: forecast.'))
}

# Armazenamento da saida em outputs.
for(cen in unique(projecao$cenario)) {
  arquivo <- paste0('drivers spreads ',if(cen=='Trajetoria do mercado') 'mercado' else 'nosso OIS',sufixo)
  ggsave(file.path(trabalho,paste0(arquivo,'.png')),desenha(cen),width=12,height=9,dpi=160,bg='white')
  write_csv(map_dfr(names(nivel),~ parcelas(cen,.x)) |> mutate(mes=as.Date(mes)),file.path(trabalho,paste0(arquivo,'.csv')))
}
cat('GRAFICO_DRIVERS_CONCLUIDO\n')
