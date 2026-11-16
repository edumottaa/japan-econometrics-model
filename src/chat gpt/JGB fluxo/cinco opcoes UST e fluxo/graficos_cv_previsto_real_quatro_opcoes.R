# Somente leitura dos pontos previstos e dos dados salvos. Nao estima modelos.
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages({library(fpp3); library(tidyverse)})
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
pasta <- file.path(raiz,'src/chat gpt/JGB fluxo/cinco opcoes UST e fluxo/resultados')
destino <- file.path(pasta,'apresentacao quatro opcoes','previsto contra realizado')
dir.create(destino,recursive=TRUE,showWarnings=FALSE)

meses <- c(jan=1,fev=2,mar=3,abr=4,mai=5,jun=6,jul=7,ago=8,set=9,out=10,nov=11,dez=12,
           feb=2,apr=4,may=5,aug=8,sep=9,oct=10,dec=12)
data_mensal <- function(x) {
  z <- str_split_fixed(str_to_lower(as.character(x)),' ',2)
  as.Date(sprintf('%s-%02d-01',z[,1],meses[z[,2]]))
}
opcoes <- c('1: UST30 exogeno','2: UST30 - UST2Y exogeno',
            '3: Satelite suave','4: Satelite rigido')
rotulos <- c('1: UST30','2: UST30 - UST2Y','3: Satelite suave','4: Satelite rigido')
cores <- set_names(c('#2563A6','#B39515','#00A779','#00A8DF','#171717'),
                   c(rotulos,'Realizado'))
alvos <- c('atividade','cpi_core','juro_curto','inclinacao','usdjpy')
escores <- read_csv(file.path(pasta,'CV original/comparacao/escores mesmas janelas.csv'),
                    show_col_types=FALSE) |>
  filter(.model %in% opcoes) |>
  mutate(date=data_mensal(date),origem=data_mensal(origem),
         modelo=rotulos[match(.model,opcoes)])
stopifnot(all(is.finite(escores$.mean)),all(escores$h>=1 & escores$h<=12),
          n_distinct(escores$origem)==9L,
          all(count(escores,origem,date,variavel)$n==4L))
historico <- readRDS(file.path(raiz,
  'data/processed/bvar_producao_tendencia_jgb30_5_20261112/dados.rds')) |>
  as_tibble() |> mutate(date=as.Date(date)) |>
  select(date,all_of(alvos)) |>
  pivot_longer(-date,names_to='variavel',values_to='valor') |>
  group_by(variavel) |> arrange(date,.by_group=TRUE) |>
  mutate(base_12m=lag(valor,12)) |> ungroup()
converter <- function(valor,variavel,base_12m) {
  case_when(variavel %in% c('atividade','cpi_core')~100*expm1((valor-base_12m)/100),
            variavel=='usdjpy'~exp(valor/100),variavel=='inclinacao'~100*valor,
            TRUE~valor)
}
previstos <- escores |>
  left_join(select(historico,date,variavel,base_12m),by=c('date','variavel')) |>
  mutate(valor=converter(.mean,variavel,base_12m)) |>
  select(origem,date,h,variavel,modelo,valor)
realizados <- escores |> distinct(origem,date,h,variavel,observado) |>
  left_join(select(historico,date,variavel,base_12m),by=c('date','variavel')) |>
  mutate(valor=converter(observado,variavel,base_12m),modelo='Realizado') |>
  select(origem,date,h,variavel,modelo,valor)
ancoras <- crossing(origem=sort(unique(escores$origem)),variavel=alvos,
                    modelo=c(rotulos,'Realizado')) |>
  mutate(date=origem,h=0L) |>
  left_join(historico,by=c('date','variavel')) |>
  mutate(valor=converter(valor,variavel,base_12m)) |>
  select(origem,date,h,variavel,modelo,valor)
dados <- bind_rows(previstos,realizados,ancoras) |>
  mutate(janela=paste('Origem:',format(origem,'%m/%Y')),
         janela=factor(janela,levels=paste('Origem:',format(sort(unique(origem)),'%m/%Y'))),
         modelo=factor(modelo,levels=c(rotulos,'Realizado'))) |>
  arrange(origem,variavel,modelo,date)
stopifnot(all(is.finite(dados$valor)),nrow(previstos)==4L*5L*86L,
          all(yearmonth(previstos$date)-12L<=yearmonth(previstos$origem)))
nomes <- c(atividade='Atividade japonesa',cpi_core='Inflacao core',
           juro_curto='Juro curto',inclinacao='Spread JGB30Y - JGB5Y',usdjpy='USD/JPY')
unidades <- c(atividade='Variacao em 12 meses (%)',cpi_core='Variacao em 12 meses (%)',
              juro_curto='% a.a.',inclinacao='Pontos-base',usdjpy='Ienes por dolar')
for(alvo in alvos) {
  x <- filter(dados,variavel==alvo)
  g <- ggplot(x,aes(date,valor,colour=modelo,group=modelo))+
    geom_line(data=filter(x,modelo!='Realizado'),linewidth=.85)+
    geom_line(data=filter(x,modelo=='Realizado'),linewidth=1.05)+
    geom_point(data=filter(x,modelo=='Realizado'),size=1.1,show.legend=FALSE)+
    facet_wrap(vars(janela),ncol=3,scales='free_x')+
    scale_colour_manual(values=cores,drop=FALSE)+
    scale_x_date(date_breaks='3 months',date_labels='%m/%y',
                 expand=expansion(mult=c(.035,.04)))+
    guides(colour=guide_legend(nrow=1,override.aes=list(linewidth=1.2)))+
    labs(title=paste(nomes[[alvo]],'| previsto contra realizado no CV'),
         subtitle='Quatro opcoes nas mesmas nove origens | previsoes salvas, sem reestimacao',
         x=NULL,y=unidades[[alvo]],colour=NULL,
         caption=paste0('Cada painel parte do ultimo valor conhecido na origem. Horizontes de 1 a 12 meses; ',
           'ultimos paineis limitados aos dados ate ago/2026.\n',
           'CV condicional ex post: juros externos, Brent e IP realizados; OIS realizado no satelite. ',
           'Fluxo futuro no satelite = media pos-YCC disponivel na origem.\n',
           'Atividade/CPI em 12 meses para leitura; metricas do CV foram calculadas nos niveis em log. ',
           'Linhas mostram apenas trajetorias centrais.'))+
    theme_minimal(base_size=12)+
    theme(legend.position='top',legend.text=element_text(size=11),
          panel.grid.minor=element_blank(),strip.text=element_text(face='bold',size=12),
          plot.title=element_text(face='bold',size=19),
          plot.subtitle=element_text(size=12),plot.caption=element_text(hjust=0,size=9),
          panel.spacing=grid::unit(1.1,'lines'))
  ggsave(file.path(destino,paste0('CV previsto x realizado ',alvo,'.png')),
         g,width=14,height=9.3,dpi=155,bg='white')
}
write_csv(mutate(dados,modelo=as.character(modelo),janela=as.character(janela)),
          file.path(destino,'valores usados nos graficos.csv'))
message('Cinco graficos gerados: 9 origens, 86 previsoes por modelo/variavel. Nenhum modelo reestimado.')
