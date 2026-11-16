# Somente graficos de resultados salvos. Nao chama estimacao, forecast() ou CV.
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages({library(fpp3);library(tidyverse)})
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
pasta <- file.path(raiz,'src/chat gpt/JGB fluxo/cinco opcoes UST e fluxo/resultados')
destino <- file.path(pasta,'apresentacao quatro opcoes')
dir.create(destino,recursive=TRUE,showWarnings=FALSE)
domesticas <- c('atividade','cpi_core','juro_curto','inclinacao','usdjpy')
opcoes <- c('1: UST30 exogeno','2: UST30 - UST2Y exogeno','3: Satelite suave','4: Satelite rigido')
cores <- c('1: UST30 exogeno'='#2563A6','2: UST30 - UST2Y exogeno'='#B39515',
  '3: Satelite suave'='#00A779','4: Satelite rigido'='#00A8DF',
  'BVAR original ex post'='#8563A0',Naive='#70836B',Drift='#999999')

# 1. CV comparavel: mesmas origens/datas/horizontes nas quatro opcoes.
metricas <- read_csv(file.path(pasta,'CV original/comparacao/metricas por periodo - mesmas janelas.csv'),
  show_col_types=FALSE) |> filter(periodo=='Geral')
escores <- read_csv(file.path(pasta,'CV original/comparacao/escores mesmas janelas.csv'),show_col_types=FALSE)
meses <- c(jan=1,fev=2,mar=3,abr=4,mai=5,jun=6,jul=7,ago=8,set=9,out=10,nov=11,dez=12,
  feb=2,apr=4,may=5,aug=8,sep=9,oct=10,dec=12)
data_mensal <- function(x) {
  z <- str_split_fixed(str_to_lower(as.character(x)),' ',2)
  as.Date(sprintf('%s-%02d-01',z[,1],meses[z[,2]]))
}
datas_cv <- data_mensal(escores$date)
faixa_cv <- paste(format(min(datas_cv),'%m/%Y'),'a',format(max(datas_cv),'%m/%Y'))
nomes_cv <- c(atividade='Atividade | erro em 100 x log do indice',
  cpi_core='CPI core | erro em 100 x log do indice',juro_curto='Juro curto | erro em p.p.',
  inclinacao='Spread 30Y-5Y | erro em p.p.',usdjpy='USD/JPY | erro em 100 x log')
metricas <- metricas |> mutate(
  variavel=factor(variavel,levels=domesticas),
  .model=factor(.model,levels=rev(names(cores))))
for(metrica in c('RMSE','CRPS')) {
  g <- ggplot(metricas,aes(.data[[metrica]],.model,fill=.model))+geom_col(width=.62)+
    geom_text(aes(label=formatC(.data[[metrica]],format='f',digits=3,decimal.mark=',')),
      hjust=-.1,size=3.5)+
    facet_wrap(vars(variavel),ncol=2,scales='free_x',labeller=as_labeller(nomes_cv))+
    scale_fill_manual(values=cores,guide='none')+
    scale_x_continuous(expand=expansion(mult=c(0,.25)))+theme_minimal(base_size=12)+
    theme(panel.grid.major.y=element_blank(),panel.grid.minor=element_blank(),
      strip.text=element_text(face='bold'),plot.title=element_text(face='bold'),
      plot.caption=element_text(hjust=0))+
    labs(title=paste('CV das quatro opcoes |',metrica,'| cinco variaveis'),
      subtitle=paste('Datas previstas:',faixa_cv,'|',n_distinct(escores$origem),
        'origens comuns | horizontes observados 1-12'),x=paste(metrica,'| menor = melhor'),y=NULL,
      caption='CV reduzido original, condicionado aos caminhos externos realizados. Mesmas observacoes entre modelos.\nO periodo deste grafico e a amostra comum; a atividade e o CPI nao estao em taxas de crescimento anual.')
  ggsave(file.path(destino,paste0('CV quatro opcoes cinco variaveis ',metrica,'.png')),
    g,width=15,height=13,dpi=150,bg='white')
}
write_csv(metricas |> mutate(across(c(.model,variavel),as.character)),
  file.path(destino,'CV quatro opcoes mesmas janelas.csv'))

# 2. Forecast Base das cinco domesticas, separadamente por opcao.
historico <- readRDS(file.path(raiz,'data/processed/bvar_producao_tendencia_jgb30_5_20261112/dados.rds')) |>
  as_tibble() |> select(date,all_of(domesticas)) |>
  mutate(date=as.Date(date),across(c(atividade,cpi_core,usdjpy),~exp(.x/100))) |>
  pivot_longer(-date,names_to='variavel',values_to='valor')
futuro <- read_csv(file.path(pasta,'comparacao/todos forecasts.csv'),show_col_types=FALSE) |>
  filter(cenario=='Base',variavel %in% domesticas,
    modelo %in% c(opcoes,'Referencia: BVAR original')) |>
  select(date,modelo,variavel,valor) |> distinct() |> mutate(date=as.Date(date))
marcas_datas <- seq(as.Date(paste0(format(min(futuro$date),'%Y'),'-01-01')),
  max(futuro$date),by='6 months')
plotar <- futuro |> group_by(modelo) |> group_modify(function(x,k) {
  bind_rows(select(historico,date,variavel,valor),select(x,date,variavel,valor)) |>
    arrange(variavel,date) |> group_by(variavel) |>
    mutate(valor_plot=case_when(variavel %in% c('atividade','cpi_core')~100*(valor/lag(valor,12)-1),
      variavel=='inclinacao'~100*valor,TRUE~valor)) |>
    ungroup() |> filter(date %in% x$date)
}) |> ungroup()
nomes_fore <- c(atividade='Atividade | variacao em 12 meses (%)',
  cpi_core='Inflacao core | variacao em 12 meses (%)',juro_curto='Juro curto | % a.a.',
  inclinacao='Spread JGB30Y - JGB5Y | pontos-base',usdjpy='USD/JPY | ienes por dolar')
for(i in 1:4) {
  x <- plotar |> filter(modelo %in% c(opcoes[i],'Referencia: BVAR original')) |>
    mutate(variavel=factor(variavel,levels=domesticas),
      serie=if_else(modelo==opcoes[i],opcoes[i],'BVAR original'))
  rotulos <- x |> filter(modelo==opcoes[i]) |> group_by(variavel) |>
    slice_max(date,n=1,with_ties=FALSE) |> ungroup() |>
    mutate(rotulo=formatC(valor_plot,format='f',digits=2,decimal.mark=','))
  g <- ggplot(x,aes(date,valor_plot,colour=serie,linetype=serie))+geom_line(linewidth=.9)+
    geom_point(data=rotulos,size=1.8,show.legend=FALSE)+
    geom_text(data=rotulos,aes(label=rotulo),hjust=-.12,size=3.7,show.legend=FALSE)+
    facet_wrap(vars(variavel),ncol=2,scales='free_y',labeller=as_labeller(nomes_fore))+
    scale_colour_manual(values=c(set_names(cores[[opcoes[i]]],opcoes[i]),'BVAR original'='#777777'))+
    scale_linetype_manual(values=c(set_names('solid',opcoes[i]),'BVAR original'='dashed'))+
    scale_x_date(breaks=marcas_datas,date_labels='%m/%Y',expand=expansion(mult=c(.02,.22)))+
    theme_minimal(base_size=12)+theme(legend.position='bottom',panel.grid.minor=element_blank(),
      strip.text=element_text(face='bold'),plot.title=element_text(face='bold'),
      plot.caption=element_text(hjust=0))+
    labs(title=paste('Cenario Base |',opcoes[i]),
      subtitle='Forecast das cinco variaveis domesticas | valores finais rotulados',
      x=NULL,y=NULL,colour=NULL,linetype=NULL,
      caption='Centros condicionais salvos; nao sao bandas preditivas. Juro curto terminal condicionado entre 1,75% e 2,25%.\nAtividade/CPI: crescimento em 12 meses. Comparacao tracejada: BVAR original recalculado com o mesmo condicionamento.')
  ggsave(file.path(destino,paste0('Forecast Base opcao ',i,' cinco variaveis.png')),
    g,width=14,height=11,dpi=150,bg='white')
}
write_csv(plotar,file.path(destino,'Forecast Base cinco variaveis.csv'))

# 3. Comparacao direta: quatro opcoes sobrepostas nos mesmos cinco paineis.
comparacao <- plotar |> filter(modelo %in% opcoes) |>
  mutate(variavel=factor(variavel,levels=domesticas))
faixas <- comparacao |> group_by(variavel) |>
  summarise(espaco=max(diff(range(valor_plot))*.04,.02),.groups='drop')
rotulos <- comparacao |> group_by(modelo,variavel) |>
  slice_max(date,n=1,with_ties=FALSE) |> ungroup() |>
  left_join(faixas,by='variavel') |> group_by(variavel) |>
  arrange(valor_plot,modelo,.by_group=TRUE) |>
  group_modify(function(x,k) {
    x$y_rotulo <- x$valor_plot
    if(nrow(x)>1L) for(j in 2:nrow(x))
      x$y_rotulo[j] <- max(x$y_rotulo[j],x$y_rotulo[j-1L]+x$espaco[j])
    x
  }) |> ungroup() |>
  mutate(data_rotulo=date+60L,
    texto=paste0(match(modelo,opcoes),': ',formatC(valor_plot,format='f',digits=2,decimal.mark=',')))
g <- ggplot(comparacao,aes(date,valor_plot,colour=modelo))+
  geom_line(linewidth=.9)+geom_point(data=rotulos,size=1.7,show.legend=FALSE)+
  geom_segment(data=rotulos,aes(xend=data_rotulo,yend=y_rotulo),
    linewidth=.3,show.legend=FALSE)+
  geom_text(data=rotulos,aes(x=data_rotulo,y=y_rotulo,label=texto),
    hjust=0,size=3.4,show.legend=FALSE)+
  facet_wrap(vars(variavel),ncol=2,scales='free_y',labeller=as_labeller(nomes_fore))+
  scale_colour_manual(values=cores[opcoes])+
  scale_x_date(breaks=marcas_datas,date_labels='%m/%Y',
    expand=expansion(mult=c(.02,.2)))+
  theme_minimal(base_size=12)+theme(legend.position='bottom',
    panel.grid.minor=element_blank(),strip.text=element_text(face='bold'),
    plot.title=element_text(face='bold'),plot.caption=element_text(hjust=0))+
  guides(colour=guide_legend(nrow=2))+
  labs(title='Cenario Base | comparacao das quatro opcoes',
    subtitle=paste('Set/2026 a set/2028 | mesmas escalas por variavel | rotulos: opcao e valor final'),
    x=NULL,y=NULL,colour=NULL,
    caption='Centros condicionais salvos. Atividade/CPI: variacao em 12 meses; juros: % a.a.; spread: pb; cambio: JPY/USD.\n1: UST30 exogeno | 2: UST30-UST2Y exogeno | 3: satelite suave | 4: satelite rigido.')
ggsave(file.path(destino,'Forecast Base comparacao quatro opcoes cinco variaveis.png'),
  g,width=15,height=11,dpi=150,bg='white')
message('Graficos dos CVs e forecasts salvos gerados. Nenhum modelo ou CV reexecutado.')
