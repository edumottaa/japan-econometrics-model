# Grafico do UST 30Y: media mensal desde 2000 (FRED DGS30) e curva a termo Bloomberg (30/10/2026).
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
pasta <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite'
hist <- read_csv(file.path(pasta,'dados/DGS30_2026-11-04.csv'),na=c('','.'),show_col_types=FALSE) |>
  filter(is.finite(DGS30)) |> group_by(mes=floor_date(observation_date,'month')) |>
  summarise(taxa=mean(DGS30),dias=n(),.groups='drop') |> filter(mes>=as.Date('2000-01-01'),dias>=10)  # descarta mes incompleto
termo <- read_csv(file.path(pasta,'dados/curvas_bloomberg.csv'),show_col_types=FALSE) |>
  filter(variavel=='ust30') |> transmute(mes=floor_date(data_snapshot,'month') %m+% months(horizonte_meses),taxa=taxa_pct)
ultimo <- slice_max(hist,mes,n=1); fim <- slice_max(termo,mes,n=1)
azul <- '#2a78d6'
g <- ggplot()+
  geom_line(data=hist,aes(mes,taxa),colour=azul,linewidth=.7)+
  geom_line(data=termo,aes(mes,taxa),colour=azul,linewidth=.7,linetype='22')+
  geom_point(data=bind_rows(ultimo,fim),aes(mes,taxa),colour=azul,size=2)+
  annotate('text',x=ultimo$mes,y=ultimo$taxa,label=sprintf('%s: %.2f%%',format(ultimo$mes,'%m/%Y'),ultimo$taxa),
    hjust=1.08,vjust=-.6,size=3.4,colour='#0b0b0b')+
  annotate('text',x=fim$mes,y=fim$taxa,label=sprintf('a termo
%s: %.2f%%',format(fim$mes,'%m/%Y'),fim$taxa),
    hjust=.9,vjust=1.5,size=3.4,colour='#52514e')+
  scale_x_date(date_breaks='2 years',date_labels='%Y',expand=expansion(mult=c(.01,.03)))+
  scale_y_continuous(labels=function(x) paste0(x,'%'))+
  theme_minimal(base_size=12)+
  theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
    panel.grid.major.y=element_line(colour='#e6e5e0'),plot.background=element_rect(fill='#fcfcfb',colour=NA),
    axis.text=element_text(colour='#52514e'),plot.subtitle=element_text(colour='#52514e'))+
  labs(title='US Treasury 30 anos (% a.a.)',
    subtitle='Media mensal desde jan/2000 (linha cheia) e curva a termo Bloomberg de 30/10/2026 (tracejada)',
    x=NULL,y=NULL,caption='Fonte: FRED (DGS30); Bloomberg FWCV (YCGT0025, 30Y par).')
ggsave(file.path(pasta,'resultados/grafico UST30.png'),g,width=11,height=5.5,dpi=160,bg='#fcfcfb')
print(ultimo); print(fim); print(hist |> summarise(min=min(taxa),max=max(taxa),media=mean(taxa)))
