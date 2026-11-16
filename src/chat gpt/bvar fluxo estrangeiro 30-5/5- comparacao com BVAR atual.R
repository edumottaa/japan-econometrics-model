# Compara o BVAR com fluxo estrangeiro (teste) com o BVAR de producao, cenario Base.
Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
pasta_teste <- file.path(raiz,'src/chat gpt/bvar fluxo estrangeiro 30-5')
le <- function(arquivo,modelo) read_csv(arquivo,show_col_types=FALSE) |> filter(cenario=='Base') |>
  transmute(modelo,variavel,date,media,inferior80,superior80)
r <- bind_rows(le(file.path(raiz,'src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao/resumo.csv'),'BVAR atual'),
  le(file.path(pasta_teste,'resultados/resumo.csv'),'BVAR com fluxo'))
tabela <- r |> filter(date %in% as.Date(c('2026-12-01','2027-12-01','2028-09-01'))) |>
  mutate(media=if_else(variavel=='inclinacao',100*media,media)) |>
  select(variavel,date,modelo,media) |> pivot_wider(names_from=modelo,values_from=media) |>
  mutate(diferenca=`BVAR com fluxo`-`BVAR atual`) |> arrange(variavel,date)
print(tabela |> mutate(across(where(is.double),~round(.x,2))),n=Inf)
write_csv(tabela,file.path(pasta_teste,'resultados/comparacao com BVAR atual.csv'))

# Grafico do 30-5 (pb): historico, BVAR atual, BVAR com fluxo e satelite final.
hist <- read_csv(file.path(raiz,'data/processed/jgb30y_5y_mof_mensal.csv'),show_col_types=FALSE) |>
  filter(date>=as.Date('2018-06-01'),mes_completo) |> transmute(date,nivel=100*inclinacao_30_5)
sat <- read_csv(file.path(raiz,'src/chat gpt/spreads JGB modelo satelite/resultados/forecast 30-5 modelo final.csv'),show_col_types=FALSE) |>
  transmute(modelo='Satelite final',date=mes,media=nivel) |> filter(date<=as.Date('2028-09-01'))
spr <- r |> filter(variavel=='inclinacao') |> mutate(across(c(media,inferior80,superior80),~100*.x))
cores <- c(`BVAR atual`='grey45',`BVAR com fluxo`='#2166AC',`Satelite final`='#E69F00')
fim <- bind_rows(spr,sat) |> group_by(modelo) |> slice_max(date,n=1)
g <- ggplot()+
  geom_ribbon(data=filter(spr,modelo=='BVAR com fluxo'),aes(date,ymin=inferior80,ymax=superior80),fill='#2166AC',alpha=.13)+
  geom_line(data=hist,aes(date,nivel),colour='grey15',linewidth=.8)+
  geom_line(data=bind_rows(spr,sat),aes(date,media,colour=modelo,linetype=modelo),linewidth=1)+
  geom_text(data=fim,aes(date,media,label=round(media),colour=modelo),hjust=-.25,size=3.8,show.legend=FALSE)+
  scale_colour_manual(values=cores)+scale_linetype_manual(values=c(`BVAR atual`='dashed',`BVAR com fluxo`='solid',`Satelite final`='dotted'))+
  scale_x_date(date_breaks='1 year',date_labels='%Y',expand=expansion(mult=c(.01,.07)))+
  theme_minimal(base_size=12)+theme(legend.position='bottom',panel.grid.minor=element_blank())+
  labs(title='JGB 30Y-5Y (pb): BVAR atual x BVAR com fluxo estrangeiro (cenario Base)',
    subtitle='Fluxo estrangeiro (lag 2) so na equacao do spread, coeficiente fixo do satelite; fluxo futuro ~ UST30 a termo',
    x=NULL,y=NULL,colour=NULL,linetype=NULL,caption='Faixa: intervalo de 80% do BVAR com fluxo.')
ggsave(file.path(pasta_teste,'resultados/comparacao 30-5 BVAR com fluxo.png'),g,width=12,height=7,dpi=160,bg='white')
cat('COMPARACAO_CONCLUIDA\n')
