# Apenas grafico: reutiliza o CV isolado salvo; nao estima modelos nem executa CV.
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages({library(fpp3);library(tidyverse)})
raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
pasta <- file.path(raiz,'src/chat gpt/JGB fluxo/resultados/teste inflacao cambio')
cv <- read_csv(file.path(pasta,'CV ex post previsoes.csv'),show_col_types=FALSE) |>
  filter(.model %in% c('original','Naive','Drift')) |>
  mutate(origem=yearmonth(origem),data_alvo=origem+12L)
amostra <- read_csv(file.path(pasta,'amostra.csv'),show_col_types=FALSE) |>
  transmute(mes=yearmonth(mes),spread)
dados <- cv |>
  left_join(rename(amostra,origem=mes,spread_origem=spread),by='origem') |>
  left_join(rename(amostra,data_alvo=mes,spread_realizado=spread),by='data_alvo') |>
  mutate(spread_previsto=spread_origem+previsto,
    modelo=recode(.model,original='Satelite com fluxo (ex post)',Naive='Naive',Drift='Drift'))
stopifnot(n_distinct(dados$origem)==11L,nrow(dados)==33L,
  all(is.finite(dados$spread_previsto)),all(is.finite(dados$spread_realizado)),
  max(abs(dados$spread_realizado-dados$spread_origem-dados$observado))<1e-8)
metricas <- dados |> group_by(modelo) |>
  summarise(RMSE=sqrt(mean((spread_previsto-spread_realizado)^2)),
    MAE=mean(abs(spread_previsto-spread_realizado)),.groups='drop')
observado <- distinct(dados,data_alvo,spread_realizado)
cores <- c('Satelite com fluxo (ex post)'='#A04491','Naive'='#999999',
  'Drift'='#B39515','Realizado'='#171717')
legendas <- metricas |> mutate(texto=paste0(modelo,': RMSE ',
  formatC(RMSE,format='f',digits=2,decimal.mark=','),' pb')) |> pull(texto)
g <- ggplot()+
  geom_line(data=dados,aes(as.Date(data_alvo),spread_previsto,colour=modelo,
    linetype=modelo),linewidth=.85)+
  geom_point(data=filter(dados,.model=='original'),
    aes(as.Date(data_alvo),spread_previsto,colour=modelo),size=2)+
  geom_line(data=observado,aes(as.Date(data_alvo),spread_realizado,colour='Realizado'),linewidth=1)+
  geom_point(data=observado,aes(as.Date(data_alvo),spread_realizado,colour='Realizado'),size=2)+
  scale_colour_manual(values=cores,breaks=names(cores))+
  scale_linetype_manual(values=c('Satelite com fluxo (ex post)'='solid',
    Naive='dotted',Drift='longdash'),guide='none')+
  scale_x_date(date_breaks='2 months',date_labels='%m/%Y',expand=expansion(mult=c(.03,.03)))+
  theme_minimal(base_size=13)+theme(legend.position='top',panel.grid.minor=element_blank(),
    plot.title=element_text(face='bold'),plot.caption=element_text(hjust=0,size=10))+
  labs(title='CV do satelite com fluxo | spread JGB30Y - JGB5Y',
    subtitle='Previsao feita 12 meses antes x realizado | 11 origens mensais: out/2024-ago/2025',
    x='Data prevista (12 meses apos cada origem)',y='Spread 30Y - 5Y | pontos-base',colour=NULL,
    caption=paste0(paste(legendas,collapse=' | '),
      '\nEx post: OIS e fluxo futuros realizados. Avalia a equacao do spread, nao a capacidade de prever o fluxo.',
      '\nCada ponto corresponde a uma origem diferente; a linha nao e a trajetoria de uma unica previsao.',
      '\nJanelas sobrepostas. Resultados salvos; nenhum CV reexecutado.'))
destino <- file.path(pasta,'graficos CV isolado')
dir.create(destino,recursive=TRUE,showWarnings=FALSE)
ggsave(file.path(destino,'CV satelite previsto x realizado 12 meses.png'),g,
  width=13,height=7,dpi=160,bg='white')
write_csv(mutate(dados,origem=as.Date(origem),data_alvo=as.Date(data_alvo)),
  file.path(destino,'valores do grafico.csv'))
write_csv(metricas,file.path(destino,'metricas.csv'))
print(metricas)
