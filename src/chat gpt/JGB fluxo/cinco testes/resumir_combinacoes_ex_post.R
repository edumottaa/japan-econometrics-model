# Apresentacao dos resultados; inclui referencia original fixa na mesma amostra.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco testes/00_configuracao.R',encoding='UTF-8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(fpp3))
suppressPackageStartupMessages(library(tidyverse))
source(file.path(pasta_testes,'09_selecao_satelite.R'),encoding='UTF-8')
source(file.path(pasta_testes,'01_dados.R'),encoding='UTF-8')
destino <- file.path(pasta_testes,'resultados','combinacoes ex post')
modelos <- readRDS(file.path(destino,'modelos.rds'))
cv <- read_csv(file.path(destino,'CV todos.csv'),show_col_types=FALSE)
referencia <- file.path(destino,'referencia original fixa CV.csv')
if(!file.exists(referencia)) {
  d <- ler_insumos()
  datas <- augment(modelos$ARIMA_base$modelo)$date
  b <- d$historico |> as_tibble() |> arrange(date) |>
    transmute(date,spread=100*inclinacao,d_spread=100*(inclinacao-lag(inclinacao))) |>
    left_join(d$aux |> select(date,ois_pos,fluxo_l2),by='date') |>
    filter(date %in% datas) |> as_tsibble(index=date)
  origens <- seq(yearmonth('2024 Oct'),max(b$date)-12L,by=1L)
  prev <- map_dfr(origens,function(o) {
    t <- b |> filter(date<=o); j <- b |> filter(date>o,date<=o+12L)
    m <- lm(d_spread~ois_pos+fluxo_l2,data=t)
    tibble(previsto=tail(t$spread,1)+sum(predict(m,newdata=as_tibble(j))),
      observado=tail(j$spread,1),sig=all(summary(m)$coefficients[c('ois_pos','fluxo_l2'),4]<=.1))
  })
  m0 <- b |> model(arima=ARIMA(d_spread~1+ois_pos+fluxo_l2+pdq(0,0,0)+PDQ(0,0,0)))
  write_csv(tibble(modelo='ARIMA_original',origens=nrow(prev),
    rmse_pb=sqrt(mean((prev$previsto-prev$observado)^2)),mae_pb=mean(abs(prev$previsto-prev$observado)),
    fracao_treinos_significativos=mean(prev$sig),
    aprovado=all(tidy(m0)$p.value[tidy(m0)$term %in% c('ois_pos','fluxo_l2')]<=.1),
    reprovadas=NA_character_),referencia)
  saveRDS(m0,file.path(destino,'referencia original fixa.rds'))
}
if(file.exists(referencia)) {
  cv <- bind_rows(cv,read_csv(referencia,show_col_types=FALSE)) |> arrange(rmse_pb)
  modelos$ARIMA_original <- list(modelo=readRDS(file.path(destino,'referencia original fixa.rds')))
}
nomes <- c(ARIMA_original='Referencia: OIS + fluxo, erro branco',ARIMA_base='ARIMA: OIS + fluxo, ordem selecionada',ARIMA_C1='ARIMA: + diferencial curto',
  ARIMA_C2='ARIMA: + cambio',VAR_base='VAR: fluxo e spread + OIS',
  VAR_C2='VAR: + cambio',Naive='Naive',Drift='Drift')
finais <- cv |> filter(aprovado %in% TRUE | modelo %in% c('Naive','Drift')) |>
  mutate(nome=coalesce(unname(nomes[modelo]),modelo))
write_csv(finais,file.path(destino,'tabela final.csv'))
g <- ggplot(finais,aes(reorder(nome,rmse_pb),rmse_pb,fill=startsWith(modelo,'VAR')))+
  geom_col(width=.7)+geom_text(aes(label=formatC(rmse_pb,format='f',digits=1,decimal.mark=',')),hjust=-.2)+
  coord_flip()+scale_y_continuous(expand=expansion(mult=c(0,.15)))+
  scale_fill_manual(values=c('#315B7A','#BD6748'),guide='none')+
  theme_minimal(base_size=12)+labs(title='CV do spread JGB30Y-5Y: erro em 12 meses',
    subtitle='11 origens comuns | Macros realizadas | Corte nominal: todas as variaveis economicas com p <= 10%',
    x=NULL,y='RMSE (pb; menor e melhor)',
    caption='Azul: ARIMA com fluxo realizado e benchmarks. Laranja: VAR com fluxo previsto. Informacoes diferentes; triagem exploratoria.')
ggsave(file.path(destino,'CV significativos.png'),g,width=12,height=6,dpi=150,bg='white')

grDevices::pdf(tempfile(fileext='.pdf'))
paineis_ar <- list(); paineis_var <- list()
datas <- augment(modelos$ARIMA_base$modelo)$date
for(id in finais$modelo[startsWith(finais$modelo,'ARIMA')]) {
  a <- modelos[[id]]
  p <- grafico_residuos_satelite(a$modelo,'spread')
  p[[1]] <- p[[1]]+labs(title=unname(nomes[id]))+theme(plot.title=element_text(size=11),plot.subtitle=element_text(size=8))
  paineis_ar[[id]] <- p
}
for(id in finais$modelo[startsWith(finais$modelo,'VAR')]) for(j in 1:2) {
  e <- residuals(modelos[[id]]$modelo)[,j]
  eq <- c('fluxo','spread')[j]
  stopifnot(length(e)==length(datas))
  p <- ggtime::gg_tsdisplay(tibble(date=datas,erro=e) |> as_tsibble(index=date),erro,
    plot_type='histogram',lag_max=24)
  pv <- sapply(c(12,24),function(k) Box.test(e,lag=k,type='Ljung-Box')$p.value)
  p[[1]] <- p[[1]]+labs(title=paste(unname(nomes[id]),'|',eq),
    subtitle=paste('Ljung-Box marginal 12m/24m:',paste(ifelse(pv<.001,'<0,001',
      formatC(pv,format='f',digits=3,decimal.mark=',')),collapse=' / ')),
    y=if(j==1) 'Tri de ienes/mes' else 'pb/mes',x=NULL)+
    theme(plot.title=element_text(size=11),plot.subtitle=element_text(size=8))
  paineis_var[[paste(id,eq)]] <- p
}
invisible(dev.off())
salvar_paineis <- function(paineis,arquivo) {
  if(!length(paineis)) return(invisible(NULL))
  grDevices::png(arquivo,width=16,height=3.6*length(paineis),units='in',res=140)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout=grid::grid.layout(length(paineis),3)))
  for(i in seq_along(paineis)) for(j in 1:3)
    print(paineis[[i]][[j]],vp=grid::viewport(layout.pos.row=i,layout.pos.col=j))
  invisible(dev.off())
}
salvar_paineis(paineis_ar,file.path(destino,'residuos ARIMA significativos.png'))
salvar_paineis(paineis_var,file.path(destino,'residuos VAR significativos.png'))
print(finais,width=Inf,n=Inf)
