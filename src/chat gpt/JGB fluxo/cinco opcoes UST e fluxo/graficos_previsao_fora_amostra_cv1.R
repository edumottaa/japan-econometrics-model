# SOMENTE GRAFICOS: le resultados salvos; nao estima nem chama forecast()/CV.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
source('src/chat gpt/JGB fluxo/cinco opcoes UST e fluxo/00_configuracao.R',encoding='UTF-8')
iniciar_testes()
source(file.path(pasta_testes,'cv_igual_bvar_original.R'),encoding='UTF-8')

origens_plot <- yearmonth(c('2021 Dec','2023 Dec','2025 Dec'))
destino <- file.path(raiz_cv_original,'opcao_1','previsoes fora da amostra')
dir.create(destino,recursive=TRUE,showWarnings=FALSE)
chave_atual <- chave_cv_original(chave_execucao())
modelos_plot <- c('1: UST30 exogeno','BVAR original ex post','Naive','Drift')
pratica <- map_dfr(seq_along(origens_plot),function(i) {
  arquivo <- file.path(raiz_cv_original,'opcao_1',
    paste0('origem_',format(as.Date(origens_plot[i]),'%Y-%m'),'.rds'))
  if(!file.exists(arquivo)) stop('Janela ainda nao salva: ',origens_plot[i])
  salvo <- readRDS(arquivo)
  if(!identical(salvo$chave,chave_atual))
    stop('Janela de outra configuracao. Conferir os dados antes de comparar: ',arquivo)
  salvo$valor |> filter(.model %in% modelos_plot) |>
    select(.id,.model,date,origem,h,variavel,.mean)
})
stopifnot(setequal(unique(pratica$variavel),domesticas_cv),all(pratica$h<=12L))
historico <- readRDS(file.path(fonte_modelo,'dados.rds')) |> as_tibble() |>
  mutate(date=yearmonth(date)) |> select(date,all_of(domesticas_cv)) |>
  pivot_longer(-date,names_to='variavel',values_to='valor')

# Mesma transformacao dos graficos do relatorio: atividade/CPI em 12 meses,
# cambio em ienes/USD. Spread apresentado em pb para facilitar a leitura.
transformar_plot <- function(x,alvo) {
  x <- arrange(x,date)
  if(alvo %in% c('atividade','cpi_core')) x$valor <- 100*expm1((x$valor-lag(x$valor,12))/100)
  else if(alvo=='usdjpy') x$valor <- exp(x$valor/100)
  else if(alvo=='inclinacao') x$valor <- 100*x$valor
  x
}
hist_plot <- historico |> group_by(variavel) |>
  group_modify(~transformar_plot(.x,.y$variavel)) |> ungroup() |>
  filter(date>=yearmonth('2020 Jan'))
pratica_plot <- pratica |> group_by(variavel,.id,.model) |>
  group_modify(function(x,k) {
    o <- first(x$origem)
    hist <- historico |> filter(variavel==k$variavel,date<=o) |> select(date,valor)
    bind_rows(hist,x |> transmute(date,valor=.mean)) |>
      transformar_plot(k$variavel) |> filter(date>o) |> mutate(origem=o)
  }) |> ungroup()
stopifnot(all(is.finite(pratica_plot$valor)))

cores <- c('Opcao 1: UST30 exogeno'='#2563A6','BVAR original'='#8563A0',
  Naive='#70836B',Drift='#999999',Observado='#171717')
nomes <- c(atividade='Atividade japonesa',cpi_core='Inflacao core',
  juro_curto='Juro curto do modelo',inclinacao='Spread JGB30Y - JGB5Y',usdjpy='USD/JPY')
unidades <- c(atividade='Variacao em 12 meses (%)',cpi_core='Inflacao em 12 meses (%)',
  juro_curto='% a.a.',inclinacao='Pontos-base',usdjpy='Ienes por dolar')
pratica_plot <- pratica_plot |> mutate(modelo=recode(.model,
  '1: UST30 exogeno'='Opcao 1: UST30 exogeno','BVAR original ex post'='BVAR original'))
for(alvo in domesticas_cv) {
  legenda <- paste0(
    'Linhas pontilhadas: inicio de cada previsao. CV ex post, com caminhos externos realizados e dados revisados.\n',
    'Trechos sem linha prevista: intervalos entre blocos de 12 meses. Ultimo bloco observado ate ago/2026.',
    if(alvo=='juro_curto') '\nSerie de juro do modelo: shadow rate ate fev/2024; call rate desde mar/2024.' else '')
  g <- ggplot()+
    geom_line(data=filter(hist_plot,variavel==alvo),
      aes(as.Date(date),valor,colour='Observado'),linewidth=.9)+
    geom_line(data=filter(pratica_plot,variavel==alvo),
      aes(as.Date(date),valor,colour=modelo,group=interaction(.id,modelo),
        linetype=modelo),linewidth=.8)+
    geom_vline(xintercept=as.Date(origens_plot+1L),linetype='dotted',colour='#999999')+
    scale_colour_manual(values=cores,breaks=names(cores))+
    scale_linetype_manual(values=c('Opcao 1: UST30 exogeno'='solid',
      'BVAR original'='solid',Naive='solid',Drift='longdash'),guide='none')+
    scale_x_date(limits=c(as.Date(yearmonth('2020 Jan')),as.Date(max(historico$date))),
      date_breaks='1 year',date_labels='%Y')+
    theme_minimal(base_size=13)+theme(legend.position='bottom',
      panel.grid.minor=element_blank(),plot.title=element_text(face='bold'),
      plot.caption=element_text(hjust=0,size=10))+
    labs(title=paste(nomes[alvo],'| previsoes fora da amostra'),
      subtitle='Origens: dez/2021, dez/2023 e dez/2025 | Ate 12 meses por origem',
      x=NULL,y=unidades[alvo],colour=NULL,
      caption=legenda)
  ggsave(file.path(destino,paste0('previsao fora da amostra ',alvo,'.png')),
    g,width=12,height=4.9,dpi=160,bg='white')
  if(MOSTRAR_GRAFICOS) print(g)
}
write_csv(pratica_plot |> mutate(date=as.Date(date),origem=as.Date(origem)),
  file.path(destino,'series previstas para os graficos.csv'))
write_csv(hist_plot |> mutate(date=as.Date(date)),file.path(destino,'series observadas para os graficos.csv'))
message('Cinco graficos gerados a partir do CV 1 salvo. Nenhuma estimacao ou CV executado.')
