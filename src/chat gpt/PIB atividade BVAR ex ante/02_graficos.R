# Graficos das mesmas observacoes de teste, separados por cobertura.
gerar_graficos_pib <- function(metricas,previsoes,pasta,nome) {
  cores <- c('PIB | BVAR original'='#8563A0','PIB | opcao 1: UST30'='#2563A6',
    'PIB | opcao 2: UST30-UST2Y'='#A3A500','PIB | opcao 3: satelite suave'='#00A779',
    'PIB | opcao 4: satelite rigido'='#00B7E5',Naive='#70836B',Drift='#999999')
  cobertura <- paste('Datas previstas:',min(previsoes$date),'a',max(previsoes$date),
    '|',n_distinct(previsoes$origem),'origens comuns')
  gerar_barras <- function(tabela,por_periodo=FALSE) {
    dados <- tabela |> select(any_of(c('periodo','.model','RMSE','MAE','CRPS'))) |>
      pivot_longer(c(RMSE,MAE,CRPS),names_to='metrica',values_to='erro') |>
      mutate(.model=factor(.model,levels=rev(names(cores))))
    g <- ggplot(dados,aes(erro,.model,fill=.model))+geom_col(width=.6)+
      geom_text(aes(label=formatC(erro,format='f',digits=2,decimal.mark=',')),hjust=-.1,size=3)+
      scale_fill_manual(values=cores,guide='none')+
      scale_x_continuous(expand=expansion(mult=c(0,.2)))+
      theme_minimal()+theme(panel.grid.major.y=element_blank(),panel.grid.minor=element_blank())+
      labs(title=paste('CV do PIB | atividade prevista |',nome),subtitle=cobertura,
        x='Erro em pontos percentuais do crescimento trimestral',y=NULL,
        caption='Atividade prevista pelo BVAR; externos realizados. Ponte reestimada por origem. CRPS condicional a trajetoria media de atividade.')
    if(por_periodo) g+facet_grid(periodo~metrica,scales='free_x') else
      g+facet_wrap(vars(metrica),scales='free_x',ncol=3)
  }
  g1 <- gerar_barras(metricas$geral)
  g2 <- gerar_barras(metricas$periodos,TRUE)
  g3 <- ggplot(metricas$horizontes,aes(h,RMSE,colour=.model))+geom_line()+geom_point()+
    scale_colour_manual(values=cores)+scale_x_continuous(breaks=1:4)+theme_minimal()+
    theme(legend.position='bottom')+
    labs(title=paste('CV do PIB | erro por horizonte |',nome),subtitle=cobertura,
      x='Trimestres depois da origem',y='RMSE (p.p.)',colour=NULL)
  ggsave(file.path(pasta,'CV PIB geral.png'),g1,width=16,height=7,dpi=160,bg='white')
  ggsave(file.path(pasta,'CV PIB por periodo.png'),g2,width=16,
    height=max(6,3.4*n_distinct(previsoes$periodo)),dpi=160,bg='white')
  ggsave(file.path(pasta,'CV PIB por horizonte.png'),g3,width=12,height=7,dpi=160,bg='white')
  # Previsoes fora da amostra contra o PIB observado, usando somente pontos salvos.
  origens <- if(nome=='cinco BVARs') yearquarter(c('2024 Q2','2025 Q2')) else
    yearquarter(c('2021 Q4','2023 Q4','2025 Q4'))
  pratica <- previsoes |> filter(origem %in% origens)
  observado <- readRDS(arquivo_pib) |> as_tibble() |>
    mutate(crescimento=100*(pib/lag(pib)-1)) |> filter(date>=min(origens)-4L)
  g4 <- ggplot()+geom_line(data=observado,
      aes(as.Date(date),crescimento,colour='Observado'),linewidth=.9)+
    geom_line(data=pratica,aes(as.Date(date),.mean,colour=.model,
      group=interaction(.id,.model)),linewidth=.75)+
    geom_vline(xintercept=as.Date(origens+1L),linetype='dotted',colour='grey60')+
    scale_colour_manual(values=c(cores,Observado='#171717'))+
    scale_x_date(date_breaks='1 year',date_labels='%Y')+theme_minimal()+
    theme(legend.position='bottom',panel.grid.minor=element_blank())+
    guides(colour=guide_legend(nrow=3))+
    labs(title=paste('PIB | previsoes contra observado |',nome),
      subtitle=paste('Origens:',paste(as.character(origens),collapse=', '),'| ate 4 trimestres por bloco'),
      x=NULL,y='Crescimento trimestral (% t/t)',colour=NULL,
      caption='Atividade futura prevista pelo BVAR. Condicionamento externo ex post. Linhas pontilhadas iniciam os blocos.\nO ultimo bloco pode ter horizonte observado menor; nao utiliza atividade futura realizada.')
  ggsave(file.path(pasta,'PIB previsoes fora da amostra.png'),g4,width=14,height=7,dpi=160,bg='white')
  if(MOSTRAR_GRAFICOS_PIB) {print(g1);print(g2);print(g3);print(g4)}
}
