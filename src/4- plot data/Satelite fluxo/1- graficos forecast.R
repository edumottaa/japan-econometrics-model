# Graficos do forecast: BVAR original x BVAR + satelite (tilting), por cenario
domesticas <- c('atividade','cpi_core','juro_curto','inclinacao','usdjpy')
destino <- file.path(saida,'graficos forecast')
dir.create(destino,recursive=TRUE,showWarnings=FALSE)
historico <- historico_satelite |> select(date,all_of(domesticas)) |>
  mutate(across(c(atividade,cpi_core,usdjpy),~exp(.x/100))) |>
  pivot_longer(-date,names_to='variavel',values_to='valor')
tab <- tabela |> filter(variavel %in% domesticas) |>
  group_by(modelo,cenario) |> group_modify(function(x,k) {
    bind_rows(historico,select(x,date,variavel,valor)) |> arrange(variavel,date) |>
      group_by(variavel) |> mutate(valor=case_when(
        variavel %in% c('atividade','cpi_core')~100*(valor/lag(valor,12)-1),
        variavel=='inclinacao'~100*valor,TRUE~valor)) |> ungroup() |> filter(date %in% x$date)
  }) |> ungroup() |> mutate(date=as.Date(date))
nomes <- c(atividade='Atividade | 12 meses (%)',cpi_core='Inflacao | 12 meses (%)',
  juro_curto='Juro curto | % a.a.',inclinacao='JGB30Y - JGB5Y | pb',usdjpy='USD/JPY')
for(cen in unique(tab$cenario)) {
  a <- filter(tab,cenario==cen)
  labels <- a |> group_by(modelo,variavel) |> slice_max(date,n=1,with_ties=FALSE)
  g <- ggplot(a,aes(date,valor,colour=modelo))+geom_line(linewidth=.8)+
    geom_text(data=labels,aes(label=round(valor,2)),hjust=-.08,size=3,show.legend=FALSE)+
    facet_wrap(vars(variavel),scales='free_y',ncol=2,labeller=as_labeller(nomes))+
    scale_x_date(date_labels='%m/%Y',expand=expansion(mult=c(.02,.18)))+
    theme_minimal()+theme(legend.position='bottom')+
    labs(title=paste('BVAR com satelite de fluxo (tilting) |',cen),x=NULL,y=NULL,colour=NULL,
      caption='Medias ponderadas dos sorteios do BVAR. Fluxo futuro = media pos-YCC. Juro terminal entre 1,75% e 2,25%.')
  ggsave(file.path(destino,paste0('forecast ',gsub('[^[:alnum:]]','_',cen),'.png')),
    g,width=13,height=10,dpi=150,bg='white')
}
