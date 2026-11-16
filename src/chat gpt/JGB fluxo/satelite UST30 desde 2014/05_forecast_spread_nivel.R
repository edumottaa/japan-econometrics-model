# Forecast do satelite ja estimado: nenhum BVAR ou CV e executado.
executar_forecast_spread_nivel <- function() {
  pasta <- file.path(pasta_testes,'resultados','spread nivel UST diferenciado')
  arq <- file.path(pasta,'resultados.rds')
  if(!file.exists(arq)) stop('Rode primeiro code_run_spread_nivel_ust_delta.R.')
  salvos <- readRDS(arq)
  aptos <- salvos$tabela |> filter(aprovado) |> arrange(AICc)
  if(!nrow(aptos)) stop('Nenhum satelite aprovado na triagem salva.')
  id <- aptos$id[1]; a <- salvos$ajustes[[id]]
  d <- ler_insumos()
  stopifnot(max(a$treino$date)==d$origem,
    identical(a$insumos$date,d$datas),a$unidades$alvo=='spread 30Y-5Y em nivel (pb)')
  # Resposta em NIVEL: nao acumula as previsoes, nem soma novamente o nivel inicial.
  previsao <- forecast(a$modelo,new_data=a$insumos)
  f <- as_tibble(previsao)
  limite <- function(p) vapply(seq_len(nrow(f)),function(i)
    as.numeric(unlist(quantile(f$d_spread[i],p=p))),numeric(1))
  futuro <- f |> transmute(date,spread_pb=.mean) |>
    mutate(p10=limite(.10),p90=limite(.90),p025=limite(.025),p975=limite(.975))
  stopifnot(all(is.finite(as.matrix(select(futuro,-date)))),
    all(futuro$p10<futuro$spread_pb & futuro$p90>futuro$spread_pb))
  historico <- d$historico |> transmute(date,spread_pb=100*inclinacao) |>
    filter(date>=yearmonth('2016 Jan'),date<=d$origem)
  # Medias mensais observadas apos o treino, usadas apenas para comparacao visual.
  mof <- read_csv(file.path(fonte_dados,'jgbcme_all_2026-11-04.csv'),skip=1,
    na=c('-',''),show_col_types=FALSE) |>
    transmute(dia=ymd(Date),spread_pb=100*(`30Y`-`5Y`)) |>
    filter(is.finite(spread_pb)) |> group_by(date=yearmonth(dia)) |>
    summarise(spread_pb=mean(spread_pb),.groups='drop') |>
    filter(date>d$origem,date<=yearmonth('2026 Oct'))
  # As observacoes de set/out nao sao ancoras nem entram no ajuste de ago/2026.
  observado <- bind_rows(historico,mof) |> mutate(date=as.Date(date))
  previsto <- bind_rows(tail(historico,1) |> mutate(p10=NA_real_,p90=NA_real_,
    p025=NA_real_,p975=NA_real_),futuro) |> mutate(date=as.Date(date))
  marcas <- futuro |> filter(date %in% yearmonth(c('2026 Dec','2027 Dec','2028 Sep'))) |>
    mutate(date=as.Date(date),label=paste0(round(spread_pb,1),' pb'))
  destino <- file.path(pasta,'forecast')
  dir.create(destino,recursive=TRUE,showWarnings=FALSE)
  g <- ggplot()+
    geom_ribbon(data=previsto,aes(date,ymin=p10,ymax=p90),fill='#b9d9ed',alpha=.65)+
    geom_line(data=observado,aes(date,spread_pb,colour='Realizado'),linewidth=.85)+
    geom_line(data=previsto,aes(date,spread_pb,colour='Satelite: projecao'),linewidth=1)+
    geom_point(data=marcas,aes(date,spread_pb),colour='#2563A6',size=2)+
    geom_label(data=marcas,aes(date,spread_pb,label=label),nudge_y=12,size=3.6,
      colour='#16476d',fill='white',linewidth=.2)+
    geom_vline(xintercept=as.Date(d$origem),linetype='dotted',colour='#777777')+
    scale_colour_manual(values=c('Realizado'='#171717','Satelite: projecao'='#2563A6'))+
    scale_x_date(date_breaks='2 years',date_labels='%Y',expand=expansion(mult=c(.01,.06)))+
    theme_minimal(base_size=13)+
    theme(legend.position='top',panel.grid.minor=element_blank(),
      plot.title=element_text(face='bold'),plot.caption=element_text(hjust=0,size=10))+
    labs(title='Japao | forecast do spread JGB30Y - JGB5Y',
      subtitle='Spread em nivel; Delta OIS2Y e Delta UST30 contemporaneos; erros AR(2); sem dummies',
      x=NULL,y='Pontos-base',colour=NULL,
      caption=paste0('Treino: fev/2014-ago/2026. Curvas Bloomberg FWCV de 30/10/2026; medias mensais aproximadas.\n',
        'Faixa de 80% condicional aos juros informados; nao inclui incerteza de parametros, selecao ou curvas.\n',
        'Linha pontilhada: fim do treino. Realizado de set/out apenas para comparacao; sem reancorar a projecao.\n',
        'Projecao do satelite isolado; nao e o BVAR condicionado.'))
  ggsave(file.path(destino,'forecast spread 30Y-5Y.png'),g,width=13,height=7,dpi=160,bg='white')
  gzoom <- g+coord_cartesian(xlim=as.Date(c('2024-01-01','2028-12-01')))+
    scale_x_date(date_breaks='1 year',date_labels='%Y',expand=expansion(mult=c(.01,.04)))
  ggsave(file.path(destino,'forecast spread 30Y-5Y recente.png'),gzoom,
    width=13,height=7,dpi=160,bg='white')
  write_csv(mutate(futuro,date=as.Date(date)),file.path(destino,'forecast spread.csv'))
  write_csv(mutate(a$insumos,date=as.Date(date)),file.path(destino,'regressores transformados.csv'))
  write_csv(mutate(d$futuro_aux,date=as.Date(date)),file.path(destino,'curvas em nivel.csv'))
  write_csv(aptos[1,],file.path(destino,'configuracao selecionada.csv'))
  write_csv(observado,file.path(destino,'realizado.csv'))
  saveRDS(list(id=id,modelo=a$modelo,forecast=previsao,tabela=futuro,
    premissas=d$futuro_aux,observado=observado),file.path(destino,'forecast.rds'))
  print(futuro |> filter(date %in% yearmonth(c('2026 Dec','2027 Dec','2028 Sep'))))
  print(tail(historico,1))
  invisible(futuro)
}
