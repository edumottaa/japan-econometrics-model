# Funcoes preservadas da configuracao selecionada. Source apenas define funcoes.

exportar_satelite <- function(sat,destino) {
  write_csv(sat$coeficientes,file.path(destino,'coeficientes satelite.csv'))
  write_csv(sat$diagnosticos,file.path(destino,'diagnosticos satelite.csv'))
  write_csv(sat$residuos |> mutate(date=as.Date(date)),file.path(destino,'residuos satelite.csv'))
  e <- sat$residuos |> transmute(date=as.Date(date),residuo=.innov)
  g1 <- ggplot(e,aes(date,residuo))+geom_hline(yintercept=0,colour='grey60')+
    geom_line()+geom_point(size=1)+theme_minimal()+
    labs(title='Satelite do spread: residuos',x=NULL,y='pb/mes',
      subtitle=paste('Ljung-Box:',paste0(sat$diagnosticos$lag,'m p=',
        round(sat$diagnosticos$p_ljung_box,3),collapse='; ')))
  ac <- stats::acf(e$residuo,lag.max=24,plot=FALSE)
  a <- tibble(lag=as.numeric(ac$lag)[-1],acf=as.numeric(ac$acf)[-1])
  g2 <- ggplot(a,aes(lag,acf))+geom_hline(yintercept=0)+geom_segment(aes(xend=lag,yend=0))+
    geom_hline(yintercept=c(-1,1)*1.96/sqrt(nrow(e)),linetype=2,colour='blue')+
    theme_minimal()+labs(title='ACF dos residuos do satelite',x='Defasagem',y='ACF')
  g3 <- ggplot(e,aes(residuo))+geom_histogram(bins=20,fill='grey50',colour='white')+
    theme_minimal()+labs(title='Distribuicao dos residuos',x='pb/mes',y='Frequencia')
  for(j in 1:3) ggsave(file.path(destino,paste0('residuos ',j,'.png')),
    list(g1,g2,g3)[[j]],width=10,height=5,dpi=150,bg='white')
  if(MOSTRAR_GRAFICOS) {print(g1); print(g2); print(g3)}
}

