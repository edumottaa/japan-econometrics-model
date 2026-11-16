# Forecast original salvo contra o adicional do fluxo, sem nova estimacao.
cores_fixas <- c('Original salvo'='#424B54','Cenario adicional de fluxo'='#B45B42')
spread_fixo <- comparacao_fixa |> filter(cenario=='Base',variavel=='inclinacao') |>
  mutate(spread_pb=100*valor)
rotulos <- spread_fixo |> group_by(caminho) |> slice_max(mes,n=1) |> ungroup() |>
  mutate(rotulo=paste0(formatC(spread_pb,format='f',digits=1,decimal.mark=','),' pb'))
g <- ggplot(spread_fixo,aes(as.Date(mes),spread_pb,colour=caminho)) +
  geom_line(linewidth=1.1) +
  geom_text(data=rotulos,aes(label=rotulo),hjust=-.12,size=3.8,show.legend=FALSE) +
  scale_colour_manual(values=cores_fixas) +
  scale_x_date(date_breaks='6 months',date_labels='%m/%Y',expand=expansion(mult=c(.01,.15))) +
  theme_minimal(base_size=12) + theme(legend.position='top',panel.grid.minor=element_blank()) +
  labs(title='JGB30Y-5Y: somente o forecast muda',
       subtitle='BVAR original preservado | adicional do fluxo estimado no satelite',
       x=NULL,y='pontos-base',colour=NULL,
       caption='Referencia de fluxo: media historica. Ajuste incremental a partir de nov/2026.\nCenario de sensibilidade; nao e um BVAR-X reestimado nem uma medida de risco fiscal.')
ggsave(file.path(saida_fixa,'spread Base.png'),g,width=11,height=5,dpi=170,bg='white')
titulos <- c(ust2y='UST2Y (%)',brent='Brent (USD)',ip_mundial='IP mundial (indice)',
             atividade='Atividade (% 12m)',cpi_core='CPI core (% 12m)',
             juro_curto='Juro curto (%)',inclinacao='30Y-5Y (%)',usdjpy='USD/JPY')
g_todas <- comparacao_fixa |> filter(cenario=='Base') |>
  ggplot(aes(as.Date(mes),valor_plot,colour=caminho)) + geom_line(linewidth=.8) +
  facet_wrap(vars(variavel),ncol=2,scales='free_y',labeller=as_labeller(titulos)) +
  scale_colour_manual(values=cores_fixas) + theme_minimal(base_size=11) +
  theme(legend.position='top',panel.grid.minor=element_blank()) +
  labs(title='Efeito do fluxo no forecast: coeficientes originais fixos',x=NULL,y=NULL,colour=NULL,
       caption='Propagacao pelas defasagens originais. Covariancias, priors e parametros do BVAR preservados.\nCentros de cenario: sem novas bandas preditivas ou novo condicionamento terminal do BoJ.')
ggsave(file.path(saida_fixa,'impactos Base.png'),g_todas,width=11,height=11,dpi=160,bg='white')
