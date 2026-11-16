# 1. Insumos comuns, conservando os quatro cenarios e a banda do forecast atual.
library(bsvarSIGNs)
original <- readRDS(file.path(fonte_modelo,'modelo.rds'))
dados_boj <- readRDS(file.path(fonte_modelo,'dados.rds'))
y <- dados_boj |> as_tibble() |> select(all_of(variaveis)) |> as.matrix()
origem_bvar <- max(yearmonth(dados_boj$date))
condicoes <- readRDS(file.path(fonte_forecast,'caminhos_novos.rds')) |>
  as_tibble() |> mutate(date=yearmonth(date))
config <- readRDS(file.path(fonte_forecast,'forecast_bvar.rds'))
banda <- config$banda_juro
stopifnot(length(banda)==2L)
datas <- sort(unique(condicoes$date))
stopifnot(identical(datas,seq(origem_bvar+1L,max(datas),by=1L)))
fit_sat <- readRDS(file.path(saida,'satelite.rds'))
satelite <- fit_sat$satelite
source(file.path(pasta,'funcoes_forecast.R'),encoding='UTF-8')

# 2. Efeito do modelo 1: impor o spread do satelite no BVAR original.
# Isso e um cenario condicionado: nao identifica um choque fiscal ou de demanda.
# Comparacao controlada: mesma regra de condicionamento em TODAS as variantes.
# O condicionamento e conjunto em toda a trajetoria, nao o algoritmo sequencial
# do forecast salvo. Exportamos tambem esse forecast salvo como referencia separada.
if(comparar_bvar_fluxo) {
  piloto_comp <- isTRUE(get0('permitir_piloto',ifnotfound=FALSE))
  arq <- file.path(saida,if(piloto_comp) 'bvar fluxo piloto.rds' else 'bvar fluxo.rds')
  if(!file.exists(arq)) stop('Estime primeiro o BVAR-X completo; piloto nao e resultado final.')
  bvar_x <- readRDS(arq)
  if(!piloto_comp) stopifnot(bvar_x$modelo$posterior$ess>=50)
  fluxo_x <- fit_sat$fluxo_completo |>
    mutate(fluxo_l2=lag(fluxo,2),x=fluxo_l2-bvar_x$media_fluxo)
}
saida_comp <- if(comparar_bvar_fluxo && piloto_comp) file.path(saida,'comparacao piloto') else saida
dir.create(saida_comp,recursive=TRUE,showWarnings=FALSE)
nota_piloto <- if(comparar_bvar_fluxo && piloto_comp)
  paste0('PILOTO: ',dim(bvar_x$modelo$posterior$A)[3],' sorteios; ESS ',
         round(bvar_x$modelo$posterior$ess,1),'. Nao e estimacao final.') else ''
resultados <- list()
for(cen in unique(condicoes$cenario)) {
  cenario <- condicoes |> filter(cenario==cen) |> arrange(date)
  cond <- cenario |> select(all_of(variaveis)) |> as.matrix()
  sat <- satelite$spread[match(datas,satelite$mes)]
  variantes <- list(Original=list(modelo=original,x=NULL,sat=NULL),
                   `Satelite condicionado`=list(modelo=original,x=NULL,sat=sat))
  if(comparar_bvar_fluxo) {
    xfut <- fluxo_x$x[match(datas,fluxo_x$mes)]
    stopifnot(all(is.finite(xfut)))
    variantes[['BVAR com fluxo']] <- list(modelo=bvar_x$modelo,x=xfut,sat=NULL)
    variantes[['BVAR com fluxo x=0']] <- list(modelo=bvar_x$modelo,x=rep(0,length(datas)),sat=NULL)
  }
  for(nome in names(variantes)) {
    v <- variantes[[nome]]
    tabela <- projetar_bvar(v$modelo,y,cond,xfuturo=v$x,spread_fixo=v$sat,banda=banda)
    resultados[[paste(cen,nome)]] <- tabela |> mutate(mes=datas,cenario=cen,modelo=nome)
    message('Forecast concluido: ',cen,' | ',nome)
  }
}
comparacao <- bind_rows(resultados) |>
  pivot_longer(all_of(variaveis),names_to='variavel',values_to='valor')

# 3. Atividade/CPI em variacao 12m; demais variaveis em suas unidades usuais.
observados <- dados_boj |> as_tibble() |> mutate(mes=yearmonth(date)) |>
  select(mes,all_of(variaveis)) |>
  mutate(across(c(brent,ip_mundial,atividade,cpi_core,usdjpy),~exp(.x/100))) |>
  pivot_longer(-mes,names_to='variavel',values_to='valor')
anualizar <- function(x) {
  completo <- bind_rows(observados |> select(mes,variavel,valor),x |> select(mes,variavel,valor)) |>
    arrange(variavel,mes) |> group_by(variavel) |>
    mutate(valor_plot=if_else(variavel %in% c('atividade','cpi_core'),100*(valor/lag(valor,12)-1),valor)) |>
    ungroup() |> filter(mes %in% datas) |> select(mes,variavel,valor_plot)
  left_join(x,completo,by=c('mes','variavel'))
}
comparacao <- comparacao |> group_by(cenario,modelo) |> group_modify(~anualizar(.x)) |> ungroup()
referencia <- comparacao |> filter(modelo=='Original') |>
  select(cenario,mes,variavel,referencia=valor,referencia_plot=valor_plot)
comparacao <- comparacao |> left_join(referencia,by=c('cenario','mes','variavel')) |>
  mutate(efeito=if_else(variavel %in% c('ust2y','juro_curto','inclinacao'),100*(valor-referencia),
                       100*(valor/referencia-1)),
         unidade_efeito=if_else(variavel %in% c('ust2y','juro_curto','inclinacao'),'pb','% do nivel'),
         efeito_anual_pp=if_else(variavel %in% c('atividade','cpi_core'),valor_plot-referencia_plot,NA_real_))
write_csv(comparacao |> mutate(mes=as.Date(mes)),file.path(saida_comp,'impactos no BVAR.csv'))
write_csv(comparacao |> filter(mes==max(mes)) |> mutate(mes=as.Date(mes)),
          file.path(saida_comp,'impactos finais.csv'))
write_csv(config$resumo |> mutate(date=as.Date(date)),file.path(saida_comp,'BVAR salvo referencia.csv'))

# Separar efeito da reestimacao e efeito exclusivo da trajetoria futura do fluxo.
# Todos os efeitos de nivel usam o denominador Original, garantindo aditividade.
if(comparar_bvar_fluxo) {
  decomp <- comparacao |> select(cenario,mes,variavel,modelo,valor,valor_plot) |>
    pivot_wider(names_from=modelo,values_from=c(valor,valor_plot)) |>
    mutate(fator=if_else(variavel %in% c('ust2y','juro_curto','inclinacao'),100,100/valor_Original),
      reestimacao=fator*(`valor_BVAR com fluxo x=0`-valor_Original),
      trajetoria=fator*(`valor_BVAR com fluxo`-`valor_BVAR com fluxo x=0`),
      total=fator*(`valor_BVAR com fluxo`-valor_Original))
  stopifnot(max(abs(decomp$total-decomp$reestimacao-decomp$trajetoria))<1e-8)
  write_csv(decomp |> mutate(mes=as.Date(mes)),file.path(saida_comp,'decomposicao efeito fluxo.csv'))
}

# 4. Graficos: os dois caminhos e os efeitos sobre TODAS as oito variaveis.
cores <- c(Original='grey35',`Satelite condicionado`='#2166AC',
           `BVAR com fluxo`='#B35C36',`BVAR com fluxo x=0`='#79937B')
titulos <- c(ust2y='UST2Y (%)',brent='Brent (USD)',ip_mundial='IP mundial (indice)',
             atividade='Atividade (% 12m)',cpi_core='CPI core (% 12m)',
             juro_curto='Juro curto (%)',inclinacao='30Y-5Y (%)',usdjpy='USD/JPY')
g <- comparacao |> filter(modelo!='BVAR com fluxo x=0') |>
  ggplot(aes(as.Date(mes),valor_plot,colour=modelo)) + geom_line(linewidth=.75) +
  facet_grid(vars(variavel),vars(cenario),scales='free_y',labeller=labeller(variavel=titulos)) +
  scale_colour_manual(values=cores) + theme_minimal(base_size=10) +
  theme(legend.position='top',panel.grid.minor=element_blank()) +
  labs(title='Impacto dos dois modelos sobre o BVAR',subtitle=nota_piloto,x=NULL,y=NULL,colour=NULL,
       caption='Centros condicionais, banda terminal e insumos comuns. Satelite: spread imposto; BVAR-X: efeito estimado. Sem bandas preditivas.')
ggsave(file.path(saida_comp,'impactos todos cenarios.png'),g,width=15,height=16,dpi=150,bg='white')
base_plot <- comparacao |> filter(cenario=='Base',modelo!='BVAR com fluxo x=0')
g_base <- ggplot(base_plot,aes(as.Date(mes),valor_plot,colour=modelo)) + geom_line(linewidth=.85) +
  facet_wrap(vars(variavel),ncol=2,scales='free_y',labeller=as_labeller(titulos)) +
  scale_colour_manual(values=cores) + theme_minimal(base_size=11) + theme(legend.position='top') +
  labs(title='BVAR Base: comparacao dos dois caminhos',subtitle=nota_piloto,x=NULL,y=NULL,colour=NULL)
ggsave(file.path(saida_comp,'impactos Base.png'),g_base,width=11,height=11,dpi=160,bg='white')
spread_plot <- base_plot |> filter(variavel=='inclinacao') |> mutate(spread_pb=100*valor)
rotulos <- spread_plot |> group_by(modelo) |> slice_max(mes,n=1) |> ungroup() |>
  mutate(rotulo=paste0(formatC(spread_pb,format='f',digits=1,decimal.mark=','),' pb'))
g_spread <- ggplot(spread_plot,aes(as.Date(mes),spread_pb,colour=modelo)) +
  geom_line(linewidth=1) +
  geom_text(data=rotulos,aes(label=rotulo),hjust=-.12,size=3.5,show.legend=FALSE) +
  scale_colour_manual(values=cores) +
  scale_x_date(date_breaks='6 months',date_labels='%m/%Y',expand=expansion(mult=c(.01,.15))) +
  theme_minimal(base_size=12) + theme(legend.position='top',panel.grid.minor=element_blank()) +
  labs(title='JGB30Y-5Y: os dois caminhos contra o BVAR original',subtitle=nota_piloto,
       x=NULL,y='pontos-base',colour=NULL,
       caption='Comparacao Base com condicionamento comum. Original: controle recalculado; nao e o forecast salvo anteriormente.')
ggsave(file.path(saida_comp,'comparacao spread Base.png'),g_spread,width=11,height=5,dpi=170,bg='white')
g_sat <- ggplot(satelite,aes(as.Date(mes),spread)) +
  geom_ribbon(aes(ymin=p10,ymax=p90),fill='#2166AC',alpha=.15) +
  geom_line(colour='#2166AC',linewidth=1) + theme_minimal(base_size=12) +
  labs(title='Satelite: spread JGB30Y-5Y',x=NULL,y='pb',
       caption='Faixa p10-p90 condicional apenas aos erros do estagio 2. Curvas FWCV de 30/10/2026.')
ggsave(file.path(saida_comp,'forecast satelite.png'),g_sat,width=10,height=5,dpi=160,bg='white')
print(comparacao |> filter(cenario=='Base',mes==max(mes),
                          variavel %in% c('inclinacao','juro_curto','cpi_core','usdjpy')),
      n=Inf,width=Inf)
