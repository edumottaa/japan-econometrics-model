# Triagem de previsibilidade condicional. Nao reestima o BVAR estrutural.
# Testa se defasagens do fluxo ajudam as variacoes domesticas e o caminho inverso.
source('src/chat gpt/JGB fluxo/00_dados.R',encoding='UTF-8')
saida_teste <- file.path(saida,'teste direcoes fluxo')
dir.create(saida_teste,recursive=TRUE,showWarnings=FALSE)

# Diferencas mensais das oito variaveis do BVAR; fluxo mensal permanece em nivel.
# CPI: inflacao mensal em log; FX positivo: depreciacao mensal do iene.
# Nao e uma decomposicao de premio de prazo nem teste de causalidade estrutural.
macro <- readRDS(file.path(fonte_modelo,'dados.rds')) |> as_tibble() |>
  arrange(date) |>
  transmute(mes=yearmonth(date),
    d_ust2=ust2y-lag(ust2y),d_brent=brent-lag(brent),
    d_ip=ip_mundial-lag(ip_mundial),
    d_atividade=atividade-lag(atividade),inflacao=cpi_core-lag(cpi_core),
    d_juro=juro_curto-lag(juro_curto),d_spread=100*(inclinacao-lag(inclinacao)),
    d_cambio=usdjpy-lag(usdjpy))
series <- c('d_ust2','d_brent','d_ip','d_atividade','inflacao',
            'd_juro','d_spread','d_cambio','fluxo')
domesticas <- c('d_atividade','inflacao','d_juro','d_spread','d_cambio')
amostra <- macro |> left_join(jsda,by='mes') |>
  left_join(base |> select(mes,ust_l2),by='mes') |>
  mutate(tendencia=(row_number()-n())/12)
for(v in series) for(j in 1:3)
  amostra[[paste0(v,'_l',j)]] <- dplyr::lag(amostra[[v]],j)
cols_lags <- unlist(lapply(series,function(v) paste0(v,'_l',1:3)))
amostra <- amostra |>
  filter(mes<=fim_amostra,
    if_all(all_of(c(series,cols_lags,'ust_l2','tendencia')),is.finite))
stopifnot(!any(has_gaps(as_tsibble(amostra,index=mes))$.gaps))

# Teste conjunto dos coeficientes do bloco com covariancia HAC de tres meses.
# p=2 principal, p=1 e p=3 sensibilidades; mesma amostra em todos.
wald <- function(m,termos) {
  b <- coef(m)[termos]
  v <- sandwich::NeweyWest(m,lag=3,prewhite=FALSE,adjust=TRUE)[termos,termos,drop=FALSE]
  w <- as.numeric(t(b)%*%solve(v,b))
  tibble(wald=w,restricoes=length(termos),p_hac=pchisq(w,length(termos),lower.tail=FALSE))
}
modelos <- list()
testes <- purrr::map_dfr(1:3,function(p) {
  lags <- unlist(lapply(series,function(v) paste0(v,'_l',seq_len(p))))
  regressoras <- c(lags,'ust_l2','tendencia')
  saida_p <- purrr::map_dfr(domesticas,function(alvo) {
    m <- lm(reformulate(regressoras,alvo),data=amostra)
    modelos[[paste0('p',p,'_',alvo)]] <<- m
    wald(m,paste0('fluxo_l',seq_len(p))) |>
      mutate(direcao='Fluxo para domestica',variavel=alvo)
  })
  mf <- lm(reformulate(regressoras,'fluxo'),data=amostra)
  modelos[[paste0('p',p,'_fluxo')]] <<- mf
  inverso <- purrr::map_dfr(domesticas,function(v)
    wald(mf,paste0(v,'_l',seq_len(p))) |>
      mutate(direcao='Domestica para fluxo',variavel=v))
  conjunto <- wald(mf,unlist(lapply(domesticas,function(v) paste0(v,'_l',seq_len(p))))) |>
    mutate(direcao='Domesticas conjuntamente para fluxo',variavel='Conjunto das cinco')
  bind_rows(saida_p,inverso,conjunto) |>
    mutate(lags=p,n=nrow(amostra),inicio=as.Date(min(amostra$mes)),fim=as.Date(max(amostra$mes)))
}) |>
  group_by(lags,direcao) |> mutate(p_BH=p.adjust(p_hac,method='BH')) |> ungroup()

# CV de uma etapa: observacoes anteriores ao mes previsto, sem insumos futuros.
# Compara regressao completa com exclusao apenas dos lags de fluxo.
# Principal p=2, meses de set/2024 ate o fim disponivel.
p <- 2L
regressoras <- c(unlist(lapply(series,function(v) paste0(v,'_l',1:p))),'ust_l2','tendencia')
sem_fluxo <- setdiff(regressoras,paste0('fluxo_l',1:p))
cv <- purrr::map_dfr(domesticas,function(alvo) {
  purrr::map_dfr(which(amostra$mes>=yearmonth('2024 Sep')),function(i) {
    treino <- amostra[seq_len(i-1L),]
    novo <- amostra[i,]
    com <- lm(reformulate(regressoras,alvo),data=treino)
    sem <- lm(reformulate(sem_fluxo,alvo),data=treino)
    tibble(variavel=alvo,mes=as.Date(novo$mes),observado=novo[[alvo]],
      com_fluxo=as.numeric(predict(com,novo)),sem_fluxo=as.numeric(predict(sem,novo)))
  })
})
metricas <- cv |> group_by(variavel) |>
  summarise(origens=n(),rmse_com_fluxo=sqrt(mean((observado-com_fluxo)^2)),
    rmse_sem_fluxo=sqrt(mean((observado-sem_fluxo)^2)),.groups='drop') |>
  mutate(ganho_rmse_pct=100*(1-rmse_com_fluxo/rmse_sem_fluxo))
write_csv(testes,file.path(saida_teste,'testes HAC.csv'))
write_csv(metricas,file.path(saida_teste,'CV um mes metricas.csv'))
write_csv(cv,file.path(saida_teste,'CV um mes previsoes.csv'))
write_csv(amostra |> mutate(mes=as.Date(mes)),file.path(saida_teste,'amostra.csv'))
saveRDS(modelos,file.path(saida_teste,'regressoes.rds'))
cat('Triagem em variacoes; principal p=2. Nao e o BVAR nem evidencia causal.\n')
print(testes |> filter(lags==2),width=Inf,n=Inf)
print(metricas,width=Inf)
cat('\nSensibilidade p=1 e p=3:\n')
print(testes |> filter(lags!=2) |> select(lags,direcao,variavel,p_hac,p_BH),width=Inf,n=Inf)
