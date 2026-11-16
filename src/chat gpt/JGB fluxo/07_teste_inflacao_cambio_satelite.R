# Comparacao isolada do satelite: substituir fluxo por inflacao e cambio.
# Nao reestima o BVAR nem altera o forecast vigente.
source('src/chat gpt/JGB fluxo/00_dados.R',encoding='UTF-8')
saida_teste <- file.path(saida,'teste inflacao cambio')
dir.create(saida_teste,recursive=TRUE,showWarnings=FALSE)

# Banco congelado do BVAR: CPI e USDJPY estao em 100*log(nivel).
# Inflacao em 12 meses (%); cambio em variacao log mensal (% aproximado).
# Cambio positivo representa depreciacao do iene. Insumos realizados: ex post.
macro <- readRDS(file.path(fonte_modelo,'dados.rds')) |> as_tibble() |>
  arrange(date) |>
  transmute(mes=yearmonth(date),
    inflacao_12m=100*(exp((cpi_core-lag(cpi_core,12))/100)-1),
    d_cambio=usdjpy-lag(usdjpy))
amostra <- base |> inner_join(macro,by='mes') |>
  filter(mes>=inicio_amostra,mes<=fim_amostra,
    if_all(c(d_spread,ois_pos,fluxo_l2,inflacao_12m,d_cambio),is.finite)) |>
  as_tsibble(index=mes)
stopifnot(!any(has_gaps(amostra)$.gaps))

# Mesmos controles, erros ARIMA e amostra; nao seleciona defasagens por p-valor.
ajustar <- function(x) x |> model(
  original=ARIMA(d_spread ~ 1+ois_pos+fluxo_l2+pdq(0,0,0)+PDQ(0,0,0)),
  fluxo_inflacao=ARIMA(d_spread ~ 1+ois_pos+fluxo_l2+inflacao_12m+
    pdq(0,0,0)+PDQ(0,0,0)),
  sem_fluxo=ARIMA(d_spread ~ 1+ois_pos+inflacao_12m+d_cambio+
    pdq(0,0,0)+PDQ(0,0,0)),
  completo=ARIMA(d_spread ~ 1+ois_pos+fluxo_l2+inflacao_12m+d_cambio+
    pdq(0,0,0)+PDQ(0,0,0)))
ajustes <- ajustar(amostra)
formulas <- list(
  original=d_spread ~ ois_pos+fluxo_l2,
  fluxo_inflacao=d_spread ~ ois_pos+fluxo_l2+inflacao_12m,
  sem_fluxo=d_spread ~ ois_pos+inflacao_12m+d_cambio,
  completo=d_spread ~ ois_pos+fluxo_l2+inflacao_12m+d_cambio)
hac <- purrr::imap_dfr(formulas,function(f,nome) {
  m <- lm(f,data=amostra)
  v <- sandwich::NeweyWest(m,lag=3,prewhite=FALSE,adjust=TRUE)
  h <- lmtest::coeftest(m,vcov.=v)
  tibble(.model=nome,term=if_else(rownames(h)=='(Intercept)','intercept',rownames(h)),
    std_error_hac=h[,2],p_hac=h[,4])
})
coeficientes <- tidy(ajustes) |> left_join(hac,by=c('.model','term'))
diagnosticos <- augment(ajustes) |> as_tibble() |> group_by(.model) |>
  summarise(n=sum(is.finite(.innov)),
    r2=1-sum(.innov^2)/sum((d_spread-mean(d_spread))^2),
    ljung_box_12_p=Box.test(.innov,lag=12,type='Ljung-Box',fitdf=0)$p.value,
    .groups='drop')
comparacao <- glance(ajustes) |> select(.model,AIC,AICc,BIC) |>
  left_join(diagnosticos,by='.model')

# Teste conjunto robusto: H0 de coeficientes da inflacao e do cambio iguais a zero.
conjunto <- purrr::map_dfr(c('sem_fluxo','completo'),function(nome) {
  m <- lm(formulas[[nome]],data=amostra)
  v <- sandwich::NeweyWest(m,lag=3,prewhite=FALSE,adjust=TRUE)
  termos <- c('inflacao_12m','d_cambio')
  b <- coef(m)[termos]
  w <- as.numeric(t(b)%*%solve(v[termos,termos],b))
  tibble(.model=nome,teste='Inflacao e cambio conjuntamente: Wald HAC, qui-quadrado(2)',
    estatistica=w,p_hac_conjunto=pchisq(w,df=2,lower.tail=FALSE))
})

# CV recente ex post em 12 meses: mesmas 11 origens para todos os modelos.
origens <- seq(yearmonth('2024 Oct'),max(amostra$mes)-12L,by=1L)
cv <- purrr::map_dfr(origens,function(o) {
  treino <- amostra |> filter(mes<=o)
  janela <- amostra |> filter(mes>o,mes<=o+12L)
  stopifnot(nrow(janela)==12L)
  previsao <- forecast(ajustar(treino),new_data=janela) |> as_tibble() |>
    group_by(.model) |> summarise(previsto=sum(.mean),.groups='drop')
  bind_rows(previsao,tibble(.model=c('Naive','Drift'),
    previsto=c(0,12*mean(treino$d_spread)))) |>
    mutate(origem=o,observado=sum(janela$d_spread))
})
metricas <- cv |> group_by(.model) |>
  summarise(origens=n(),rmse_pb=sqrt(mean((previsto-observado)^2)),
    mae_pb=mean(abs(previsto-observado)),.groups='drop')
correlacoes <- amostra |> as_tibble() |>
  select(fluxo_l2,inflacao_12m,d_cambio,ois_pos) |> cor()
write_csv(coeficientes,file.path(saida_teste,'coeficientes.csv'))
write_csv(comparacao,file.path(saida_teste,'ajuste e residuos.csv'))
write_csv(conjunto,file.path(saida_teste,'teste conjunto.csv'))
write_csv(metricas,file.path(saida_teste,'CV ex post metricas.csv'))
write_csv(cv |> mutate(origem=as.Date(origem)),file.path(saida_teste,'CV ex post previsoes.csv'))
write_csv(amostra |> as_tibble() |> mutate(mes=as.Date(mes)),file.path(saida_teste,'amostra.csv'))
write.csv(correlacoes,file.path(saida_teste,'correlacoes.csv'))
saveRDS(ajustes,file.path(saida_teste,'modelos.rds'))
cat('Amostra:',format(min(amostra$mes)),'a',format(max(amostra$mes)),'\n')
print(coeficientes,width=Inf)
print(comparacao,width=Inf)
print(conjunto,width=Inf)
print(metricas,width=Inf)
