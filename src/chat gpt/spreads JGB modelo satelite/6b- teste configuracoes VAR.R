Sys.setlocale('LC_ALL','Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
trabalho <- 'C:/Users/dudup/OneDrive/japan-econometrics-model/src/chat gpt/spreads JGB modelo satelite/resultados'
dados <- readRDS(file.path(trabalho,'dados.rds'))$dados
K <- 3L; H <- 12L
cenarios <- readRDS(file.path(trabalho,'forecast.rds'))$cenarios   # caminhos de UST30 e OIS2Y

# Qual VAR projetar as variaveis estrangeiras? Tres versoes, mesmas exogenas (UST30, OIS2Y).
configs <- list(`JGB30 em nivel`='jgb30',`JGB30 em variacao`='d_jgb30',`Sem JGB30`=NULL)
var_formula <- function(j) as.formula(paste0('vars(',paste(c(j,'liq_estr','d_fatia'),collapse=','),
  ') ~ AR(p=1:2) + xreg(1,d_ust30,d_ois2)'))
spreads <- list(d_s30_10='lag(d_fatia, 3)',d_s30_5='lag(liq_estr, 2)')
ajusta_spread <- function(treino,alvo) treino |>
  mutate(!!alvo:=if_else(row_number()<=K,NA_real_,.data[[alvo]])) |>
  model(a=ARIMA(!!as.formula(paste(alvo,'~ d_ust30 + d_ois2 +',spreads[[alvo]],'+ pdq(1,0,0) + PDQ(0,0,0) + 1'))))
constante <- function(m) tidy(m)$estimate[tidy(m)$term=='intercept']

# 1. Validacao cruzada da cadeia VAR -> spread (juros futuros realizados: ex post).
# Projecao sem a constante, como no forecast.
origens <- seq(yearmonth('2022 Dec'),max(dados$mes)-H,by=1)
cv <- map_dfr(origens,function(o) {
  treino <- dados |> filter(mes<=o); futuro <- dados |> filter(mes>o,mes<=o+H)
  vars_proj <- imap(configs,function(j,nome) forecast(treino |> model(v=VAR(!!var_formula(j),ic='aicc')),new_data=futuro)$.mean)
  imap_dfr(spreads,function(r,alvo) {
    m <- ajusta_spread(treino,alvo); c0 <- constante(m); real <- futuro[[alvo]]
    prev <- imap(vars_proj,function(v,nome) forecast(m,new_data=futuro |>
      mutate(liq_estr=v[,'liq_estr'],d_fatia=v[,'d_fatia']))$.mean-c0)
    teto <- forecast(m,new_data=futuro)$.mean-c0
    as_tibble(prev) |> mutate(`Estrangeiras realizadas (teto)`=teto,`Variacao zero`=0,
      alvo=alvo,origem=o,h=seq_len(H),real=real)
  })
}) |> pivot_longer(-c(alvo,origem,h,real),names_to='modelo',values_to='previsto') |>
  group_by(alvo,modelo,origem) |> arrange(h,.by_group=TRUE) |>
  mutate(erro=cumsum(real)-cumsum(previsto)) |> ungroup()
metricas <- cv |> filter(h %in% c(1,3,6,12)) |> group_by(alvo,modelo,h) |>
  summarise(RMSE_pb=round(sqrt(mean(erro^2)),2),.groups='drop') |>
  pivot_wider(names_from=h,values_from=RMSE_pb,names_prefix='h') |> arrange(alvo,h12)
print(metricas,n=Inf)

# 2. Plausibilidade: projecao de 24 meses com a trajetoria de mercado dos juros.
x <- cenarios$`Trajetoria do mercado`
ultimo_jgb <- last(dados$jgb30)
plaus <- imap_dfr(configs,function(j,nome) {
  v <- forecast(dados |> model(v=VAR(!!var_formula(j),ic='aicc')),new_data=x)$.mean
  jgb <- if(is.null(j)) NA_real_ else if(j=='jgb30') last(v[,'jgb30']) else ultimo_jgb+sum(v[,'d_jgb30'])/100
  tibble(versao=nome,JGB30_out28=round(jgb,2),compra_liq_media=round(mean(v[,'liq_estr']),2),
    compra_liq_out28=round(last(v[,'liq_estr']),2),fatia_acumulada_pp=round(sum(v[,'d_fatia']),1))
})
print(plaus)

# Armazenamento da saida em outputs.
write_csv(metricas,file.path(trabalho,'teste VAR cv metricas.csv'))
write_csv(plaus,file.path(trabalho,'teste VAR plausibilidade.csv'))
cat('TESTE_VAR_CONCLUIDO\n')
