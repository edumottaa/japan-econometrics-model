# Copia sem tendencia do BVAR estrutural com producao mundial; demais configuracoes preservadas.
# Mesma especificacao C_so_oferta; reunida na pasta models para executar por Source.
.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
library(tidyverse)
library(fpp3)
library(bsvars)
library(bsvarSIGNs)

raiz <- "C:/Users/dudup/OneDrive/japan-econometrics-model"
fonte_dados <- file.path(raiz, "data/processed/cenarios_propios_20261104/modelo_producao")
fonte <- file.path(raiz, "data/processed/sensibilidade_pesos_sem_tendencia/rho_0990")
dir.create(fonte, recursive = TRUE, showWarnings = FALSE)
dados_boj <- readRDS(file.path(fonte_dados, "dados.rds"))

# Pesos por precisão: Var(erro_t) = escala_COVID_t^2 * Sigma / peso_t.

# 1. Construção das matrizes dos blocos externo e doméstico

matrix_externa <- dados_boj |>
  as_tibble() |>
  select(ust2y, brent, ip_mundial) |>
  as.matrix()

matrix_interna <- dados_boj |>
  as_tibble() |>
  select(atividade, cpi_core, juro_curto, inclinacao, usdjpy) |>
  as.matrix()

# Sem tendencia linear: manter a constante e as seis defasagens.

#2. Seis defasagens mensais; oito variáveis e oito choques estruturais
p <- 6

# Priors 
lambda <- 0.2
mu <- 1
delta <- 1
rho_peso <- 0.99  # Desconto mensal: 1 = pesos iguais; menor = mais peso ao recente
stopifnot(rho_peso > 0, rho_peso <= 1)
stopifnot(lambda > 0, mu > 0, delta > 0)

#3 Estimação dos hiperparametros da COVID
# COVID contada após as p observações iniciais usadas como defasagens
inicio_covid <- match(as.Date("2020-03-01"), as.Date(dados_boj$date)) - p
if (is.na(inicio_covid) || inicio_covid <= 0) inicio_covid <- NULL
# Estimar os quatro parâmetros COVID somente com quatro meses já observados
if (!is.null(inicio_covid) && nrow(dados_boj) - p - inicio_covid + 1L < 4L) inicio_covid <- NULL

# Respostas x choques x horizontes; posições 1:13 representam meses 0:12
# Variáveis: ust2y, brent, ip_mundial, atividade, cpi_core, juro_curto, inclinacao, usdjpy
# Choques: Fed, petróleo, residual externo, residual doméstico 1, residual doméstico 2, BoJ, prêmio fiscal/prazo, residual de fluxo/UIP
sinais_boj <- array(NA_real_, dim = c(8, 8, 13))

# BoJ (6): juro sobe, curva achata; atividade e CPI caem nos meses 3 a 12
sinais_boj[6, 6, 1:2] <- 1
sinais_boj[7, 6, 1:2] <- -1
sinais_boj[c(4, 5), 6, 4:13] <- -1

# Choque 3: residual externo; choques 4 e 5: residuais domésticos, sem sinais.

# Prêmio fiscal/de prazo (7): curva inclina e USDJPY sobe
# Esses sinais não provam uma causa fiscal exclusiva
sinais_boj[7, 7, 1:2] <- 1
sinais_boj[8, 7, 1:2] <- 1

# Fed (1): Treasury de 2 anos sobe e Brent cai; respostas japonesas livres
sinais_boj[1, 1, 1:2] <- 1
sinais_boj[2, 1, 1:2] <- -1

# Petroleo (2): Brent sobe; IP mundial cai em 1-6; Treasury e respostas japonesas livres

sinais_boj[2, 2, 1:2] <- 1

# Fluxo/UIP (8): residual, sem restricoes de sinal ou narrativas

# 4. Restrições Narrativas 

# A oferta e identificada pela IP mundial; demanda global e risk-off nao sao identificados separadamente.

# 4.3. Fim do juro negativo e do YCC em março/2024: aperto do BoJ (choque 6).
# Decisão em 19/03/2024; o evento não prova que a surpresa mensal tenha sido positiva.
# Fonte: https://www.boj.or.jp/en/mopo/mpmdeci/state_2024/k240319a.htm
inicio_fim_ycc <- match(as.Date("2024-03-01"), as.Date(dados_boj$date))
narrativa_boj <- list()
if (!is.na(inicio_fim_ycc) && inicio_fim_ycc > p) {
  narrativa_boj <- list(bsvarSIGNs::specify_narrative(
    start = inicio_fim_ycc, periods = 1, type = "S", sign = 1, shock = 6
  ))
}

# 4.4. Eleição de Takaichi para a presidência do LDP em outubro/2025: prêmio positivo (choque 7).
# Eleição em 04/10/2025; o sinal fiscal/de prazo é a hipótese do plano, não um fato confirmado pela data.
# Fonte: https://www.jimin.jp/news/information/211511.html
inicio_takaichi <- match(as.Date("2025-10-01"), as.Date(dados_boj$date))
narrativa_takaichi <- list()
if (!is.na(inicio_takaichi) && inicio_takaichi > p) {
  narrativa_takaichi <- list(bsvarSIGNs::specify_narrative(
    start = inicio_takaichi, periods = 1, type = "S", sign = 1, shock = 7
  ))
}

narrativas <- c(narrativa_boj, narrativa_takaichi)

# Oferta: Brent positivo em 0-1; producao mundial negativa em 1-6.
sinais_boj[3, 2, 2:7] <- -1
# Choque 3 permanece residual externo, sem identificar demanda mundial.

# Os choques identificados entram juntos na mesma especificação e na mesma estimação
spec_boj = specify_bsvarSIGN$new(
  data = matrix_interna,
  foreign = matrix_externa,
  p = p,
  hyper_mu = FALSE,
  hyper_delta = FALSE,
  hyper_lambda = FALSE,
  hyper_psi = FALSE,
  hyper_covid = inicio_covid,
  sign_irf = sinais_boj,
  sign_narrative = narrativas,
  sign_structural = matrix(NA_real_, 8, 8)
)

# Aplicar os valores manuais antes da estimacao.
spec_boj$prior$hyper[1:3, ] <- c(mu, delta, lambda)

# 4. Pesos exponenciais, depois de construir as defasagens
# Multiplicar a equação inteira por sqrt(peso) dá menor precisão aos meses antigos.
Y_boj <- spec_boj$data_matrices$Y
X_boj <- spec_boj$data_matrices$X
pesos <- rho_peso^rev(seq_len(ncol(Y_boj)) - 1)
stopifnot(all(is.finite(pesos)), all(pesos > 0))
spec_boj$data_matrices$Y <- sweep(Y_boj, 2, sqrt(pesos), "*")
spec_boj$data_matrices$X <- sweep(X_boj, 2, sqrt(pesos), "*")
spec_boj$prior$Y <- spec_boj$data_matrices$Y
spec_boj$prior$X <- spec_boj$data_matrices$X


# 5. Estimar a posterior com 2.500 sorteios.
# Recalibrar a COVID: a matriz X mudou com a retirada da tendencia.
set.seed(123)
if (!is.null(inicio_covid)) spec_boj$estimate_hyper(S = 5000, burn_in = 2500)
saveRDS(spec_boj, file.path(fonte, "especificacao_hiper_covid.rds"))
spec_boj$identification$max_tries <- 10000L
spec_boj$mc.cores <- 4L
set.seed(123)
post_boj <- bsvars::estimate(spec_boj, S = 2500, show_progress = TRUE)

# 6. Recuperar as unidades historicas e os choques, como na finalizacao do experimento.
S <- dim(post_boj$posterior$A)[3]; p <- post_boj$last_draw$p
Yw <- post_boj$last_draw$data_matrices$Y; Xw <- post_boj$last_draw$data_matrices$X
pesos <- rho_peso^rev(seq_len(ncol(Yw))-1)
Y <- sweep(Yw,2,sqrt(pesos),'/'); X <- sweep(Xw,2,sqrt(pesos),'/')
original <- dados_boj |> as_tibble() |> select(ust2y,brent,ip_mundial,atividade,cpi_core,juro_curto,inclinacao,usdjpy) |> as.matrix()
stopifnot(max(abs(Y-t(original[-seq_len(p),])))<1e-10)
inicio <- match(as.Date('2020-03-01'),as.Date(dados_boj$date))-p
escalas <- array(NA_real_,c(nrow(Y),ncol(Y),S)); choques <- escalas
for(s in seq_len(S)) {
  escala <- rep(1,ncol(Y)); covid <- tail(post_boj$posterior$hyper[,s],4)
  escala[inicio+0:2] <- covid[1:3]
  meses <- (inicio+3):length(escala)
  escala[meses] <- 1+(covid[3]-1)*covid[4]^seq_along(meses)
  escala <- escala/sqrt(pesos)
  escalas[,,s] <- matrix(escala,nrow(Y),ncol(Y),byrow=TRUE)
  choques[,,s] <- post_boj$posterior$B[,,s]%*%sweep(Y-post_boj$posterior$A[,,s]%*%X,2,escala,'/')
}
post_boj$posterior$sigma <- escalas; post_boj$posterior$shocks <- choques
post_boj$last_draw$data_matrices$Y <- Y; post_boj$last_draw$data_matrices$X <- X
post_boj$last_draw$prior$Y <- Y; post_boj$last_draw$prior$X <- X
attr(post_boj,'pesos_exponenciais') <- list(rho=rho_peso,dados=tibble(date=dados_boj$date[-seq_len(p)],peso=pesos))
a <- matrix(post_boj$posterior$A,ncol=S)
dom <- unlist(lapply(0:(p-1),function(j) 4:8+8*j))
diag <- tibble(variante = "C_so_oferta_sem_tendencia",sorteios=S,ess=post_boj$posterior$ess,
  distintos=ncol(unique(a,MARGIN=2)),finitos=all(is.finite(a)),
  feedback_lags=max(abs(post_boj$posterior$A[1:3,dom,])),
  feedback_impacto=max(abs(post_boj$posterior$Theta0[1:3,4:8,])))
# Conferir ANTES de substituir os resultados salvos.
stopifnot(diag$ess >= 50, diag$distintos >= 50, diag$finitos,
  diag$feedback_lags < 1e-8, diag$feedback_impacto < 1e-8)
irfs <- bsvars::compute_impulse_responses(post_boj, horizon = 24, standardise = FALSE)
stopifnot(all(irfs[2, 2, 1:2, ] > 0), all(irfs[3, 2, 2:7, ] < 0))

# Armazenamento da saida em data/processed para os scripts de forecast.
saveRDS(post_boj, file.path(fonte, "modelo.rds"))
saveRDS(dados_boj, file.path(fonte, "dados.rds"))
saveRDS(irfs, file.path(fonte, "irfs.rds"))
utils::write.csv(diag, file.path(fonte, "diagnostico.csv"), row.names = FALSE)
print(diag)

