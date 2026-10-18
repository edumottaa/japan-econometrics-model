# 1. Data collection

library(fredr)
library(tidyverse)
library(estatapi)
library(fpp3)
library(bsvarSIGNs)


# FRED
fredr_set_key(Sys.getenv("FRED_API_KEY"))   # key stored in ~/.Renviron

series_ids <- c(
  "DGS2",            # 1. US 2-year Treasury yield (%)
  "DCOILBRENTEU",    # 2. Brent crude (USD/barrel)
  "VIXCLS",          # 3. VIX
  "DEXJPUS",         # 4. USD/JPY
  "SPASTT01JPM661N"  # 5. TOPIX (monthly average, 2015=100; OECD)
)

fred <- map_dfr(series_ids, ~ fredr(.x,
                                    observation_start = as.Date("2000-01-01"),
                                    frequency         = "m")) |>
  select(date, series_id, value) |>
  pivot_wider(names_from = series_id, values_from = value)

# e-Stat: core CPI
cpi <- estat_getStatsData(
  appId       = Sys.getenv("ESTAT_APP_ID"),  # key stored in ~/.Renviron
  statsDataId = "0003427113",  # 2020 base year
  cdTab       = "1",           # Index
  cdArea      = "00000",       # Japan
  cdCat01     = "0161",        # Core CPI (ex fresh food)
  cdTimeFrom  = "2000000101"
) |>
  mutate(ano = substr(time_code, 1, 4),
         mes = substr(time_code, 7, 8)) |>
  filter(mes != "00") |>       # Exclude annual averages
  transmute(date = ym(paste(ano, mes)), cpi_core = value)

# MoF: 10-year minus 2-year JGB yield
url <- "https://www.mof.go.jp/english/policy/jgbs/reference/interest_rate/"

le_jgb <- function(arquivo) {
  read_csv(paste0(url, arquivo), skip = 1, na = "-",
           col_types = cols(Date = col_date("%Y/%m/%d")))
}

jgb <- bind_rows(le_jgb("historical/jgbcme_all.csv"), le_jgb("jgbcme.csv")) |>
  filter(Date >= "2000-01-01") |>
  group_by(date = floor_date(Date, "month")) |>
  summarise(inclinacao_10_2 = mean(`10Y` - `2Y`, na.rm = TRUE))

# CPB: global industrial production
download.file("https://www.cpb.nl/system/files/cpbmedia/CPB-world-trade-monitor-july-2026.xlsx",
              "cpb_wtm.xlsx", mode = "wb")

ip_mundial <- readxl::read_excel("cpb_wtm.xlsx", sheet = "inpro_out", skip = 3) |>
  filter(...2 == "ipz_w1_qnmi_sm") |>
  pivot_longer(matches("^\\d{4}m"), names_to = "date", values_to = "ip_mundial") |>
  transmute(date = ym(date), ip_mundial)

# Cabinet Office: coincident index
# Find the latest release file on the source page.
pagina_ci <- 
  readLines("https://www.esri.cao.go.jp/jp/stat/di/di.html", warn = FALSE)
arq_ci    <- 
  unique(regmatches(pagina_ci, regexpr("[0-9]{4}ci[.]xlsx", pagina_ci)))[1]
download.file(paste0("https://www.esri.cao.go.jp/jp/stat/di/", arq_ci),
              "ci.xlsx", mode = "wb")


atividade <- readxl::read_excel("ci.xlsx", skip = 6, col_names = FALSE) |>
  transmute(date = make_date(...2, ...3), atividade = ...5) |>
  filter(date >= "2000-01-01")

# Short-term rate: Nakajima shadow rate and BoJ call rate
shadow <- read_csv("https://raw.githubusercontent.com/jouchinakajima/program/main/yield_M.csv") |>
  transmute(date = ym(YYYYMM), shadow = ShadowRate)

boj <- jsonlite::fromJSON("https://www.stat-search.boj.or.jp/api/v1/getDataCode?format=json&lang=en&db=FM02&code=STRACLUCON&startDate=200001")

call_rate <- tibble(date      = ym(boj$RESULTSET$VALUES$SURVEY_DATES[[1]]),
                    call_rate = boj$RESULTSET$VALUES$VALUES[[1]])

juro_curto <- full_join(shadow, call_rate, by = "date") |>
  filter(date >= "2000-01-01") |>
  mutate(juro_curto = if_else(date < "2024-03-01", shadow, call_rate)) |>
  arrange(date)


# 2. Data preparation

# 2.1 Rename series and create a tsibble
data <- fred |>
  rename(
    ust2y  = DGS2,
    brent  = DCOILBRENTEU,
    vix    = VIXCLS,
    usdjpy = DEXJPUS,
    topix  = SPASTT01JPM661N
  ) |>
  left_join(ip_mundial, by = "date") |>
  left_join(atividade,  by = "date") |>
  left_join(cpi,        by = "date") |>
  left_join(select(juro_curto, date, juro_curto), by = "date") |>
  left_join(jgb,        by = "date") |>
  mutate(date = yearmonth(date)) |>
  filter(date < yearmonth(Sys.Date())) |>   # Exclude the incomplete current month
  as_tsibble(index = date)

# 2.2 Transformations
data_modelo <- data |>
  transmute(
    # Rates and spreads: levels (%)
    ust2y      = ust2y,
    juro_curto = juro_curto,
    inclinacao = inclinacao_10_2,
    
    # Indices and prices: 100 * log(x)
    ip_mundial = 100 * log(ip_mundial),
    brent      = 100 * log(brent),
    vix        = 100 * log(vix),
    atividade  = 100 * log(atividade),
    cpi_core   = 100 * log(cpi_core),
    usdjpy     = 100 * log(usdjpy),
    topix      = 100 * log(topix)
  )

# 3. Plot each model variable
data_modelo |>
  as_tibble() |>
  mutate(date = as.Date(date)) |>
  gather(variavel, valor, -date) |>
  ggplot(aes(x = date, y = valor, colour = variavel)) +
  geom_line() +
  facet_wrap(~variavel, scales = "free") +
  theme(legend.position = "none")



# Keep observations through the latest month with complete data.


ultimo <- data_modelo |>
  as_tibble() |>
  drop_na() |>
  summarise(ultimo = max(date)) |>
  pull(ultimo)

data_bal <- data_modelo |>
  filter(date <= ultimo)

# Check for missing values
colSums(is.na(as_tibble(data_bal)))

# 4 Initial VAR and lag selection
lag_selection <- data_bal |>
  model(
    aicc = VAR(vars(atividade,cpi_core, juro_curto)),
    bic = VAR(vars(atividade,cpi_core, juro_curto), ic = "bic")
  )
lag_selection

lag_selection |>
  select(aicc) |>
  forecast() |>
  autoplot(data_bal |> filter(year(date) > 2000))

# 5 Build the model data matrix
data_matrix <- data_bal |>
  as_tibble() |>
  select(ust2y, ip_mundial, brent, vix,
         atividade, cpi_core, juro_curto, inclinacao, usdjpy, topix) |>
  as.matrix()

# 6 Specify the BVAR (Minnesota + sum-of-coefficients + single-unit-root prior, GLP 2015)
# TODO (Jota): block exogeneity (plan 3.3). `foreign` in bsvarSIGNs 3.0 imposes it by
# importance sampling, which degenerates here (effective sample size ~2 of 200 draws),
# so the 10 variables enter as a single block for now.
# TODO (Jota): COVID adjustment (hyper_covid).
spec <- specify_bsvarSIGN$new(data = data_matrix, p = 12)


# COVID: position of March 2020 in the estimation sample (the first 12 months are only lags)
covid <- which(data_bal$date == yearmonth("2020 Mar")) - 12


























# 7 Posterior of the hyperparameters and of the VAR parameters
# Three routines of bsvarSIGNs 3.0 are replaced (details in the PDF):
#  - estimate_hyper(): a failed evaluation of the dummy-observation likelihood returns
#    -1e10, which is subtracted and becomes +1e10, so the sampler gets stuck;
#  - estimate(): computes the posterior scale of Sigma as a difference of large matrices,
#    which stops being positive definite for the hyperparameters favoured by the data;
#  - forecast(): starts one period early and, for p > 1, only updates the first lag.
# The model is unchanged: same prior and likelihood, from the package's own functions.
make_bvar <- function(spec) {
  ns     <- asNamespace("bsvarSIGNs")
  log_ml <- get("_bsvarSIGNs_log_ml", ns)
  log_pr <- get("_bsvarSIGNs_log_prior_hyper", ns)
  prior  <- spec$prior$get_prior()
  Y  <- t(spec$prior$Y)
  X  <- t(spec$prior$X)
  N  <- ncol(Y)
  K  <- ncol(X)
  p  <- spec$p
  B0 <- t(prior$A)
  v0 <- diag(prior$V)
  Ysoc <- t(prior$Ysoc); Xsoc <- t(prior$Xsoc)
  Ysur <- t(prior$Ysur); Xsur <- t(prior$Xsur)

  # prior variances and dummy observations for h = (mu, delta, lambda, psi_1, ..., psi_N)
  setup <- function(h) {
    v <- v0
    v[1:(N * p)] <- h[3]^2 * v0[1:(N * p)] * rep(1 / h[-(1:3)], p)
    list(v = v, psi = h[-(1:3)],
         Yd = rbind(Ysoc / h[1], Ysur / h[2]), Xd = rbind(Xsoc / h[1], Xsur / h[2]))
  }

  # log posterior of the hyperparameters; a failed evaluation is an impossible point
  log_post <- function(h) {
    s <- setup(h)
    ml_all   <- .Call(log_ml, B0, diag(s$v), diag(s$psi), prior$nu, rbind(s$Yd, Y), rbind(s$Xd, X))
    ml_dummy <- .Call(log_ml, B0, diag(s$v), diag(s$psi), prior$nu, s$Yd, s$Xd)
    if (ml_all <= -1e10 || ml_dummy <= -1e10) return(-1e10)
    out <- .Call(log_pr, matrix(c(h, 1, 1, 1, 0.8)), c(1, 1, 1, 1, 0), prior) + ml_all - ml_dummy
    if (is.finite(out)) out else -1e10
  }

  # one draw of (B, Sigma) from the conjugate posterior, given h
  draw <- function(h) {
    s     <- setup(h)
    Yp    <- rbind(s$Yd, Y)
    Xp    <- rbind(s$Xd, X)
    V_inv <- diag(1 / s$v)
    R     <- chol(V_inv + crossprod(Xp))                    # posterior precision of B = R'R
    B_hat <- backsolve(R, forwardsolve(t(R), crossprod(Xp, Yp) + V_inv %*% B0))
    E     <- Yp - Xp %*% B_hat
    S     <- diag(s$psi) + crossprod(E) + t(B_hat - B0) %*% V_inv %*% (B_hat - B0)
    Sigma <- solve(rWishart(1, prior$nu + nrow(Yp), solve(S))[, , 1])
    B     <- B_hat + backsolve(R, matrix(rnorm(K * N), K, N)) %*% chol(Sigma)
    list(B = B, Sigma = Sigma)
  }

  list(log_post = log_post, draw = draw)
}

bv      <- make_bvar(spec)
target  <- function(z) bv$log_post(exp(z)) + sum(z)   # hyperparameters on the log scale (+ Jacobian)
n_hyper <- 3 + ncol(data_matrix)

# 7.1 Posterior mode of the hyperparameters, starting from the package defaults
opt <- optim(log(spec$prior$hyper[1:n_hyper, 1]), function(z) -target(z),
             method = "BFGS", control = list(maxit = 2000))
stopifnot(opt$convergence == 0)

# 7.2 Random-walk Metropolis (GLP 2015): proposal = inverse Hessian at the mode, scaled by 2.38 / sqrt(d)
L <- t(chol(solve(optimHess(opt$par, function(z) -target(z))))) * 2.38 / sqrt(n_hyper)

n_draw <- 15000
n_burn <- 5000
n_keep <- 1000

set.seed(123)
z <- opt$par
lp <- target(z)
accepted <- 0
draws <- matrix(NA, n_hyper, n_draw)
for (i in 1:n_draw) {
  z_new  <- z + drop(L %*% rnorm(n_hyper))
  lp_new <- target(z_new)
  if (log(runif(1)) < lp_new - lp) {
    z <- z_new
    lp <- lp_new
    accepted <- accepted + 1
  }
  draws[, i] <- exp(z)
}
accepted / n_draw   # acceptance rate, should be around 0.2-0.4

hyper <- draws[, seq(n_burn + 1, n_draw, by = (n_draw - n_burn) / n_keep)]
rownames(hyper) <- c("mu", "delta", "lambda", paste0("psi_", colnames(data_matrix)))

# 7.3 Inspect the hyperparameter draws
round(t(apply(hyper, 1, quantile, probs = c(0.05, 0.5, 0.95))), 4)
plot.ts(t(hyper[c("lambda", "mu", "delta"), ]), main = "Hyperparameter draws")

# 7.4 VAR parameters: one draw of (B, Sigma) per hyperparameter draw
set.seed(123)
post <- lapply(seq_len(n_keep), function(s) bv$draw(hyper[, s]))

# 8 Forecast: simulate the VAR forward from the last observation, once per posterior draw
forecast_var <- function(post, data, p, horizon) {
  N  <- ncol(data)
  T  <- nrow(data)
  x0 <- c(t(data[T:(T - p + 1), ]), 1)         # (y_T, y_T-1, ..., y_T-p+1, constant)
  out <- array(NA, c(horizon, N, length(post)), dimnames = list(NULL, colnames(data), NULL))
  for (s in seq_along(post)) {
    x <- x0
    L <- t(chol(post[[s]]$Sigma))
    for (h in 1:horizon) {
      y <- drop(crossprod(post[[s]]$B, x) + L %*% rnorm(N))
      out[h, , s] <- y
      x <- c(y, x[1:(N * (p - 1))], 1)
    }
  }
  out
}

horizon <- 24
set.seed(123)
fc <- forecast_var(post, data_matrix, p = 12, horizon = horizon)

# Fan chart: median and 90% band, history since 2021
fc_bands <- map_dfr(colnames(data_matrix), function(v) tibble(
  date     = as.Date(max(data_bal$date) + 1:horizon),
  variavel = v,
  q05      = apply(fc[, v, ], 1, quantile, 0.05),
  q50      = apply(fc[, v, ], 1, median),
  q95      = apply(fc[, v, ], 1, quantile, 0.95)
))

history <- data_bal |>
  as_tibble() |>
  filter(year(date) >= 2021) |>
  mutate(date = as.Date(date)) |>
  pivot_longer(-date, names_to = "variavel", values_to = "valor")

ggplot() +
  geom_ribbon(data = fc_bands, aes(date, ymin = q05, ymax = q95), fill = "#F500BD", alpha = 0.25) +
  geom_line(data = fc_bands, aes(date, q50), colour = "#F500BD") +
  geom_line(data = history, aes(date, valor)) +
  facet_wrap(~variavel, scales = "free_y")



#Cross Validation Test CPI
