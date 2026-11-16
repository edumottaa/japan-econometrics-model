# Recalcular somente a curva auxiliar com o forecast principal salvo.
# OIS de mercado congelado; repasse de 50% e regressao original preservados.
# Curva JGB: JGB2Y por regressao OIS/ARIMA; 30Y-5Y pelo BVAR.
library(fpp3)
library(readr)
library(readxl)

pasta <- "src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao"
dados <- "src/JGB30Y/projecoes/dados"
fim <- yearmonth("2028 Sep")
arquivo_bvar <- file.path(pasta, "resumo_forecast.csv")
nome_saida <- "curva_jgb_2_5_10_30_cenarios.csv"
variaveis <- c("jgb2", "jgb5", "jgb10", "jgb30")

# 1. Fechamentos dos quatro vencimentos, na mesma fonte MoF.
historico <- read_csv("src/JGB30Y/projecoes/dados/jgbcme_all_2026-11-04.csv",
                      skip = 1, na = c("-", ""), show_col_types = FALSE) |>
  transmute(date = ymd(Date), jgb2 = `2Y`, jgb5 = `5Y`,
            jgb10 = `10Y`, jgb30 = `30Y`) |>
  filter(date >= ymd("2016-01-01"), complete.cases(jgb2, jgb5, jgb10, jgb30)) |>
  group_by(mes = yearmonth(date)) |>
  summarise(date = last(date), across(all_of(variaveis), last), .groups = "drop") |>
  mutate(spread5_2 = jgb5 - jgb2, spread10_5 = jgb10 - jgb5,
         spread30_5 = jgb30 - jgb5)
origem <- historico |> slice_tail(n = 1)

# 2. Mesma regressao JGB2Y: OIS2Y defasado dois meses e erro ARIMA(0,0,1).
ois_hist <- read_csv("src/JGB30Y/projecoes/dados/ois2_historico.csv",
                     show_col_types = FALSE) |>
  transmute(mes = yearmonth(date), ois2_pct) |>
  filter(mes <= origem$mes) |> group_by(mes) |> summarise(ois2_pct = last(ois2_pct), .groups = "drop")
treino2 <- historico |> select(mes, jgb2) |>
  left_join(ois_hist, by = "mes") |>
  mutate(ois_lag2 = lag(ois2_pct, 2)) |>
  filter(!is.na(ois_lag2)) |> as_tsibble(index = mes)
modelo2 <- treino2 |>
  model(arima = ARIMA(jgb2 ~ 1 + ois_lag2 + pdq(0, 0, 1) + PDQ(0, 0, 0)))

ois <- read_xlsx(file.path(dados, "OIS jpy fwrd.xlsx"), skip = 1,
                 col_types = c("text", "numeric", "numeric")) |>
  transmute(inicio = mdy(Date), mes = yearmonth(inicio),
            forward = `Forward Rate`) |> arrange(mes)
montar_ois2 <- function(x) {
  x |> mutate(
    dias = as.numeric(lead(inicio) - inicio),
    desconto = cumprod(lag(1 / (1 + forward / 100 * dias / 365), default = 1)),
    ano1 = as.numeric(lead(inicio, 12) - inicio) / 365,
    ano2 = as.numeric(lead(inicio, 24) - lead(inicio, 12)) / 365,
    ois2 = 100 * (desconto - lead(desconto, 24)) /
      (ano1 * lead(desconto, 12) + ano2 * lead(desconto, 24)))
}

bvar <- read_csv(arquivo_bvar, show_col_types = FALSE) |>
  filter(variavel %in% c("juro_curto", "inclinacao")) |>
  select(cenario, variavel, mes = date, media) |>
  pivot_wider(names_from = variavel, values_from = media) |>
  mutate(mes = yearmonth(mes))
cenarios <- unique(bvar$cenario)

projetar2 <- function(cen) {
  curva <- ois |>
    left_join(bvar |> filter(cenario == .env$cen) |>
                select(mes, juro_curto), by = "mes") |>
    mutate(forward = forward + 0.5 * coalesce(juro_curto - forward, 0)) |>
    montar_ois2()
  novo <- bind_rows(ois_hist,
    curva |> filter(mes > origem$mes, mes <= fim) |>
      transmute(mes, ois2_pct = ois2)) |>
    arrange(mes) |> mutate(ois_lag2 = lag(ois2_pct, 2)) |>
    filter(mes > origem$mes) |> as_tsibble(index = mes)
  stopifnot(nrow(novo) == as.integer(fim - origem$mes), all(is.finite(novo$ois_lag2)))
  forecast(modelo2, new_data = novo) |> as_tibble() |>
    transmute(cenario = cen, mes, jgb2 = .mean)
}
jgb2 <- purrr::map_dfr(cenarios, projetar2)

# 3. Regressao das variacoes mensais dos segmentos intermediarios.
# Preve mudancas condicionais; o nivel parte do ultimo fechamento observado.
amostra <- historico |>
  mutate(across(c(jgb2, spread5_2, spread10_5, spread30_5),
                ~ .x - lag(.x), .names = "d_{.col}")) |>
  filter(complete.cases(d_jgb2, d_spread5_2, d_spread10_5, d_spread30_5))
modelo5 <- lm(d_spread5_2 ~ 0 + d_jgb2 + d_spread30_5, data = amostra)
modelo10 <- lm(d_spread10_5 ~ 0 + d_jgb2 + d_spread30_5, data = amostra)
coeficientes <- bind_rows(
  tibble(segmento = "5Y−2Y", termo = names(coef(modelo5)), beta = unname(coef(modelo5))),
  tibble(segmento = "10Y−5Y", termo = names(coef(modelo10)), beta = unname(coef(modelo10))))
write_csv(coeficientes, file.path(pasta, "curva_jgb_regressoes.csv"))

# 4. Somar as parcelas. A mudanca do BVAR 30Y−5Y parte de out/2026.
base_bvar <- bvar |> filter(cenario == "Base", mes == origem$mes) |>
  pull(inclinacao)
stopifnot(length(base_bvar) == 1L)
curva <- jgb2 |>
  left_join(bvar |> select(cenario, mes, inclinacao), by = c("cenario", "mes")) |>
  mutate(spread30_5 = origem$spread30_5 + inclinacao - base_bvar,
         d_jgb2 = jgb2 - origem$jgb2,
         d_spread30_5 = spread30_5 - origem$spread30_5,
         spread5_2 = origem$spread5_2 +
           coef(modelo5)["d_jgb2"] * d_jgb2 +
           coef(modelo5)["d_spread30_5"] * d_spread30_5,
         spread10_5 = origem$spread10_5 +
           coef(modelo10)["d_jgb2"] * d_jgb2 +
           coef(modelo10)["d_spread30_5"] * d_spread30_5,
         jgb5 = jgb2 + spread5_2,
         jgb10 = jgb5 + spread10_5,
         jgb30 = jgb5 + spread30_5,
         spread2_5 = jgb2 - jgb5, spread2_10 = jgb2 - jgb10,
         spread30_10 = jgb30 - jgb10,
         date = as.Date(mes + 1L) - 1) |>
  select(cenario, mes, date, all_of(variaveis), spread2_5, spread2_10,
         spread30_10, spread30_5, spread5_2, spread10_5)
stopifnot(nrow(curva) == 4L * as.integer(fim - origem$mes), all(is.finite(as.matrix(select(curva, all_of(variaveis))))))
write_csv(curva, file.path(pasta, nome_saida))

