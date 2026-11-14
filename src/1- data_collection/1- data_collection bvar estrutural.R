# 1. Extração de dados do núcleo A: dez variáveis

library(fredr)
library(tidyverse)
library(estatapi)
library(fpp3)
library(bsvarSIGNs)
library(readxl)
library(jsonlite)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
if (file.exists(".Renviron")) readRenviron(".Renviron")

# 1.1. FRED: juros americanos, petróleo, volatilidade, câmbio e bolsa
fredr_set_key(Sys.getenv("FRED_API_KEY"))

series_ids <- c(
  "DGS2",            # 1. Taxa do Treasury americano de 2 anos (%)
  "DCOILBRENTEU",    # 2. Petróleo Brent (USD por barril)
  "VIXCLS",          # 3. Índice de volatilidade VIX
  "DEXJPUS",         # 4. Taxa de câmbio USD/JPY
  "SPASTT01JPM661N"  # 5. TOPIX (média mensal, 2015 = 100; OCDE)
)

fred <- map_dfr(series_ids, ~ fredr(.x,
                                    observation_start = as.Date("2000-01-01"),
                                    frequency         = "m")) |>
  select(date, series_id, value) |>
  pivot_wider(names_from = series_id, values_from = value)

# 1.2. e-Stat: CPI subjacente, sem alimentos frescos
app_id_estat <- Sys.getenv("ESTAT_APP_ID")
cpi <- estat_getStatsData(
  appId       = app_id_estat,
  statsDataId = "0003427113",  # Ano-base: 2020
  cdTab       = "1",           # Índice de preços
  cdArea      = "00000",       # Japão
  cdCat01     = "0161",        # CPI subjacente, sem alimentos frescos
  cdTimeFrom  = "2000000101"
) |>
  mutate(ano = substr(time_code, 1, 4),
         mes = substr(time_code, 7, 8)) |>
  filter(mes != "00") |>       # Excluir as médias anuais
  transmute(date = ym(paste(ano, mes)), cpi_core = value)

# Encadear a base 2025 ao ultimo mes comum, preservando o historico em base 2020
cpi_2025 <- estat_getStatsData(appId = app_id_estat, statsDataId = "0004052037",
  cdTab = "1", cdArea = "00000", cdCat01 = "0161", cdTimeFrom = "2000000101") |>
  filter(substr(time_code, 7, 8) != "00") |>
  transmute(date = ym(paste(substr(time_code, 1, 4), substr(time_code, 7, 8))), cpi_novo = value)
elo_cpi <- inner_join(cpi, cpi_2025, by = "date") |> drop_na() |> slice_max(date, n = 1)
stopifnot(nrow(elo_cpi) == 1L)
cpi <- bind_rows(cpi, cpi_2025 |> filter(date > max(cpi$date)) |>
  transmute(date, cpi_core = cpi_novo * elo_cpi$cpi_core / elo_cpi$cpi_novo))

# 1.3. MoF: JGB30Y e spreads 10Y−2Y, 30Y−5Y e 30Y−10Y
# Taxas de maturidade constante; médias mensais das diferenças diárias.
url <- "https://www.mof.go.jp/english/policy/jgbs/reference/interest_rate/"

le_jgb <- function(arquivo) {
  read_csv(paste0(url, arquivo), skip = 1, na = c("-", ""),
           locale = locale(encoding = "CP932"),
           col_types = cols(.default = col_double(), Date = col_character()))
}

jgb_mof <- bind_rows(le_jgb("historical/jgbcme_all.csv"), le_jgb("jgbcme.csv")) |>
  mutate(Date = ymd(Date, quiet = TRUE)) |>
  filter(!is.na(Date), Date >= "2000-01-01") |>
  distinct(Date, .keep_all = TRUE)

jgb <- jgb_mof |>
  group_by(date = floor_date(Date, "month")) |>
  summarise(inclinacao_10_2 = mean(`10Y` - `2Y`, na.rm = TRUE))

jgb_diario <- jgb_mof |>
  transmute(date = Date, jgb30y = `30Y`, jgb5y = `5Y`) |>
  filter(is.finite(jgb30y), is.finite(jgb5y)) |>
  mutate(inclinacao_30_5 = jgb30y - jgb5y) |>
  arrange(date)

jgb_mensal <- jgb_diario |>
  group_by(date = floor_date(date, "month")) |>
  summarise(jgb30y = mean(jgb30y), jgb5y = mean(jgb5y),
            inclinacao_30_5 = mean(inclinacao_30_5), dias = n(), .groups = "drop") |>
  mutate(mes_completo = date < floor_date(Sys.Date(), "month"))

stopifnot(nrow(jgb_mensal) > 0,
          min(jgb_mensal$date) == as.Date("2000-01-01"),
          all(is.finite(jgb_mensal$inclinacao_30_5)))
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write_csv(jgb_diario, "data/raw/jgb30y_5y_mof_diario.csv")
write_csv(jgb_mensal, "data/processed/jgb30y_5y_mof_mensal.csv")
message("JGB 30Y/5Y: ", min(jgb_mensal$date), " a ", max(jgb_mensal$date),
        " | ", nrow(jgb_mensal), " meses.")

# JGB 30Y−10Y: reutilizar a mesma coleta diaria do MoF.
jgb30_10_diario <- jgb_mof |>
  transmute(date = Date, jgb30y = `30Y`, jgb10y = `10Y`) |>
  filter(is.finite(jgb30y), is.finite(jgb10y)) |>
  mutate(inclinacao_30_10 = jgb30y - jgb10y) |>
  arrange(date)

jgb30_10_mensal <- jgb30_10_diario |>
  group_by(date = floor_date(date, "month")) |>
  summarise(jgb30y = mean(jgb30y), jgb10y = mean(jgb10y),
            inclinacao_30_10 = mean(inclinacao_30_10), dias = n(), .groups = "drop") |>
  mutate(mes_completo = date < floor_date(Sys.Date(), "month"))

stopifnot(nrow(jgb30_10_mensal) > 0,
          min(jgb30_10_mensal$date) == as.Date("2000-01-01"),
          all(is.finite(jgb30_10_mensal$inclinacao_30_10)))

write_csv(jgb30_10_diario, "data/raw/jgb30y_10y_mof_diario.csv")
write_csv(jgb30_10_mensal, "data/processed/jgb30y_10y_mof_mensal.csv")
message("JGB 30Y/10Y: ", min(jgb30_10_mensal$date), " a ", max(jgb30_10_mensal$date),
        " | ", nrow(jgb30_10_mensal), " meses.")

# 1.4. CPB: produção industrial mundial
mes_cpb <- as.Date(yearmonth(Sys.Date()) - rep(1:6, each = 2))
urls_cpb <- paste0("https://www.cpb.nl/system/files/cpbmedia/", rep(c("CPB", "cpb"), 6),
                   "-world-trade-monitor-", tolower(month.name[month(mes_cpb)]), "-", year(mes_cpb), ".xlsx")
arquivo_cpb <- tempfile(fileext = ".xlsx")
for (url_cpb in urls_cpb) {
  baixou_cpb <- tryCatch(
    suppressWarnings(download.file(url_cpb, arquivo_cpb, mode = "wb", quiet = TRUE)),
    error = function(e) 1L) == 0L
  if (baixou_cpb) break
}
if (!baixou_cpb) stop("Não foi encontrado um Excel do CPB nos últimos seis meses.")
file.copy(arquivo_cpb, "data/cpb_wtm.xlsx", overwrite = TRUE)
message("CPB: ", basename(url_cpb))

ip_mundial <- read_excel("data/cpb_wtm.xlsx", sheet = "inpro_out", skip = 3) |>
  filter(...2 == "ipz_w1_qnmi_sm") |>
  pivot_longer(matches("^\\d{4}m"), names_to = "date", values_to = "ip_mundial") |>
  transmute(date = ym(date), ip_mundial)

# 1.5. Gabinete do Governo: índice coincidente de atividade
# Localizar o arquivo da divulgação mais recente na página da fonte
pagina_ci <- 
  readLines("https://www.esri.cao.go.jp/jp/stat/di/di.html", warn = FALSE)
arq_ci    <- 
  unique(regmatches(pagina_ci, regexpr("[0-9]{4}ci[.]xlsx", pagina_ci)))[1]
download.file(paste0("https://www.esri.cao.go.jp/jp/stat/di/", arq_ci),
              "data/ci.xlsx", mode = "wb")


atividade <- read_excel("data/ci.xlsx", skip = 6, col_names = FALSE) |>
  transmute(date = make_date(...2, ...3), atividade = ...5) |>
  filter(date >= "2000-01-01")

# 1.6. Juro curto: taxa-sombra de Nakajima e taxa interbancária do BoJ
shadow <- read_csv("https://raw.githubusercontent.com/jouchinakajima/program/main/yield_M.csv") |>
  transmute(date = ym(YYYYMM), shadow = ShadowRate)

# Overnight efetiva de fim de mês, em % a.a. (BoJ/FM02).
boj_overnight <- fromJSON(paste0(
  "https://www.stat-search.boj.or.jp/api/v1/getDataCode?format=json&lang=en",
  "&db=FM02&code=STRECLUCON&startDate=200001"))
overnight_fim_mes <- tibble(
  date = yearmonth(ym(boj_overnight$RESULTSET$VALUES$SURVEY_DATES[[1]])),
  overnight_fim_mes = as.numeric(boj_overnight$RESULTSET$VALUES$VALUES[[1]])) |>
  filter(date < yearmonth(Sys.Date())) |> arrange(date) |> as_tsibble(index = date)
stopifnot(nrow(overnight_fim_mes) > 0L, !any(has_gaps(overnight_fim_mes)$.gaps),
          all(is.finite(overnight_fim_mes$overnight_fim_mes)))
call_rate <- overnight_fim_mes |>
  as_tibble() |> transmute(date = as.Date(date), call_rate = overnight_fim_mes)

juro_curto <- full_join(shadow, call_rate, by = "date") |>
  filter(date >= "2000-01-01") |>
  mutate(juro_curto = if_else(date < "2024-03-01", shadow, call_rate)) |>
  arrange(date)



data_final_domesticas <- yearmonth("2026 Sep")
# 1.7. Preparar a amostra de oito variáveis do BVAR estrutural
dados_domesticos <- atividade |>
  full_join(cpi, by = "date") |>
  full_join(select(juro_curto, date, juro_curto), by = "date") |>
  full_join(jgb |> rename(inclinacao = inclinacao_10_2), by = "date") |>
  full_join(fred |> select(date, usdjpy = DEXJPUS), by = "date") |>
  mutate(date = yearmonth(date)) |>
  filter(date >= yearmonth("2000 Jan"), date <= data_final_domesticas,
         date < yearmonth(Sys.Date())) |>
  arrange(date)

dados_boj <- readRDS("data/cache/cache_fred.rds") |>
  transmute(date = yearmonth(date), ust2y = DGS2, brent = DCOILBRENTEU) |>
  inner_join(dados_domesticos, by = "date") |>
  select(date, ust2y, brent, atividade, cpi_core, juro_curto, inclinacao, usdjpy) |>
  mutate(across(c(brent, atividade, cpi_core, usdjpy), ~ 100 * log(.x))) |>
  drop_na() |>
  arrange(date) |>
  as_tsibble(index = date)
stopifnot(!any(has_gaps(dados_boj)$.gaps),
          all(is.finite(as.matrix(select(as_tibble(dados_boj), -date)))))
saveRDS(dados_domesticos, "data/cache/cache_domesticas_bvar.rds")
message("Amostra do BVAR: ", min(dados_boj$date), " a ", max(dados_boj$date), " | ", nrow(dados_boj), " meses.")
print(dados_domesticos |> filter(date > max(dados_boj$date)))
