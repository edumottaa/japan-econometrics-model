# 1. Data collection

library(fredr)
library(tidyverse)
library(estatapi)
library(fpp3)
library(bsvarSIGNs)


# FRED
fredr_set_key("19da91aa806704d2aa100725f3e017a7")

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
  appId       = "7750945adab45d705c86fcbda91715447f0f088c",
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
              "data/cpb_wtm.xlsx", mode = "wb")

ip_mundial <- readxl::read_excel("data/cpb_wtm.xlsx", sheet = "inpro_out", skip = 3) |>
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
              "data/ci.xlsx", mode = "wb")


atividade <- readxl::read_excel("data/ci.xlsx", skip = 6, col_names = FALSE) |>
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


