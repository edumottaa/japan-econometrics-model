# 1. Extração de dados para o BVAR reduzido

library(fredr)
library(tidyverse)
library(estatapi)
library(fpp3)
library(bsvarSIGNs)
library(readxl)
library(jsonlite)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")

# 1.1. FRED: juros americanos, petróleo, volatilidade, câmbio e bolsa
fredr_set_key("19da91aa806704d2aa100725f3e017a7")

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
app_id_estat <- "7750945adab45d705c86fcbda91715447f0f088c"
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

# 1.3. MoF: diferença entre as taxas dos JGBs de 10 e 2 anos
url <- "https://www.mof.go.jp/english/policy/jgbs/reference/interest_rate/"

le_jgb <- function(arquivo) {
  read_csv(paste0(url, arquivo), skip = 1, na = "-",
           col_types = cols(Date = col_date("%Y/%m/%d")))
}

jgb <- bind_rows(le_jgb("historical/jgbcme_all.csv"), le_jgb("jgbcme.csv")) |>
  filter(Date >= "2000-01-01") |>
  group_by(date = floor_date(Date, "month")) |>
  summarise(inclinacao_10_2 = mean(`10Y` - `2Y`, na.rm = TRUE))

# 1.4. CPB: produção industrial mundial
download.file("https://www.cpb.nl/system/files/cpbmedia/CPB-world-trade-monitor-july-2026.xlsx",
              "data/cpb_wtm.xlsx", mode = "wb")

ip_mundial <- read_excel("data/cpb_wtm.xlsx", sheet = "inpro_out", skip = 3) |>
  filter(...2 == "ipz_w1_qnmi_sm") |>
  pivot_longer(matches("^\\d{4}m"), names_to = "date", values_to = "ip_mundial") |>
  transmute(date = ym(date), ip_mundial)

# 1.5. Gabinete do Governo: índice coincidente de atividade
# Localizar o arquivo da divulgação mais recente na página da fonte.
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

boj <- fromJSON("https://www.stat-search.boj.or.jp/api/v1/getDataCode?format=json&lang=en&db=FM02&code=STRACLUCON&startDate=200001")

call_rate <- tibble(date      = ym(boj$RESULTSET$VALUES$SURVEY_DATES[[1]]),
                    call_rate = boj$RESULTSET$VALUES$VALUES[[1]])

juro_curto <- full_join(shadow, call_rate, by = "date") |>
  filter(date >= "2000-01-01") |>
  mutate(juro_curto = if_else(date < "2024-03-01", shadow, call_rate)) |>
  arrange(date)


# 2. Extração de dados para o S2: inflação e preços administrados

# 2.1. BoJ/Tankan: inflação esperada em 1 ano, todas as empresas e setores (% a.a.)
boj_tankan <- fromJSON("https://www.stat-search.boj.or.jp/api/v1/getDataCode?format=json&lang=en&db=CO&code=TK99F0000204HCQ00000&startDate=201401")
periodos <- boj_tankan$RESULTSET$VALUES$SURVEY_DATES[[1]]
expectativa_inflacao <- tibble(
  date = yearquarter(make_date(periodos %/% 100, (periodos %% 100) * 3)),
  expectativa_1ano = boj_tankan$RESULTSET$VALUES$VALUES[[1]]
)

# 2.2. BoJ: core e core-core sem fatores institucionais (variação em 12 meses, %)
download.file("https://www.boj.or.jp/en/research/research_data/cpi/cpirev.xlsx",
              "data/cpirev.xlsx", mode = "wb")
cpi_boj <- read_excel("data/cpirev.xlsx", skip = 5, col_names = FALSE) |>
  transmute(
    date = as.Date(...1),
    base = case_when(!is.na(...2) ~ "2025", !is.na(...3) ~ "2020", !is.na(...4) ~ "2015"),
    pi_core_sem_inst = coalesce(...2, ...3, ...4),
    pi_core_core_sem_inst = coalesce(...6, ...7, ...8)
  ) |>
  filter(!is.na(base))

cpi_core_core_boj <- cpi_boj |>
  select(date, pi_core_core_sem_inst)

# 2.3. MIC/e-Stat: CPI oficial e serviços; usar a mesma base de cada trecho do BoJ
# Índices (tab = 1) e variações oficiais em 12 meses (tab = 3).
cpi_s2_mic <- imap_dfr(
  c("2015" = "0003143513", "2020" = "0003427113", "2025" = "0004052037"),
  ~ estat_getStatsData(appId = app_id_estat, statsDataId = .x,
                      cdTab = "1,3", cdArea = "00000", cdCat01 = "0161,0178,0220",
                      cdTimeFrom = "2000000101") |> mutate(base = .y)
) |>
  filter(substr(time_code, 7, 8) != "00") |>
  transmute(date = ym(paste(substr(time_code, 1, 4), substr(time_code, 7, 8))),
            base, serie = paste0("c", cat01_code, "_", tab_code), valor = value) |>
  pivot_wider(names_from = serie, values_from = valor)

cpi_servicos <- cpi_s2_mic |>
  filter(base == "2025") |>
  transmute(date, cpi_servicos = c0220_1) |>
  arrange(date)

# 2.4. Fatores institucionais: CPI oficial menos CPI ajustado pelo BoJ (p.p. em 12 meses)
# Diferença calculada na mesma base; não são dummies nem projeções futuras.
fatores_institucionais <- cpi_s2_mic |>
  inner_join(cpi_boj, by = c("date", "base")) |>
  transmute(date,
            fator_inst_core = c0161_3 - pi_core_sem_inst,
            fator_inst_core_core = c0178_3 - pi_core_core_sem_inst) |>
  arrange(date)

# 2.5. BoJ: hiato do produto, trimestral (%)
download.file("https://www.boj.or.jp/en/research/research_data/gap/gap.xlsx",
              "data/gap.xlsx", mode = "wb")
hiato_produto <- read_excel("data/gap.xlsx", sheet = "data1", skip = 5, col_names = FALSE) |>
  transmute(date = yearquarter(...1), hiato = ...2) |>
  filter(date >= yearquarter("2000 Q1"), !is.na(hiato))

# 2.6. BoJ: índice de preços de importação em ienes, mensal (2020 = 100)
boj_importacao <- fromJSON("https://www.stat-search.boj.or.jp/api/v1/getDataCode?format=json&lang=en&db=PR01&code=PRCG20_2600000000&startDate=200001")
precos_importacao <- tibble(
  date = ym(boj_importacao$RESULTSET$VALUES$SURVEY_DATES[[1]]),
  preco_importacao_iene = boj_importacao$RESULTSET$VALUES$VALUES[[1]]
)

# 2.7. Cabinet Office: expectativa de preços em 1 ano, famílias com 2 ou mais pessoas.
# Média ponderada das faixas conforme Nakazono (BoJ WP 16-E-1, seção 2.1).
# https://www.boj.or.jp/en/research/wps_rev/wps_2016/data/wp16e01.pdf
# As caudas recebem -5% e +5% por hipótese; "não sei" é excluído e os pesos renormalizados.
# A pesquisa trata dos preços de compras frequentes das famílias, não especificamente do CPI core.
# Há descontinuidade em abril/2013 (correio) e mudança de coleta em outubro/2018 (online).
# Metódo do BoJ adotado
download.file("https://www.esri.cao.go.jp/en/stat/shouhi/shouhi3.xlsx", "data/shouhi3.xlsx", mode = "wb")

expectativas_cabinet_faixas <- read_excel("data/shouhi3.xlsx", sheet = 1, skip = 6, col_names = FALSE, col_types = "text") |>
  filter(str_detect(...1, "^[0-9]{10}$")) |>
  transmute(
    date = ym(paste(substr(...1, 1, 4), substr(...1, 7, 8))),
    queda_5_mais = as.numeric(...5), queda_2_5 = as.numeric(...6),
    queda_ate_2 = as.numeric(...7), estavel = as.numeric(...11),
    alta_ate_2 = as.numeric(...14), alta_2_5 = as.numeric(...15),
    alta_5_mais = as.numeric(...16), nao_sabe = as.numeric(...20)
  )

expectativa_cabinet <- expectativas_cabinet_faixas |>
  transmute(
    date,
    expectativa_1ano_cabinet = (
      -5 * queda_5_mais - 3.5 * queda_2_5 - queda_ate_2 +
        alta_ate_2 + 3.5 * alta_2_5 + 5 * alta_5_mais
    ) / (queda_5_mais + queda_2_5 + queda_ate_2 + estavel +
           alta_ate_2 + alta_2_5 + alta_5_mais)
  )

# 2.8. BEA: investimento em TI nos EUA, conforme Himino (BoJ, 27/08/2026, gráfico 4).
# Software + equipamentos de informação + data centers; trimestral, SAAR, trilhões de US$ de 2017.
# Soma analítica de componentes encadeados; não é um agregado oficial do BEA. Data centers: desde 2020 T1.
download.file("https://apps.bea.gov/national/Release/XLS/Survey/Section5All_xls.xlsx",
              "data/bea_investimento.xlsx", mode = "wb")
download.file("https://apps.bea.gov/national/Release/XLS/Underlying/Section5All_xls.xlsx",
              "data/bea_investimento_detalhado.xlsx", mode = "wb")

investimento_ti_eua <- map2_dfr(
  c("data/bea_investimento.xlsx", "data/bea_investimento_detalhado.xlsx"),
  c("T50306-Q", "U50406-Q"),
  ~ read_excel(.x, sheet = .y, skip = 7, col_types = "text") |>
    rename(codigo = 3) |>
    filter(codigo %in% c("Y034RX", "B985RX", "LB001282")) |>
    select(codigo, matches("^[0-9]{4}Q[1-4]$"))
) |>
  pivot_longer(-codigo, names_to = "date", values_to = "valor") |>
  mutate(date = yearquarter(date), valor = as.numeric(na_if(valor, ".....")) / 1e6) |>
  pivot_wider(names_from = codigo, values_from = valor) |>
  transmute(date, equipamentos = Y034RX, software = B985RX, data_centers = LB001282,
            investimento_ti = Y034RX + B985RX + LB001282) |>
  filter(!is.na(investimento_ti)) |>
  arrange(date)
