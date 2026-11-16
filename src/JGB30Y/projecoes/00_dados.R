# 1. Bases recuperadas da versao original; taxas em % ao ano.
library(fpp3)
library(readr)
library(readxl)

dados <- file.path(pasta_curva, "dados")
saida <- file.path(pasta_curva, "resultados")
dir.create(saida, recursive = TRUE, showWarnings = FALSE)
variaveis <- c("jgb2", "jgb5", "jgb10", "jgb30")

# MoF: maturidade constante, nao um titulo especifico por ISIN.
# Amostra desde 2016, ultimo fechamento comum em cada mes completo.
# A coleta recuperada termina em out/2026; nao e uma atualizacao ate hoje.
historico <- read_csv(file.path(dados, "jgbcme_all_2026-11-04.csv"),
                      skip = 1, na = c("-", ""), show_col_types = FALSE) |>
  transmute(date = ymd(Date), jgb2 = `2Y`, jgb5 = `5Y`, jgb10 = `10Y`, jgb30 = `30Y`) |>
  filter(date >= ymd("2016-01-01"), date < floor_date(Sys.Date(), "month"),
         complete.cases(jgb2, jgb5, jgb10, jgb30)) |>
  arrange(date) |>
  group_by(mes = yearmonth(date)) |>
  summarise(date = last(date), across(all_of(variaveis), last), .groups = "drop") |>
  mutate(spread5_2 = jgb5 - jgb2, spread10_5 = jgb10 - jgb5,
         spread10_2 = jgb10 - jgb2, spread30_5 = jgb30 - jgb5,
         spread30_10 = jgb30 - jgb10, spread30_2 = jgb30 - jgb2)
origem <- slice_tail(historico, n = 1)

# Historico diario Bloomberg JYSO2 Curncy / PX_LAST, ja tratado na versao original.
ois_hist <- read_csv(file.path(dados, "ois2_historico.csv"), show_col_types = FALSE) |>
  filter(is.finite(ois2_pct)) |> arrange(date) |>
  group_by(mes = yearmonth(date)) |>
  summarise(ois2_pct = last(ois2_pct), .groups = "drop") |>
  filter(mes <= origem$mes)

# Snapshot Bloomberg: forwards mensais de TONA, nao OIS2Y observados no futuro.
# Datas americanas: 11/06/2026 = 6 de novembro de 2026.
ois <- read_xlsx(file.path(dados, "OIS jpy fwrd.xlsx"), skip = 1,
                 col_types = c("text", "numeric", "numeric")) |>
  transmute(inicio = mdy(Date), mes = yearmonth(inicio), forward = `Forward Rate`) |>
  arrange(mes)
stopifnot(all(is.finite(ois$forward)), !anyDuplicated(ois$mes),
          all(diff(as.numeric(ois$mes)) == 1), first(ois$mes) == origem$mes + 1L)

# 2. Quatro cenarios do BVAR atual: inclinacao = 30Y - 5Y (em p.p.).
arquivo_bvar <- file.path(fonte_bvar, "resumo_forecast.csv")
if (!file.exists(arquivo_bvar)) stop("Rode primeiro o forecast do BVAR 30Y-5Y.")
bvar <- read_csv(arquivo_bvar, show_col_types = FALSE) |>
  filter(variavel %in% c("juro_curto", "inclinacao")) |>
  select(cenario, variavel, mes = date, media) |>
  pivot_wider(names_from = variavel, values_from = media) |>
  mutate(mes = yearmonth(mes)) |> arrange(cenario, mes)
cenarios <- c("Base", "Benigno", "Adverso", "Adverso estrutural")
fim_projecao <- min(max(bvar$mes), max(ois$mes) - 24L)
meses <- seq(origem$mes + 1L, fim_projecao, by = 1L)
stopifnot(setequal(unique(bvar$cenario), cenarios), length(meses) > 0L,
          all(is.finite(bvar$juro_curto)), all(is.finite(bvar$inclinacao)),
          !anyDuplicated(bvar[c("cenario", "mes")]))
write_csv(historico, file.path(saida, "base_mensal.csv"))
