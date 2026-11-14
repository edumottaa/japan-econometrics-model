# Coleta das variáveis complementares: PIB real trimestral do Japão.

library(tidyverse)
library(rvest)

setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")

# 1. Localizar a divulgação mais recente do PIB no Cabinet Office
pagina_pib <- "https://www.esri.cao.go.jp/en/sna/sokuhou/sokuhou_top.html"
links_pib <- read_html(pagina_pib) |> html_elements("a")
url_divulgacao_pib <- links_pib |>
  keep(~ str_detect(html_text2(.x), "Time series table")) |>
  map_chr(~ html_attr(.x, "href")) |>
  xml2::url_absolute(pagina_pib)
stopifnot(length(url_divulgacao_pib) == 1L)

links_series_pib <- read_html(url_divulgacao_pib) |> html_elements("a")
url_pib <- links_series_pib |> html_attr("href") |>
  keep(~ !is.na(.x) && str_detect(.x, "/gaku-jk[0-9]+[.]csv$")) |>
  unique() |>
  xml2::url_absolute(url_divulgacao_pib)
stopifnot(length(url_pib) == 1L)

# 2. Guardar o arquivo original; a limpeza fica na etapa seguinte
dir.create("data/raw/variaveis complementares", recursive = TRUE, showWarnings = FALSE)
dir.create("data/cache", recursive = TRUE, showWarnings = FALSE)
arquivo_pib <- "data/raw/variaveis complementares/pib_real_trimestral.csv"
download.file(url_pib, arquivo_pib, mode = "wb")

fonte_pib <- tibble(fonte = "Cabinet Office - Quarterly Estimates of GDP",
  serie = "PIB real, ajuste sazonal, nivel oficial em bilhoes de ienes encadeados",
  pagina = url_divulgacao_pib, arquivo = url_pib,
  coleta = as.character(Sys.Date()), md5 = unname(tools::md5sum(arquivo_pib)))
saveRDS(fonte_pib, "data/cache/fonte_pib.rds")
fonte_pib
