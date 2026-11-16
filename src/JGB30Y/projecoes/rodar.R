# Executar a partir da raiz do projeto, depois do forecast do BVAR.
# Nao coleta dados nem reestima o BVAR. Reestima apenas as regressoes da curva.
.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
if (!exists("fonte_bvar")) fonte_bvar <- "src/Outputs/Satelite fluxo suave"
pasta_curva <- "src/JGB30Y/projecoes"

source(file.path(pasta_curva, "00_dados.R"), encoding = "UTF-8")
source(file.path(pasta_curva, "01_jgb2.R"), encoding = "UTF-8")
source(file.path(pasta_curva, "02_spreads.R"), encoding = "UTF-8")
source(file.path(pasta_curva, "03_curva.R"), encoding = "UTF-8")
source(file.path(pasta_curva, "04_graficos.R"), encoding = "UTF-8")
