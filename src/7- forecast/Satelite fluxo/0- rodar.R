# Executor do satelite de fluxo: dados -> modelo satelite -> forecast com tilting -> graficos.
# Nao reestima o BVAR; usa os sorteios salvos pelo forecast de producao.
setwd('C:/Users/dudup/OneDrive/japan-econometrics-model')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6',.libPaths()))
saida <- 'src/Outputs/Satelite fluxo suave'
rodar_cv_satelite <- TRUE
source('src/1- data_collection/data_collection_sSpread.R',encoding='UTF-8')
source('src/2- data_cleaning/1- data_cleaning sSpread.R',encoding='UTF-8')
source('src/3- feature_engineering/1- feature_engineering sSpread.R',encoding='UTF-8')
source('src/5- models/Satelite fluxo/1- modelo satelite.R',encoding='UTF-8')
if(rodar_cv_satelite) source('src/6- evaluation/Satelite fluxo/1- cv satelite.R',encoding='UTF-8')
source('src/7- forecast/Satelite fluxo/1- condicionamento suave.R',encoding='UTF-8')
source('src/7- forecast/Satelite fluxo/2- forecast selecionado.R',encoding='UTF-8')
source('src/4- plot data/Satelite fluxo/1- graficos forecast.R',encoding='UTF-8')
