# Executar as etapas na mesma ordem do código original.
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")

source("src/1- data_collection.R")
source("src/2- data_cleaning.R")
source("src/3- feature_engineering.R")
source("src/4- data visualization.R")
source("src/models/var doméstico simples.R")
source("src/models/var externo simples.R")
source("src/models/bvar reduzido condicionado.R")
source("src/evaluation/cross_validation.R")
source("src/evaluation/holdout.R")
