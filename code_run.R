# Executar as etapas na mesma sessão, em ordem sequencial.
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")

# 1. Dados e visualização
source("src/1- data_collection/1- data_collection bvar estrutural.R")
source("src/1- data_collection/2- data_collection s2.R")
source("src/2- data_cleaning/1- data_cleaning bvar estrutural.R")
source("src/2- data_cleaning/2- data_cleaning s2.R")
source("src/3- feature_engineering/1- feature_engineering bvar estrutural.R")
source("src/3- feature_engineering/2- feature_engineering s2.R")
source("src/4- data visualization.R")

# 2. VARs de referência; o VAR externo é .
source("src/models/Var models/var doméstico simples.R") #var doméstico como comparação
source("src/models/Var models/var externo simples.R")   #necessário ao BVAR condicionado

# 3. BVARs reduzidos
source("src/models/Reduced BVAR models/Bvar reduzido (pacote Bvar signs).R")     
source("src/models/Reduced BVAR models/bvar reduzido condicionado (Patote BVAR).R")

# 4. BVAR estrutural: sete variáveis, p=6 e sete choques; CV ex post
source("src/5- models/Structural BVAR models/Bvar estrutural choque BoJ (pacote Bvar signs).R")

# 5. IRFs: executar após estimar o modelo estrutural (post_boj).
source("src/Outputs/IRF e FEVD/IRF (Bvar estrutural - bvarsigns)/Códigos/IRF choque BoJ.R")
source("src/Outputs/IRF e FEVD/IRF (Bvar estrutural - bvarsigns)/Códigos/IRF choque demanda domestica.R")
source("src/Outputs/IRF e FEVD/IRF (Bvar estrutural - bvarsigns)/Códigos/IRF choque oferta domestica.R")
source("src/Outputs/IRF e FEVD/IRF (Bvar estrutural - bvarsigns)/Códigos/IRF choque premio fiscal.R")
source("src/Outputs/IRF e FEVD/IRF (Bvar estrutural - bvarsigns)/Códigos/IRF choque oferta petroleo.R")
source("src/Outputs/IRF e FEVD/IRF (Bvar estrutural - bvarsigns)/Códigos/IRF choque politica Fed.R")

# 6. Evaluation: testes CV
source("src/evaluation/corss validation BVAR reduzido pacote bvarsigns.R")
source("src/evaluation/cross validation BVAR reduzido condicionado.R")
source("src/evaluation/cross_validation_s2.R")

