# Executar as etapas na mesma sessão, em ordem sequencial.
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")

# 1. Dados e visualização
source("src/1- data_collection.R")
source("src/2- data_cleaning.R")
source("src/3- feature_engineering.R")
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
# demanda global não é identificado separadamente na especificação de sete variáveis.
source("src/Outputs/IRF e FEVD/IRF (Bvar estrutural - bvarsigns)/Códigos/IRF choque oferta petroleo.R")
source("src/Outputs/IRF e FEVD/IRF (Bvar estrutural - bvarsigns)/Códigos/IRF choque politica Fed.R")
# risk off global não é identificado separadamente na especificação de sete variáveis.


# 6. Avaliação: CVs pausados; descomentar somente quando quiser retomar.
source("src/evaluation/corss validation BVAR reduzido pacote bvarsigns.R")
source("src/evaluation/cross validation BVAR reduzido condicionado.R")
source("src/evaluation/cross_validation_s2.R")

