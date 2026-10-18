suppressMessages(library(bsvarSIGNs))
base <- "C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/"
m <- readRDS(paste0(base, "mats.rds"))
dados <- cbind(m$ext, m$int)
N <- 10; p <- 12; h <- 12
spec <- specify_bsvarSIGN$new(data = dados, p = 12)
set.seed(123)
post <- estimate(spec, S = 200, show_progress = FALSE)
A <- post$posterior$A; Sigma <- post$posterior$Sigma

x0 <- c(t(dados[nrow(dados):(nrow(dados) - p + 1), ]), 1)

# (1) caminho com choque = 0, por sorteio
zero_path <- function(s) {
  x <- x0; out <- matrix(NA, h, N)
  for (j in 1:h) { y <- A[, , s] %*% x; out[j, ] <- y; x <- c(y, x[1:(N * (p - 1))], 1) }
  out
}
# (2) caminho com choque sorteado
sim_path <- function(s) {
  x <- x0; L <- t(chol(Sigma[, , s])); out <- matrix(NA, h, N)
  for (j in 1:h) { y <- A[, , s] %*% x + L %*% rnorm(N); out[j, ] <- y; x <- c(y, x[1:(N * (p - 1))], 1) }
  out
}
icpi <- which(colnames(dados) == "cpi_core")

# A) media de 200 caminhos simulados (1 por sorteio) vs media dos 200 caminhos com choque = 0
set.seed(1)
sim200  <- sapply(1:200, function(s) sim_path(s)[, icpi])
zero200 <- sapply(1:200, function(s) zero_path(s)[, icpi])
cat("A) cpi_core, media entre 200 sorteios\n")
print(round(rbind(com_choque = rowMeans(sim200), choque_zero = rowMeans(zero200),
                  diferenca = rowMeans(sim200) - rowMeans(zero200)), 3))

# B) 1 sorteio fixo: formula analitica vs simulacao
s <- 1
Ac <- A[, , s][, 1:(N * p)]
comp <- rbind(Ac, cbind(diag(N * (p - 1)), matrix(0, N * (p - 1), N)))
S1 <- Sigma[, , s]
V <- matrix(0, N, N); M <- diag(N * p); sd_formula <- numeric(h)
for (j in 1:h) {
  Psi <- M[1:N, 1:N]
  V <- V + Psi %*% S1 %*% t(Psi)
  sd_formula[j] <- sqrt(V[icpi, icpi])
  M <- comp %*% M
}
set.seed(2)
sim_fixo <- sapply(1:20000, function(i) sim_path(s)[, icpi])
cat("\nB) cpi_core, 1 sorteio fixo de coeficientes: desvio padrao da projecao\n")
print(round(rbind(formula = sd_formula, simulacao = apply(sim_fixo, 1, sd)), 3))

# C) banda total (200 sorteios de coeficientes) vs banda de 1 sorteio
set.seed(3)
sim_todos <- do.call(cbind, lapply(1:200, function(s) sapply(1:50, function(i) sim_path(s)[, icpi])))
cat("\nC) desvio padrao: 1 sorteio fixo vs todos os sorteios (inclui incerteza dos coeficientes)\n")
print(round(rbind(um_sorteio = apply(sim_fixo, 1, sd), todos = apply(sim_todos, 1, sd)), 3))

# D) so incerteza dos coeficientes (choque = 0 em todos)
cat("\nD) desvio padrao entre os 200 caminhos com choque = 0 (so incerteza dos coeficientes)\n")
print(round(apply(zero200, 1, sd), 3))
