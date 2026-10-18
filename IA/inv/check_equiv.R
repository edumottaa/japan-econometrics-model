suppressMessages({library(tidyverse); library(fpp3); library(bsvarSIGNs)})
base <- "C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/"
m <- readRDS(paste0(base, "inv/mats.rds"))
data_bal <- as_tsibble(tibble(date = yearmonth("2000 Jan") + 0:(nrow(m$ext) - 1), as_tibble(cbind(m$ext, m$int))), index = date)
src  <- readLines(paste0(base, "model_clean_comments_corrigido.R"), encoding = "UTF-8")
code <- src[grep("^# 5 Build the model data matrix", src):grep("^bv      <- make_bvar", src)]
eval(parse(text = code))                     # dados, spec, make_bvar, bv

h_def <- spec$prior$hyper[1:13, 1]
set.seed(1); pkg <- estimate(spec, S = 400, show_progress = FALSE)
set.seed(2); ours <- lapply(1:4000, function(i) bv$draw(h_def))
A_pkg  <- pkg$posterior$A                                   # N x K x S
A_ours <- simplify2array(lapply(ours, function(d) t(d$B)))  # N x K x S
m_pkg <- apply(A_pkg, c(1, 2), mean);  v_pkg <- apply(A_pkg, c(1, 2), var)
m_our <- apply(A_ours, c(1, 2), mean); v_our <- apply(A_ours, c(1, 2), var)
z <- (m_pkg - m_our) / sqrt(v_pkg / dim(A_pkg)[3] + v_our / dim(A_ours)[3])
K <- dim(A_pkg)[2]
cat("largest abs difference is in column", which(abs(m_pkg - m_our) == max(abs(m_pkg - m_our)), arr.ind = TRUE)[, 2],
    "of", K, "(column", K, "= intercept)\n")
cat("posterior sd of intercepts (ours):", round(sqrt(v_our[, K]), 1), "
")
cat("z-scores of the difference (1210 coefficients): max |z| =", round(max(abs(z)), 2),
    "| share |z| > 1.96 =", round(mean(abs(z) > 1.96), 3), "(about 0.05 if both samplers agree)\n")
