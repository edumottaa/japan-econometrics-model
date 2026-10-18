suppressMessages(library(bsvarSIGNs))
base <- "C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/"
ns <- asNamespace("bsvarSIGNs")
m  <- readRDS(paste0(base, "mats.rds"))

# Load the ChatGPT functions exactly as written (lines 185-338 of its script)
src <- readLines("C:/Users/dudup/OneDrive/NT Macro/model_psi_corrigido.R", encoding = "UTF-8")
eval(parse(text = src[185:338]))

spec <- specify_bsvarSIGN$new(data = m$int, p = 12, foreign = m$ext,
                              sign_structural = matrix(NA_real_, 10, 10))

# 1) Its QR log posterior vs the package's log posterior
gpt_lp <- stable_hyper_target(spec, FALSE)
pr <- spec$prior$get_prior()
pr$B <- t(pr$A); pr$Ysoc <- t(pr$Ysoc); pr$Xsoc <- t(pr$Xsoc); pr$Ysur <- t(pr$Ysur); pr$Xsur <- t(pr$Xsur); pr$covid <- 0
pkg_lp <- function(h) .Call(get("_bsvarSIGNs_log_posterior_hyper", ns), matrix(c(h, 1, 1, 1, 0.8)),
                            c(1, 1, 1, 1, 0), t(spec$prior$Y), t(spec$prior$X), pr)
mine <- readRDS(paste0(base, "hyper_draws_valid.rds"))          # 13 x 10000, my corrected sampler
set.seed(1)
pts <- mine[, sample(ncol(mine), 20)]
d <- apply(pts, 2, function(h) gpt_lp(c(h, 1, 1, 1, 0.8)) - pkg_lp(h))
cat("valid points: GPT minus package log posterior, max abs diff:", signif(max(abs(d)), 3), "\n")
trap <- c(0.0422, 0.07126, 0.2192, 0.418, 3.213, 2.555e-06, 96.20, 8.851, 0.7116, 0.009206, 0.04528, 0.3482, 2.081)
cat("trapped point: package =", pkg_lp(trap), "| GPT =", gpt_lp(c(trap, 1, 1, 1, 0.8)), "\n")

summ <- function(H, rows) round(t(apply(H[rows, , drop = FALSE], 1, quantile, c(.05, .5, .95))), 4)
nm <- c("mu", "delta", "lambda", paste0("psi_", c(colnames(m$ext), colnames(m$int))))

# 2) Its sampler without COVID, compared with mine
set.seed(123)
t1 <- system.time(capture.output(estimar_hyper_corrigido(spec, S = 15000, burn_in = 5000)) -> log1)
cat("\n--- GPT sampler, no COVID (", round(t1["elapsed"]), "s ) ---\n"); cat(grep("Taxa|Menor", log1, value = TRUE), sep = "\n")
H1 <- spec$prior$hyper; rownames(H1) <- c(nm, "s0", "s1", "s2", "rho")
cmp <- cbind(summ(H1, 1:13), mine_median = round(apply(mine, 1, median), 4))
rownames(cmp) <- nm; print(cmp)

# 3) Its sampler with COVID, exactly as in its script
covid_idx <- 243 - 12   # March 2020 = row 243 of the data (Jan 2000 = 1)
spec_c <- specify_bsvarSIGN$new(data = m$int, p = 12, foreign = m$ext,
                                sign_structural = matrix(NA_real_, 10, 10), hyper_covid = covid_idx)
set.seed(123)
t2 <- system.time(capture.output(estimar_hyper_corrigido(spec_c, S = 15000, burn_in = 5000, inicio_covid = covid_idx)) -> log2)
cat("\n--- GPT sampler, with COVID (", round(t2["elapsed"]), "s ) ---\n"); cat(grep("Taxa|Menor", log2, value = TRUE), sep = "\n")
H2 <- spec_c$prior$hyper; rownames(H2) <- c(nm, "s0_mar20", "s1_apr20", "s2_may20", "rho")
print(summ(H2, c(1:3, 14:17)))
saveRDS(H2, paste0(base, "gpt_hyper_covid.rds"))

# 4) What happens next in its pipeline: estimate() with foreign
res <- tryCatch({ set.seed(1); p <- estimate(spec_c, S = 200, show_progress = FALSE); paste("ran; ESS =", p$posterior$ess) },
                error = function(e) paste("error:", conditionMessage(e)))
cat("\nestimate(spec, S = 200) after its hyperparameters:", res, "\n")
