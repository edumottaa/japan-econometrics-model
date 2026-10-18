suppressMessages(library(bsvarSIGNs))
ns <- asNamespace("bsvarSIGNs")
m <- readRDS("C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/mats.rds")

# Same steps as estimate.BSVARSIGN, stopping BEFORE importance_sampling() so we can see the weights
raw_draws <- function(spec, S) {
  prior <- spec$prior$get_prior()
  idn <- spec$identification$get_identification()
  dm <- spec$data_matrices$get_data_matrices()
  prior$B <- t(prior$A); prior$Ysoc <- t(prior$Ysoc); prior$Xsoc <- t(prior$Xsoc)
  prior$Ysur <- t(prior$Ysur); prior$Xsur <- t(prior$Xsur)
  sign <- idn$sign_irf; sign[is.na(sign)] <- 0
  struc <- idn$sign_structural; struc[is.na(struc)] <- 0
  .Call(get("_bsvarSIGNs_bsvar_sign_cpp", ns), S, spec$p, t(dm$Y), t(dm$X), sign,
        t(c(0, 1, 1, 1, 1, 1)), struc, get("get_Z", ns)(idn$sign_irf),
        spec$num_foreign_vars, prior, FALSE, 1, 0)
}

report <- function(label, spec, S = 200) {
  set.seed(123)
  q <- raw_draws(spec, S)
  w <- as.vector(q$posterior$w)
  cat("\n===", label, "===\n")
  cat("draws:", length(w), "| w = Inf:", sum(is.infinite(w)), "| w = 0:", sum(w == 0),
      "| NaN:", sum(is.nan(w)), "\n")
  fin <- w[is.finite(w) & w > 0]
  cat("log10(w) range (finite, >0):", signif(range(log10(fin)), 4), "\n")
  cat("ESS as the package computes it:", sum(w)^2 / sum(w^2), "\n")
  wn <- fin / max(fin)
  cat("ESS computed stably (finite weights):", round(sum(wn)^2 / sum(wn^2), 2), "of", length(fin), "\n")
  cat("share of total weight in the single largest draw:", signif(max(wn) / sum(wn), 4), "\n")
  set.seed(1)
  idx <- tryCatch(sample(seq_along(w), length(w), replace = TRUE, prob = w),
                  error = function(e) { cat("resampling error:", conditionMessage(e), "\n"); NULL })
  if (!is.null(idx)) cat("distinct draws left after resampling:", length(unique(idx)), "of", length(w), "\n")
}

report("SOE model (foreign = matrix_externa), default hyperparameters",
       specify_bsvarSIGN$new(data = m$int, p = 12, foreign = m$ext))
report("Same 10 variables WITHOUT foreign (no block restriction)",
       specify_bsvarSIGN$new(data = cbind(m$ext, m$int), p = 12))
