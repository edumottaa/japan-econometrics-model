suppressMessages(library(bsvarSIGNs))
ns <- asNamespace("bsvarSIGNs")
base <- "C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/"
m  <- readRDS(paste0(base, "mats.rds"))
r0 <- readRDS(paste0(base, "mode_base.rds"))

routines <- names(getDLLRegisteredRoutines("bsvarSIGNs")$.Call)
cat("log_ml registered:", "_bsvarSIGNs_log_ml" %in% routines, "\n")

spec <- specify_bsvarSIGN$new(data = m$int, p = 12, foreign = m$ext)
prior <- spec$prior$get_prior()
N <- 10; p <- 12

# Rebuild the two pieces of log_ml_dummy exactly as in sample_hyper.cpp
pieces <- function(h) {
  mu <- h[1]; delta <- h[2]; lambda <- h[3]; psi <- h[4:13]
  v <- diag(prior$V); v[1:(N*p)] <- lambda^2 * v[1:(N*p)] * rep(1/psi, p)
  Ystar <- rbind(t(prior$Ysoc) / mu, t(prior$Ysur) / delta)
  Xstar <- rbind(t(prior$Xsoc) / mu, t(prior$Xsur) / delta)
  Yd <- t(spec$prior$Y); Xd <- t(spec$prior$X)
  B <- t(prior$A); V <- diag(v); S <- diag(psi); nu <- prior$nu
  f <- get("_bsvarSIGNs_log_ml", ns)
  c(plus = .Call(f, B, V, S, nu, rbind(Ystar, Yd), rbind(Xstar, Xd)),
    star = .Call(f, B, V, S, nu, Ystar, Xstar))
}
h <- c(r0$mode, 1, 1, 1, 0.8)
for (v in c(93, 1, 1e-2, 1e-4, 2.55e-6, 1e-8)) {
  hh <- h; hh[6] <- v
  pc <- pieces(hh)
  cat(sprintf("psi_brent=%-9g log_ml_plus=%14.3f  log_ml_star=%14.3f  plus-star=%16.3f\n",
              v, pc["plus"], pc["star"], pc["plus"] - pc["star"]))
}

# Run the real sampler with almost no burn-in to see WHEN the chain falls into the trap
set.seed(123)
spec$estimate_hyper(S = 3000, burn_in = 1)
H <- spec$prior$hyper
lp <- function(x) .Call(get("_bsvarSIGNs_log_posterior_hyper", ns), matrix(x),
                        c(1,1,1,1,0), t(spec$prior$Y), t(spec$prior$X),
                        within(prior, {B <- t(A); Ysoc <- t(Ysoc); Xsoc <- t(Xsoc); Ysur <- t(Ysur); Xsur <- t(Xsur); covid <- 0}))
first_trap <- which(H[6, ] < 1e-3)[1]
cat("\nfirst iteration with psi_brent < 1e-3:", first_trap, "of", ncol(H), "\n")
cat("psi_brent at iterations 1,10,50,100,500,2999:", signif(H[6, c(1,10,50,100,500,2999)], 4), "\n")
cat("log posterior at iteration 1:", lp(H[,1]), " at last:", lp(H[, ncol(H)]), "\n")
cat("share of iterations where any parameter changed (after trap):",
    mean(colSums(abs(diff(t(H[, first_trap:ncol(H)])) > 0) > 0) > 0), "\n")
