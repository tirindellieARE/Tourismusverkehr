suppressPackageStartupMessages(library(data.table))

# 5000-agent result at 300 alts (from old nalts_results.csv)
old <- fread("results_output/nalts_results.csv")
old <- unique(old, by = c("n_alts", "param"))
dt5k <- old[n_alts == 300]

# 10000-agent result: extract from iterations file (last row = converged estimates)
iter10k <- fread("results_output/mnl_nalts300_n10000_iterations.csv")
last    <- iter10k[.N]
params  <- setdiff(names(last), "logLike")
dt10k   <- data.table(
  n_alts   = 300,
  param    = params,
  estimate = as.numeric(last[, ..params]),
  final_ll = last$logLike
)

# Fit comparison (get rho2 from iterations: LL(0) = -N*log(C), C = 300)
# LL0 = -N * log(300) for equal-share null
ll0_5k  <- -5000  * log(300)
ll0_10k <- -10000 * log(300)

cat("=== Fit comparison: 300 alternatives ===\n\n")
cat(sprintf("  N=5000:  LL=%.1f  rho2=%.4f  (LL0=%.1f)\n",
            dt5k$final_ll[1], 1 - dt5k$final_ll[1] / ll0_5k, ll0_5k))
cat(sprintf("  N=10000: LL=%.1f  rho2=%.4f  (LL0=%.1f)\n\n",
            dt10k$final_ll[1], 1 - dt10k$final_ll[1] / ll0_10k, ll0_10k))

cat("=== Coefficients: 5000 vs 10000 agents (300 alts) ===\n\n")
coef5  <- dt5k[param  != "beta_tt_other", .(param, est_5k  = round(estimate, 5))]
coef10 <- dt10k[param != "beta_tt_other", .(param, est_10k = round(estimate, 5))]
comp   <- merge(coef5, coef10, by = "param")
comp[, diff_pct := round(100 * (est_10k - est_5k) / abs(est_5k), 1)]
print(comp[order(param)])
