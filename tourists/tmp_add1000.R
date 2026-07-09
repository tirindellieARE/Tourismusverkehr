suppressPackageStartupMessages(library(data.table))

iter <- fread("output/mnl_nalts1000_n10000_iterations.csv")
last <- iter[.N]
params <- setdiff(names(last), "logLike")

# LL0 for equal-share null: -N * log(C)
ll0  <- -10000 * log(1000)
ll   <- last$logLike
rho2 <- 1 - ll / ll0

# Read the last row's timing from iterations (not available, use NA)
est <- data.table(
  n_agents = 10000L,
  n_alts   = 1000L,
  param    = params,
  estimate = as.numeric(last[, ..params]),
  final_ll = ll,
  rho2     = rho2,
  seconds  = NA_real_
)

cat("N_ALTS=1000 estimates:\n")
print(est[param != "beta_tt_other", .(param, estimate = round(estimate, 5),
                                       final_ll = round(final_ll, 0), rho2 = round(rho2, 4))])

out_file <- "output/nalts_n10000_results.csv"
fwrite(est, out_file, append = TRUE)
cat("\nAppended to", out_file, "\n")
