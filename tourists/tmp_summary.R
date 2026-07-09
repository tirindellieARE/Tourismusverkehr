library(data.table)
dt <- fread("output/bootstrap_results.csv")
cat("Samples completed:", uniqueN(dt$sample), "\n\n")

dt_free <- dt[param != "beta_tt_other"]

summ <- dt_free[, .(
  mean   = round(mean(estimate),   5),
  sd     = round(sd(estimate),     5),
  min    = round(min(estimate),    5),
  median = round(median(estimate), 5),
  max    = round(max(estimate),    5)
), by = param]

cat("=== Coefficient summary across 10 samples ===\n")
print(summ)

cat("\nFinal LL per sample:\n")
ll <- unique(dt[, .(sample, final_ll)])
print(ll[order(sample)])
cat(sprintf("\nMean LL: %.2f  SD: %.2f\n", mean(ll$final_ll), sd(ll$final_ll)))
