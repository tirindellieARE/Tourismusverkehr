suppressPackageStartupMessages(library(data.table))
dt <- fread("output/nalts_results.csv")

# deduplicate (N_ALTS=500 ran twice due to overlapping runs; estimates identical)
dt <- unique(dt, by = c("n_alts", "param"))

cat("=== Fit statistics by number of alternatives (5000 agents, seed=42) ===\n\n")
fit <- unique(dt[, .(n_alts, final_ll = round(final_ll, 1),
                      rho2 = round(rho2, 4), seconds)])
print(fit[order(n_alts)])

cat("\n=== Coefficients by N_ALTS (excluding fixed beta_tt_other) ===\n\n")
wide <- dcast(dt[param != "beta_tt_other"], param ~ n_alts,
              value.var = "estimate", fun.aggregate = mean)
cols <- as.character(sort(unique(dt$n_alts)))
wide[, (cols) := lapply(.SD, function(x) round(x, 5)), .SDcols = cols]
print(wide)
