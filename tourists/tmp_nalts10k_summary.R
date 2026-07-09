suppressPackageStartupMessages(library(data.table))
dt <- fread("output/nalts_n10000_results.csv")
dt <- unique(dt, by = c("n_alts", "param"))
dt <- dt[order(n_alts)]

cat("=== Fit by N_ALTS (10,000 agents, seed=42) ===\n\n")
fit <- unique(dt[, .(n_alts, final_ll = round(final_ll, 0),
                      rho2 = round(rho2, 4), seconds = round(seconds, 0))])
print(fit[order(n_alts)])

cat("\n=== Travel-time coefficients by N_ALTS ===\n\n")
tt <- dcast(dt[param %in% c("beta_tt_DE","beta_tt_AT","beta_tt_FR","beta_tt_IT")],
            param ~ n_alts, value.var = "estimate", fun.aggregate = mean)
cols <- as.character(sort(unique(dt$n_alts)))
tt[, (cols) := lapply(.SD, function(x) round(x, 5)), .SDcols = cols]
print(tt)

cat("\n=== Topology coefficients by N_ALTS ===\n\n")
tp <- dcast(dt[param %in% c("beta_topo2","beta_topo3")],
            param ~ n_alts, value.var = "estimate", fun.aggregate = mean)
tp[, (cols) := lapply(.SD, function(x) round(x, 4)), .SDcols = cols]
print(tp)

cat("\n=== % change relative to N_ALTS=300 ===\n\n")
base <- dt[n_alts == 300, .(param, base = estimate)]
pct  <- merge(dt[param != "beta_tt_other"], base, by = "param")
pct[, delta_pct := round(100 * (estimate - base) / abs(base), 1)]
wide_pct <- dcast(pct, param ~ n_alts, value.var = "delta_pct")
print(wide_pct)
