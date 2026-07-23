# =============================================================================
# Simple mode choice model on the TMS 2023 survey: P(mode1 | mode2, nationality,
# commune).
#
# mode1, mode2      : {car, pt, other} -- individual-level covariates, not
#                      alternative-specific attributes (no travel time/cost
#                      data here), so this is a standard multinomial logistic
#                      regression (nnet::multinom), not an Apollo-style utility
#                      model with alternative-specific variables.
# nationality       : {Germany, France, Italy, other}
# commune           : 530 distinct communes (531 - 1 dropped: no data with
#                      NA/blank commune, 1,019 obs excluded)
#
# CAVEAT: commune is highly sparse -- median 3 obs/commune, 182/530 communes
# have exactly 1 observation, 333/530 have fewer than 5. A dummy for a
# singleton commune will drive that commune's coefficient toward +-Inf
# (quasi-complete separation) since it can perfectly "predict" its one
# observation. Those coefficients are not meaningfully estimated; they are
# kept in the output for completeness but should not be interpreted
# individually. mode2/nationality effects are unaffected by this.
#
# Reads:
#   data/TMS2023.csv
#
# Writes:
#   results_output/tms_mode_choice_estimates.csv -- full coefficient table
#     (mode1 level x predictor, incl. every commune dummy)
#   results_output/tms_mode_choice_model.rds     -- the fitted nnet::multinom object
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(nnet)
})

dir.create("results_output", showWarnings = FALSE)

tms <- fread("data/TMS2023.csv")
cat(sprintf("Loaded data/TMS2023.csv: %d rows\n", nrow(tms)))

n_before <- nrow(tms)
tms <- tms[!is.na(commune) & commune != ""]
cat(sprintf("Dropped %d rows with missing commune -> %d rows, %d distinct communes\n",
    n_before - nrow(tms), nrow(tms), uniqueN(tms$commune)))

# Reference levels: the largest category for each predictor, so the reference
# case is well-populated and the intercept is stable.
tms[, mode1       := relevel(factor(mode1),       ref = "pt")]
tms[, mode2       := relevel(factor(mode2),       ref = "pt")]
tms[, nationality := relevel(factor(nationality), ref = "other")]
ref_commune <- tms[, .N, by = commune][order(-N)][1, commune]
tms[, commune := relevel(factor(commune), ref = ref_commune)]
cat(sprintf("Reference levels: mode1/mode2 = pt, nationality = other, commune = %s\n\n", ref_commune))

model <- multinom(mode1 ~ mode2 + nationality + commune, data = tms, trace = FALSE, maxit = 500, MaxNWts = 5000)

# --- fit statistics ---------------------------------------------------------
null_model <- multinom(mode1 ~ 1, data = tms, trace = FALSE)
ll_model <- -model$value
ll_null  <- -null_model$value
mcfadden_r2 <- 1 - ll_model / ll_null
k <- length(coef(model))
aic <- -2 * ll_model + 2 * k

cat(sprintf("LL(model) = %.2f | LL(null) = %.2f | McFadden R2 = %.4f | AIC = %.1f | params = %d\n\n",
    ll_model, ll_null, mcfadden_r2, aic, k))

# --- coefficient table ------------------------------------------------------
est <- coef(model)      # rows = mode1 levels (excl. reference), cols = predictors
se  <- summary(model)$standard.errors

est_dt <- as.data.table(est, keep.rownames = "mode1_level")
se_dt  <- as.data.table(se,  keep.rownames = "mode1_level")

est_long <- melt(est_dt, id.vars = "mode1_level", variable.name = "param", value.name = "estimate")
se_long  <- melt(se_dt,  id.vars = "mode1_level", variable.name = "param", value.name = "se")

out <- merge(est_long, se_long, by = c("mode1_level", "param"))
out[, tstat := estimate / se]
out[, sig := fifelse(abs(tstat) >= 2.576, "***",
             fifelse(abs(tstat) >= 1.96,  "**",
             fifelse(abs(tstat) >= 1.645, "*", "")))]
out[, is_commune := startsWith(as.character(param), "commune")]
setorder(out, mode1_level, is_commune, param)

fwrite(out, "results_output/tms_mode_choice_estimates.csv")
saveRDS(model, "results_output/tms_mode_choice_model.rds")

cat("=== mode2 / nationality coefficients (reference: mode1 = pt) ===\n")
print(out[is_commune == FALSE, .(mode1_level, param, estimate = round(estimate, 4),
                                  se = round(se, 4), tstat = round(tstat, 2), sig)])

cat(sprintf("\nFull coefficient table (incl. %d commune dummies x %d mode1 levels) saved to results_output/tms_mode_choice_estimates.csv\n",
    uniqueN(tms$commune) - 1, uniqueN(tms$mode1) - 1))
cat("Model object saved to results_output/tms_mode_choice_model.rds\n")
