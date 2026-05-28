library(haven)
library(dplyr)

raw         <- read_sav("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists/data/TMS 2017.sav")
commune_ref <- read.csv("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists/data/commune_type_region.csv",
                        stringsAsFactors = FALSE, encoding = "UTF-8")

tms_foreign <- raw %>%
  filter(!is.na(means_of_transport_in_CH)) %>%
  mutate(commune_name_tms = as.character(as_factor(destination)))

unmatched <- tms_foreign %>%
  anti_join(commune_ref, by = c("commune_name_tms" = "commune_name")) %>%
  count(commune_name_tms, sort = TRUE)

cat("All unmatched commune names (n =", nrow(unmatched), "):\n")
print(unmatched, n = Inf)

# Also print all CSV commune names for reference when building the lookup
cat("\n\nAll CSV commune names (sorted):\n")
cat(paste(sort(commune_ref$commune_name), collapse = "\n"), "\n")
