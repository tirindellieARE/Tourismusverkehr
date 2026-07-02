library(haven)
raw <- read_sav("E:/ARE/ProjekteTIE/Turismusverkehr/RScript/tourists/data/TMS 2017.sav")

# Raw numeric values
cat("=== destination: raw numeric ===\n")
cat("Unique numeric codes:", length(unique(raw$destination)), "\n")
print(sort(unique(raw$destination)))

# Label values
dest_label <- as.character(as_factor(raw$destination))
cat("\n=== destination: label values ===\n")
cat("Unique labels:", length(unique(dest_label)), "\n")
print(head(sort(unique(dest_label)), 30))

cat("\nTop 10 by frequency:\n")
print(head(sort(table(dest_label), decreasing = TRUE), 10))
