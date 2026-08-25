suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))

z <- st_read("data/input/zones_communes.gpkg", quiet = TRUE)
dt <- as.data.table(st_drop_geometry(z))
cat("Columns:", paste(names(dt), collapse = ", "), "\n")
cat("Rows:", nrow(dt), "\n\n")
cat("First rows:\n")
print(head(dt, 5))
cat("\nNO range:", min(dt$NO), "-", max(dt$NO), "\n")

# Compare to Benzoni zone_id and npvm_id
benz <- fread("../benzoni_thesis/output/attractivity_indexes.csv")
cat("\nBenzoni zone_id range:", min(benz$zone_id), "-", max(benz$zone_id), "\n")
cat("Benzoni npvm_id range:", min(benz$npvm_id), "-", max(benz$npvm_id), "\n")
cat("\nNO values in zones_communes that match Benzoni zone_id:", sum(dt$NO %in% benz$zone_id), "\n")
cat("NO values in zones_communes that match Benzoni npvm_id:", sum(dt$NO %in% benz$npvm_id), "\n")
