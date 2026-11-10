library(readxl)
setwd("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/data/PASTA_HESTA")

sheet <- 1   # or the sheet name, e.g. "GEO Codes" (your tab name looks cut off)
path = "PASTA_HESTA.xlsx"
# 1. Find where the header "ID" is
raw <- read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text")
pos <- which(raw == "ID", arr.ind = TRUE)[1, ]
hdr_row <- pos[1]; first_col <- pos[2]

measure <- unlist(raw[hdr_row,     first_col:ncol(raw)])   # ID, Name, ..., Ankünfte, Logiernächte
country <- unlist(raw[hdr_row - 1, first_col:ncol(raw)])   # Ägypten, NA, Argentinien, NA, ...

# 2. Fill each country across its pair of columns
is_meas <- measure %in% c("Ankünfte", "Logiernächte")
country[!is_meas] <- NA
country <- tidyr::fill(data.frame(x = country), x)$x

# 3. Clean names: Ä -> Ae, ü -> ue, é -> e, spaces -> _
clean <- function(x) {
  x <- stringi::stri_trans_general(x, "de-ASCII; Latin-ASCII")
  gsub("[^A-Za-z0-9]+", "_", x)
}
new_names <- ifelse(is_meas,
                    paste(clean(country), clean(measure), sep = "_"),
                    clean(measure))
new_names[new_names == "Koordinate"] <- c("E", "N")   # the two LV95 columns

# 4. Read the data (everything below the header) and apply the names
df <- read_excel(path, sheet = sheet, col_names = FALSE,
                 range = cellranger::cell_limits(
                   c(hdr_row + 1, first_col),
                   c(NA, first_col + length(new_names) - 1)))
names(df) <- new_names

# 5. Pivot longer: one row per hotel x country, with Ankuenfte and Logiernaechte as columns
library(tidyr)
library(dplyr)
long <- df |>
  pivot_longer(
    cols = matches("_(Ankuenfte|Logiernaechte)$"),
    names_to = c("Land", ".value"),
    names_pattern = "(.*)_(Ankuenfte|Logiernaechte)"
  ) |>
  filter(!(is.na(Ankuenfte) & is.na(Logiernaechte)))   # drop empty country rows

head(long)
