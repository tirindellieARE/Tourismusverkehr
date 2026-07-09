library(pdftools)
txt <- pdf_text("../activity_simulation/Scherr_et_al2020.pdf")
cat("Total pages:", length(txt), "\n\n")
for (i in seq_along(txt)) {
  cat(sprintf("=== PAGE %d ===\n", i))
  cat(txt[[i]])
  cat("\n")
}
