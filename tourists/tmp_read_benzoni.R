library(pdftools)
txt <- pdf_text("tmp_benzoni_paper.pdf")
cat("Total pages:", length(txt), "\n\n")
for (i in seq_along(txt)) {
  cat(sprintf("=== PAGE %d ===\n", i))
  cat(txt[[i]])
  cat("\n")
}
