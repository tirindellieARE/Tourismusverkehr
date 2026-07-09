suppressPackageStartupMessages(library(data.table))

# Check what Apollo saved
f <- "output/mnl_nalts100.RData"
if (file.exists(f)) {
  load(f)
  cat("LLout:", model$LLout, "\n")
  cat("LL0out:", model$LL0out, "\n")
  cat("names:", paste(names(model$estimate), collapse=", "), "\n")
  cat("values:", round(as.numeric(model$estimate), 5), "\n")
} else {
  cat("No RData file found\n")
  cat("Files in output/:", paste(list.files("output/"), collapse="\n"), "\n")
}
