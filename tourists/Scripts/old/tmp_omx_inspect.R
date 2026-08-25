library(hdf5r)
library(data.table)

agents     <- fread("data/output/agents.csv")
sample_orig <- unique(agents[nationality != 1, origin_zone])[1]
sample_dest <- unique(agents$dest_zone)[1:5]

read_vals <- function(path, mat_name, orig, dests, no_path = "lookup/NO") {
  h   <- H5File$new(path, mode = "r")
  nos <- h[[no_path]][]
  ri  <- match(orig,  nos)
  ci  <- match(dests, nos)
  # Read full row then subset columns
  row <- h[[mat_name]][ri, ]
  h$close_all()
  row[ci]
}

ttc  <- read_vals("data/input/TTC.omx",  "data/131", sample_orig, sample_dest)
rita <- read_vals("data/input/RITA.omx", "data/141", sample_orig, sample_dest)
egt  <- read_vals("data/input/EGT.omx",  "data/143", sample_orig, sample_dest)
act  <- read_vals("data/input/ACT.omx",  "data/142", sample_orig, sample_dest)

cat(sprintf("Origin zone: %d\n", sample_orig))
cat(sprintf("Dest zones : %s\n\n", paste(sample_dest, collapse=", ")))
cat(sprintf("TTC (car)  : %s\n", paste(round(ttc,  1), collapse=", ")))
cat(sprintf("RITA (PT)  : %s\n", paste(round(rita, 1), collapse=", ")))
cat(sprintf("EGT  (PT)  : %s\n", paste(round(egt,  1), collapse=", ")))
cat(sprintf("ACT  (PT)  : %s\n", paste(round(act,  1), collapse=", ")))
cat(sprintf("PT total   : %s\n", paste(round(rita + egt + act, 1), collapse=", ")))
cat(sprintf("tt_avg(0.9/0.1): %s\n",
    paste(round(0.9 * ttc + 0.1 * (rita + egt + act), 1), collapse=", ")))
