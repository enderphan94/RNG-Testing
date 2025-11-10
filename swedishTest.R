# Usage: Rscript swedishTest.R <file.bin>
# Requires: install.packages("tseries")

suppressWarnings(suppressMessages(library(tseries)))

# Read file as raw bytes -> expand to bits (MSB-first)
read_bits_msb <- function(file_path) {
  nbytes <- file.info(file_path)$size
  con <- file(file_path, "rb")
  on.exit(close(con))
  r <- readBin(con, what = "raw", n = nbytes)
  b <- as.integer(rawToBits(r))       # LSB-first, 8 bits per byte
  dim(b) <- c(8, length(r))           # 8 rows x nbytes cols
  b <- b[8:1, , drop = FALSE]         # flip to MSB-first per byte
  as.vector(b)                        # column-wise vector of 0/1
}


main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1) stop("Usage: Rscript swedishTest.R <file.bin>")
  file_path <- args[1]
  
  bits <- read_bits_msb(file_path)
  n <- length(bits)
  cat(sprintf("Read %d bits (MSB-first)\n", n))
  
  # --- 1) Monobit Chi-square test (0/1 equally likely) ---
  counts <- table(factor(bits, levels = 0:1))
  chi <- suppressWarnings(chisq.test(counts, p = c(0.5, 0.5)))
  p_chi <- chi$p.value
  cat("\nMonobit Chi-square:\n")
  print(chi)

  # --- 2) Wald–Wolfowitz Runs test on the bit sequence ---
  # tseries::runs.test accepts a two-level factor for a dichotomous sequence
  runs <- runs.test(factor(bits))
  p_runs <- runs$p.value
  cat("\nRuns test (Wald–Wolfowitz) on bits:\n")
  print(runs)

  # --- 3) Ljung–Box test on the bit sequence ---
  h <- max(1, floor(10 * log10(n)))
  lb <- Box.test(ts(bits), lag = h, type = "Ljung-Box")
  p_lb <- lb$p.value
  cat(sprintf("\nLjung–Box (lag=%d) on bits:\n", h))
  print(lb)
}

main()
