# Script aims to run the short binary sequences

suppressWarnings(suppressMessages(library(tseries)))
analyze_bits <- function(bits, h = NULL, alpha = 0.05) {
  n <- length(bits)
  if (is.null(h)) h <- max(1, floor(10 * log10(n)))  # your default
  cat(sprintf("Testing %d bits\n", n))
  
  counts <- table(factor(bits, levels = 0:1))
  chi <- suppressWarnings(chisq.test(counts, p = c(0.5, 0.5)))
  cat("\nMonobit Chi-square p:", chi$p.value, "\n")
  
  runs <- runs.test(factor(bits))
  cat("Runs test p:", runs$p.value, "\n")
  
  lb <- Box.test(ts(bits), lag = h, type = "Ljung-Box")
  cat(sprintf("Ljung–Box (lag=%d) p: %g\n", h, lb$p.value))
}

# ---- use it on your vector:
#x <- c(1,0,1,1,0,0,1,0,1,1, 1,0,0,1,0,1,0,0,1,1)
#x <- c(1,0,1,1,0,1,0,0, 1,0,1,1,0,1,0,0, 1,1,1,0,0,1,0,1, 1,0,0,1,0,1)
x <- c(1,0,0,1,1,0,1,0,1,1)
analyze_bits(x, h = 2)   # h=2 to match the manual example
