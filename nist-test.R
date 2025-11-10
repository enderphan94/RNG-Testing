#!/usr/bin/env Rscript
# NIST SP 800-22r1a p-values (byte-accurate)
# Tests: Monobit, Runs, Block Frequency (M), Approximate Entropy (m),
#        Cumulative Sums (forward & reverse)
# Defaults mirror: ./assess 1000000 with 100 bitstreams.
# Usage
# ./script <file.bin> --n=1000000 --seq=100 ("--n=1000000 --seq=100" is default setting)
# ./script -test "1100100100001111110110101010001000100001011010001100001000110100110001001100011001100010100010111000" --M-block=8 --m-apen=4
#Note that blockfreq test with M=10, ApproxEntropy with M is null, --m-apen=2 (follow the sample size test in Guideline)

# ---------- bytes -> bits (NIST: MSB->LSB within each BYTE) ----------
raw_bytes_to_bits <- function(rawv, msb_first=TRUE) {
  if (length(rawv) == 0) return(integer(0))
  b <- rawToBits(rawv)           # 8 x nbytes, LSB..MSB per byte
  dim(b) <- c(8, length(rawv))
  if (msb_first) b <- b[8:1, , drop=FALSE]
  as.integer(b)
}

# ------------------ per-sequence p-values ------------------

# Monobit 
p_monobit <- function(bits) {
  n <- length(bits)
  S <- sum(ifelse(bits == 1L, 1, -1))
  2 * pnorm(-abs(S) / sqrt(n))         # erfc(|S|/sqrt(2n))
}

# Runs 
p_runs <- function(bits) {
  n <- length(bits); pi_hat <- mean(bits)
  if (abs(pi_hat - 0.5) >= 2 / sqrt(n)) return(0.0)    # frequency guard
  Vn <- 1L + sum(bits[-1L] != bits[-n])
  z  <- abs(Vn - 2 * n * pi_hat * (1 - pi_hat)) /
    (2 * sqrt(2 * n) * pi_hat * (1 - pi_hat))
  2 * pnorm(-z * sqrt(2))                               # erfc(z)
}

# Block Frequency 
p_blockfreq <- function(bits, M) {
  n <- length(bits); N <- floor(n / M)
  if (N < 1) stop("Block Frequency: n < M")
  x <- bits[seq_len(N * M)]
  blocks <- matrix(x, nrow = M, ncol = N)
  pis <- colMeans(blocks)
  chisq <- 4 * M * sum((pis - 0.5)^2)
  pchisq(chisq, df = N, lower.tail = FALSE)
}

# Approximate Entropy 
counts_m <- function(bits, m) {
  n <- length(bits)
  if (m == 0) return(c(n))
  k <- bitwShiftL(1L, m)           # 2^m
  x <- c(bits, bits[seq_len(m-1)]) # wrap
  counts <- integer(k)
  acc <- 0L
  for (j in 1:m) acc <- bitwOr(bitwShiftL(acc, 1L), x[j])
  counts[acc + 1L] <- counts[acc + 1L] + 1L
  mask <- k - 1L
  for (i in 2:n) {
    acc <- bitwAnd(bitwShiftL(acc, 1L), mask)
    acc <- bitwOr(acc, x[i + m - 1L])
    counts[acc + 1L] <- counts[acc + 1L] + 1L
  }
  counts
}
phi_m <- function(counts, n) { p <- counts / n; p <- p[p > 0]; sum(p * log(p)) }
p_apen <- function(bits, m) {
  n <- length(bits)
  cm  <- counts_m(bits, m)
  cm1 <- counts_m(bits, m + 1L)
  ApEn <- phi_m(cm, n) - phi_m(cm1, n)
  chisq <- 2 * n * (log(2) - ApEn)
  pchisq(chisq, df = bitwShiftL(1L, m), lower.tail = FALSE)
}

# Cumulative Sums (Cusum) — forward or reverse
p_cusum <- function(bits, reverse=FALSE) {
  y <- if (reverse) rev(2L*bits - 1L) else (2L*bits - 1L)
  n <- length(y)
  s <- cumsum(y)
  z <- max(abs(s))
  if (z == 0) return(1.0)
  z_n <- z / sqrt(n)
  
  # Helper to sum normal cdf terms for given k range and offset a
  sum_terms <- function(k_lo, k_hi, a) {
    if (k_lo > k_hi) return(0)
    k <- seq.int(k_lo, k_hi)
    sum(pnorm((4*k + a) * z_n) - pnorm((4*k + a - 2) * z_n))
  }
  
  k1_lo <- ceiling( (-n / z + 1) / 4 )
  k1_hi <- floor(  ( n / z - 1) / 4 )
  k2_lo <- ceiling( (-n / z - 3) / 4 )
  k2_hi <- floor(  ( n / z - 1) / 4 )
  
  p <- 1 - sum_terms(k1_lo, k1_hi, 1) + sum_terms(k2_lo, k2_hi, 3)
  max(min(p, 1), 0)  # clamp to [0,1] for numerical safety
}

# ---------- NEW: inline NIST sample support ----------
bits_from_string <- function(s) {
  s <- gsub("\\s+", "", s)
  if (nchar(s) == 0) stop("empty bitstring")
  b <- as.integer(strsplit(s, "", fixed = TRUE)[[1]])
  if (any(is.na(b) | !(b %in% c(0L,1L)))) stop("bitstring must contain only 0/1")
  b
}

run_test_string <- function(bitstring, M_block = 128L, m_apen = 10L) {
  bits <- bits_from_string(bitstring)
  n <- length(bits)
  res <- c(
    monobit   = p_monobit(bits),
    runs      = p_runs(bits),
    blockfreq = if (n >= M_block) p_blockfreq(bits, M_block) else NA_real_,
    apen      = tryCatch(p_apen(bits, m_apen), error = function(e) NA_real_),
    cusum_fwd = p_cusum(bits, reverse = FALSE),
    cusum_rev = p_cusum(bits, reverse = TRUE)
  )
  attr(res, "n") <- n
  res
}

# ---------- NEW: early dispatch for -test "<bits>" ----------
argv <- commandArgs(trailingOnly = TRUE)
if (length(argv) >= 2 && argv[1] %in% c("-test", "--test")) {
  # optional overrides like --M-block=32 --m-apen=5
  M_override <- 128L
  m_override <- 10L
  Mb <- grep("^--M-block=", argv, value = TRUE)
  if (length(Mb)) M_override <- as.integer(sub("^--M-block=", "", Mb[1]))
  ma <- grep("^--m-apen=", argv, value = TRUE)
  if (length(ma)) m_override <- as.integer(sub("^--m-apen=", "", ma[1]))
  
  r <- run_test_string(argv[2], M_block = M_override, m_apen = m_override)
  cat(sprintf("n = %d bits\n", attr(r, "n")))
  for (nm in names(r)) cat(sprintf("%s: %.15g\n", nm, r[[nm]]))
  quit(save = "no")
}

# ------------------ stream file & compute ------------------

uniformity10 <- function(pv) {
  eps <- .Machine$double.eps
  pv <- pv[is.finite(pv)]
  pv[pv <= 0] <- eps; pv[pv >= 1] <- 1 - eps
  o <- hist(pv, breaks = seq(0,1,length.out=11L), plot = FALSE)$counts
  e <- length(pv) / 10
  pchisq(sum((o - e)^2 / e), df = 9, lower.tail = FALSE)
}

compute_all <- function(path, n_bits, seq_count, bit_order="msb", chunk_bytes=1e6,
                        M_block=128L, m_apen=10L) {
  con <- file(path, "rb"); on.exit(close(con))
  buf <- integer(0); msb_first <- (bit_order == "msb")
  # columns: monobit, runs, blockfreq, apen, cusum_fwd, cusum_rev
  out <- matrix(NA_real_, nrow = seq_count, ncol = 6L)
  colnames(out) <- c("monobit","runs","blockfreq","apen","cusum_fwd","cusum_rev")
  
  for (i in seq_len(seq_count)) {
    while (length(buf) < n_bits) {
      rb <- readBin(con, what = "raw", n = chunk_bytes)
      if (length(rb) == 0L) stop("EOF before completing requested sequences")
      buf <- c(buf, raw_bytes_to_bits(rb, msb_first = msb_first))
    }
    bits <- buf[seq_len(n_bits)]
    buf  <- if (length(buf) == n_bits) integer(0) else buf[(n_bits+1):length(buf)]
    
    out[i, 1] <- p_monobit(bits)
    out[i, 2] <- p_runs(bits)
    out[i, 3] <- p_blockfreq(bits, M_block)
    out[i, 4] <- p_apen(bits, m_apen)
    out[i, 5] <- p_cusum(bits, reverse = FALSE)
    out[i, 6] <- p_cusum(bits, reverse = TRUE)
  }
  out
}

# -------- file-mode CLI parsing (unchanged logic) --------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) { cat("need .bin path\n"); quit(status=1) }
opt <- list(
  file       = args[1],
  n          = 1e6,
  seq        = 100L,
  bit_order  = "msb",   # msb|lsb within each BYTE (NIST tools use MSB-first)
  M_block    = 128L,
  m_apen     = 10L,
  per_seq    = FALSE
)
for (a in args[-1]) {
  if (grepl("^--n=", a))              opt$n <- as.numeric(sub("^--n=", "", a))
  else if (grepl("^--seq=", a))       opt$seq <- as.integer(sub("^--seq=", "", a))
  else if (grepl("^--bit-order=", a)) opt$bit_order <- tolower(sub("^--bit-order=", "", a))
  else if (grepl("^--M-block=", a))   opt$M_block <- as.integer(sub("^--M-block=", "", a))
  else if (grepl("^--m-apen=", a))    opt$m_apen <- as.integer(sub("^--m-apen=", "", a))
  else if (a == "--per-seq")          opt$per_seq <- TRUE
}
if (!file.exists(opt$file)) quit(status=2)
if (!opt$bit_order %in% c("msb","lsb")) quit(status=3)

pv <- compute_all(
  opt$file, n_bits = as.integer(opt$n), seq_count = as.integer(opt$seq),
  bit_order = opt$bit_order, M_block = opt$M_block, m_apen = opt$m_apen
)

# ------------------ Output ------------------

if (opt$per_seq) {
  # CSV rows: seq,monobit,runs,blockfreq,apen,cusum_fwd,cusum_rev
  for (i in seq_len(nrow(pv))) {
    cat(i); cat(",")
    cat(sprintf("%.15g", pv[i,1])); cat(",")
    cat(sprintf("%.15g", pv[i,2])); cat(",")
    cat(sprintf("%.15g", pv[i,3])); cat(",")
    cat(sprintf("%.15g", pv[i,4])); cat(",")
    cat(sprintf("%.15g", pv[i,5])); cat(",")
    cat(sprintf("%.15g", pv[i,6])); cat("\n")
  }
} else {
  cat("Monobit: ");        cat(sprintf("%.15g\n", uniformity10(pv[,1])))
  cat("Runs: ");           cat(sprintf("%.15g\n", uniformity10(pv[,2])))
  cat("BlockFrequency: "); cat(sprintf("%.15g\n", uniformity10(pv[,3])))
  cat("ApproxEntropy: ");  cat(sprintf("%.15g\n", uniformity10(pv[,4])))
  cat("Cusum-Forward: ");  cat(sprintf("%.15g\n", uniformity10(pv[,5])))
  cat("Cusum-Reverse: ");  cat(sprintf("%.15g\n", uniformity10(pv[,6])))
}
