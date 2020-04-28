blck_data <- function(x, n = 2L) {
  xlen <- NROW(x)
  xseq <- seq_len(xlen)
  denom <- xlen + 1
  frac <- ceiling((xseq %% denom)/n)
  split(x, frac)
}

blck_f <- function(x, f, n = 2L, drop = FALSE) {
  out_data <- blck_data(x, n)
  out <- lapply(out_data, f)
  if (isTRUE(drop)) {
    out <- Reduce(c, out)
  }
  out
}

blck_mean <- function(x, n = 2L, drop = FALSE) {
  blck_f(x, mean, n = n, drop = drop)
}

blck_median <- function(x, n = 2L, drop = FALSE) {
  blck_f(x, median, n = n, drop = drop)
}

blck_sd <- function(x, n = 2L, drop = FALSE) {
  blck_f(x, f = sd, n = n, drop = drop)
}

blck_sample <- function(x, n = 2L, drop = FALSE){
  blck_f(x, f = function(x) sample(x, replace = TRUE), n = n, drop = drop)
}
