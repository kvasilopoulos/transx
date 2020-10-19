is_odd <- function(x) {
  x %% 2L == 1L
}

#' Functions used to calculate non-overlapping blocks
#'
#' @param x data argument
#' @param n size of the block
#' @param align align
#'
#'
blck_idx <- function(x, n, align) {

  xlen <- length(xlen)
  if (is_odd(n) && align %in% c("center", "right")) {
    message("To retain the sample size, when n is odd `center` and `right` alignment give one less observation.")
  }

  if (align == "left") {
    xseq <- seq(n, xlen, by = n)
  } else if (align == "right") {
    xseq <- seq(1, xlen, by = n)
  } else {
    xseq <- seq.int(ceiling(n/2), xlen, by = n)
  }

  which(!1:xlen %in% xseq)
}

#' @rdname blck_idx
blck_data <- function(x, n = 2L) {
  # consider vec_chop
  xlen <- NROW(x)
  xseq <- seq_len(xlen)
  denom <- xlen + 1
  frac <- ceiling((xseq %% denom)/n)
  split(x, frac)
}

#' @rdname blck_idx
blck_f <- function(x, f, n = 2L, fill = NA, fill_fun = NULL, align = "left") {
  vec_data <- blck_data(x, n)
  xlen <- length(x)
  vec <- lapply(vec_data, f)
  body <- Reduce(c, vec)
  if (is_odd(n) && align %in% c("center", "right"))
    body <- body[-length(body)]
  idx <- blck_idx(x, n, align)
  out <- idx_fill(body, idx, fill = fill, fill_fun = fill_fun)
  out
}

#' Non-Overapping Block Moment Calculations
#'
#' @param x numeric vector
#' @param n block size
#' @param drop reduce to vector
#'
#' @export
blck_mean <- function(x, n = 2L, fill = NA, fill_fun = NULL) {
  blck_f(x, mean, n = n, fill = fill, fill_fun = fill_fun)
}

#' @rdname blck_mean
blck_median <- function(x, n = 2L, drop = TRUE) {
  blck_f(x, median, n = n)
}

#' @rdname blck_mean
blck_sd <- function(x, n = 2L, drop = TRUE) {
  blck_f(x, f = sd, n = n)
}

#' Block Resampling
#'
#' @inheritParams blck_mean
#'
#' @export
blck_sample <- function(x, n = 2L, drop = FALSE){
  blck_f(x, f = function(x) sample(x, replace = TRUE), n = n, drop = drop)
}
