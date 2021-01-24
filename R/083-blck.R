is_odd <- function(x) {
  x %% 2L == 1L
}

is_even <- function(x) {
  !is_odd(x)
}

#' Functions used to calculate non-overlapping blocks
#'
#' @param x data argument
#' @param n size of the block
#' @param align align
#'
#'
blck_idx <- function(x, n, align) {

  xlen <- length(x)
  if (is_odd(n) && align %in% c("center", "right")) {
    disp_info("To retain the sample size, when n is odd `center` and `right` alignment give one less observation.")
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


# main --------------------------------------------------------------------

#' Rolling operations
#'
#' Apply rolling operations over a moving window for size `n` and increment `step`.
#'
#' @template x
#'
#' @param fn `[function]`
#'
#' @param n `[positive integer(1):1L]`
#'
#' Window size.
#'
#' @param align `[character(1): "left"]`
#'
#' Specifying whether the index of the result should be left- or right-aligned
#' or centered (default) compared to the rolling window of observations.
#'
#' @template fill
#' @param ... Additional arguments passed to the function `fn`.
#'
#' @return
#'
#' * `roll()` returns a vector with the same class and attributes as the input vector.
#' * `roll_data()` Returns a list of length length(x)/step.
#'
#' @export
#' @examples
#'
#' x <- seq(10, 1, -1)
blck <- function(x, fn, n = 1L, fill = NA, align = "left", ...) {
  vec_data <- blck_data(x, n)
  xlen <- length(x)
  vec <- lapply(vec_data, as_fn(fn, ...))
  body <- Reduce(c, vec)
  if (is_odd(n) && align %in% c("center", "right"))
    body <- body[-length(body)]
  idx <- blck_idx(x, n, align)
  out <- fill_(body, idx, fill = fill)
  out
}


#' @rdname blck
blck_data <- function(x, n = 1L) {
  # consider vec_chop
  xlen <- length(x)
  xseq <- seq_len(xlen)
  denom <- xlen + 1
  frac <- ceiling((xseq %% denom)/n)
  unname(split(x, frac))
}



# Block statistics --------------------------------------------------------

# Centrality ----

#' Non-Overapping Block Moment Calculations
#'
#' @param x numeric vector
#' @param n block size
#'
#' @template fill
#'
#' @export
blck_mean <- function(x, n = 2L, fill = NA) {
  blck(x, mean, n = n, fill = fill)
}

#' @rdname blck_mean
blck_median <- function(x, n = 2L) {
  blck(x, median, n = n)
}

#' @rdname blck_mean
blck_modex <- function(x, n = 2L) {
  blck(x, modex, n = n)
}

# Dispersion ----



# Shape  ----

#' @rdname blck_mean
blck_sd <- function(x, n = 2L) {
  blck(x, sd, n = n)
}

