

# Basic -------------------------------------------------------------------

#' Compute lagged or leading values
#'
#' @description
#'
#' `r rlang:::lifecycle("stable")`
#'
#' Find the "previous" (`lagx()`) or "next" (`leadx()`) values in a vector.
#' Useful for comparing values behind of or ahead of the current values.
#'
#' @template x
#'
#' @param n `[positive integer(1): 1L]`
#'
#' Value indicating the number of positions to lead or lag by.
#'
#' @template fill
#'
#' @details This functions has been taken and modified from the `dplyr` package,
#' however, to reduce dependecies they are not imported.
#'
#'
#' @importFrom stats lag
#' @importFrom rlang as_function
#'
#' @template return
#'
#' @name leadx-lagx
#' @rdname leadx-lagx
#' @export
#' @examples
#'
#' x <- c(5,3,2,2,5)
#' lagx(x)
#' lagx(x, fill = mean)
#' lagx(x, fill = fill_nocb)
#'
#' leadx(x)
#' leadx(x, fill = fill_locf)
lagx <- function(x, n = 1L, fill = NA) {
  assert_lx(x, n)
  asserts_fill(n, fill)
  out <- lagx_(x, n, fill = fill, internal = FALSE)
  with_attrs(out, x)
}

lagx_ <- function(x, n, fill = NA, internal = TRUE) {
  xlen <- length(x)
  n <- pmin(n, xlen)
  # here the idx and the subseting do not match
  idx <- 1:n
  body <- x[seq_len(xlen - n)]
  fill_(body, idx, fill, internal = internal)
}


#' @rdname leadx-lagx
#' @export
leadx <- function(x, n = 1L, fill = NA) {
  assert_lx(x, n)
  asserts_fill(n, fill)
  out <- leadx_(x, n, fill = fill, internal = FALSE)
  with_attrs(out, x)
}

leadx_ <- function(x, n, fill = NA, internal = TRUE) {
  xlen <- length(x)
  n <- pmin(n, xlen)
  body <- x[-seq_len(n)] # no need for body_ here
  idx <- (xlen - n + 1):xlen
  fill_(body, idx, fill, internal = internal)
}


# Differencing ------------------------------------------------------------

#' Compute lagged differnces
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#' Returns suitably lagged and iterated differences.
#'
#' * `diffx` computes simple differences.
#' * `rdffix` computes percentage differences.
#' * `ldiffx` computes logged differences.
#'
#' @inheritParams leadx-lagx
#'
#' @param n `[positive integer(1): 1L]`
#'
#' Value indicating which lag to use.
#'
#' @param order `[positive integer(1): 1L]`
#'
#' Value indicating the order of the difference.
#'
#' @param rho `[numeric(1): NULL]`
#'
#' Value indicating the autocorrelation parameter. The purpose of this parameter is
#' to provide  quasi-differencing assuming the value falls within 0 and 1.
#'
#' @name diffx-rdiffx-ldiffx
#' @export
#' @examples
#' x <- c(2, 4, 8, 20)
#' diffx(x)
#' rdiffx(x)
#' ldiffx(x)
#'
diffx <- function(x, n = 1L, order = 1L, rho = 1, fill = NA) {
  asserts_diff(x, n, order = order)
  asserts_fill(n, fill)
  out <- diffx_(x, n, order = order, rho = rho, fill = fill, internal = FALSE)
  with_attrs(out, x)
}

diffx_ <- function(x, n, order = 1L, rho = 1, fill = NA, internal = TRUE) {
  xlen <- length(x)
  n <- pmin(n, xlen)
  idx <- 1:n
  for (i in seq_len(order))  {
    nx <- x - rho*lagx_(x, n)
  }
  body <- body_(nx, idx)
  fill_(body, idx, fill, internal = internal)
}

#' @rdname diffx-rdiffx-ldiffx
#' @export
rdiffx <- function(x, n = 1L, order = 1L, rho = NULL, fill = NA) {
  asserts_diff(x, n, order = order)
  asserts_fill(n, fill)
  out <- rdiffx_(x, n, order = order, fill = fill, internal = FALSE)
  with_attrs(out, x)
}

rdiffx_ <- function(x, n, order = 1L, fill = NA, internal = TRUE) {
  xlen <- length(x)
  n <- pmin(n, xlen)
  idx <- 1:n
  len <- length(x)
  for (i in seq_len(order))  {
    nx <- diffx_(x, n)/lagx_(x, n)
  }
  body <- body_(nx, idx)
  fill_(body, idx, fill, internal = internal)
}

#' @rdname diffx-rdiffx-ldiffx
#' @export
ldiffx <- function(x, n = 1L, order = 1L, rho = 1, fill = NA) {
  asserts_diff(x, n, order = order)
  asserts_fill(n, fill)
  out <- ldiffx_(x, n, order = order, rho = rho, fill = fill, internal = FALSE)
  with_attrs(out, x)
}

ldiffx_ <- function(x, n, order = 1L, rho = 1, fill = NA, internal = TRUE) {
  xlen <- length(x)
  n <- pmin(n, xlen)
  idx <- 1:n
  for (i in seq_len(order))  {
    nx <- diffx_(log(x), n, rho = rho)
  }
  body <- body_(nx, idx)
  fill_(body, idx, fill, internal = internal)
}

# Rescaling ---------------------------------------------------------------


#' Removes measure of centrality from the series
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#' Removes the mean, the median or the mode from the series.
#'
#' @template x-na
#' @template return
#' @name demean-demedian
#' @export
#' @examples
#'
#' x <- c(2,5,10,20,30)
#' summary(x)
#'
#' demean(x)
#' demedian(x)
#' demode(x)
demean <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  x <- with_na_rm(x, na.rm)
  out <- demean_(x)
  with_attrs(out, x)
}

demean_ <- function(x) {
  x - mean(x)
}

#' @rdname demean-demedian
#' @export
demedian <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  x <- with_na_rm(x, na.rm)
  out <- x - median(x)
  with_attrs(out, x)
}

#' @rdname demean-demedian
#' @export
demode <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  x <- with_na_rm(x, na.rm)
  out <- x - modex(x)
  with_attrs(out, x)
}


#' Change the base year
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#' Change the base year.
#'
#' @name rebase
#' @template x
#' @param n `[numeric(1): NULL]`
#'
#' The index of the new base year.
#'
#' @template return
#' @export
#' @examples
#'
#' x <- 3:10
#'
#' # New base  would be 5
#' rebase(x, 5)
#'
#' # Or the origin
#' rebase_origin(x)
#'
#' # Fro the base to be 100 or 0 then:
#' rebase(x, 5)*100
#' rebase(x, 5) - 1
rebase <- function(x, n = NULL) {
  assert_lx(x, n)
  out <- rebase_(x, n)
  with_attrs(out, x)
}

rebase_ <- function(x, n) {
  x/x[n]
}

#' @rdname rebase
#' @export
rebase_origin <- function(x) {
  assert_lx(x, 1)
  out <- rebase_(x, n = 1L)
  with_attrs(out, x)
}


#' Rescale
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#'
#' @details To rescale a range between an arbitrary set of values \[a, b\], the formula becomes:
#'
#' @inheritParams demean-demedian
#' @param to `[numeric(2): NULL]`
#'
#' Values that will determine the output range.
#'
#' @template return
#'
#' @export
#'
#' @examples
#' x <- c(10,5,1,-2)
#' scale_range(x, c(-1, 2))
#' scale_minmax(x)
scale_range <- function(x, to, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  x <- with_na_rm(x, na.rm)
  out <- scale_range_(x, a = to[1], b = to[2])
  with_attrs(out, x)
}

#' @rdname scale_range
#' @export
scale_minmax <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  x <- with_na_rm(x, na.rm)
  out <- scale_range_(x, a = 0, b = 1)
  with_attrs(out, x)
}

#' @rdname scale_range
#' @export
scale_unit_len <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  out <- x/norm_vec(x)
  with_attrs(out, x)
}

norm_vec <- function(x) sqrt(sum(x^2))

scale_range_ <- function(x, a, b) {
  a + (b - a)/(max(x) - min(x))*(x - min(x))
}




# experimental ------------------------------------------------------------



#' Standarization
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#'
#' Convert number of standard deviations by which the value of a raw score
#' is above or below the mean value of what is being observed or measured.
#'
#' @template x-na
#' @template return
#'
#' @name std
#' @export
#'
#' @examples
#' x <- c(10,2,5,3)
#' std_mean(x)
#' scale(x)
#'
#' std_median(x)
std_mean  <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  x <- with_na_rm(x, na.rm)
  out <- zscore_(x)
  with_attrs(out, x)
}


#' @rdname std
#' @export
std_median <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  x <- with_na_rm(x, na.rm)
  out <- madscore_(x)
  with_attrs(out, x)
}

# std_moment <- function(x) {
  # https://en.wikipedia.org/wiki/Standardized_moment#:~:text=In%20probability%20theory%20and%20statistics,renders%20the%20moment%20scale%20invariant.
  #1st = 0
  #2nd = 1
  #3rd = skewness
  #4th = curtosis
# }



# TODO write examples to all functions



