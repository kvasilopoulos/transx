

# Basic -------------------------------------------------------------------

#' Compute lagged or leading values
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' Find the "previous" (`lagx()`) or "next" (`leadx()`) values in a vector.
#' Useful for comparing values behind of or ahead of the current values.
#'
#'
#' @param x `[univariate vector]`
#'
#' Univariate vector, numeric or ts object with only one dimension.
#'
#' @param n `[positive integer(1): 1L]`
#'
#' Value indicating the number of positions to lead or lag by.
#'
#' @param fill `[numeric: NA]`
#'
#' Value used to fill observations.
#'
#' @param fill_fun `[function: NULL]`
#'
#' Function used to fill observations. Uses the remaining observations as the
#' body of the function.
#'
#' @param ...
#'
#' Additional arguments passed on to the `fill_fun` function.
#'
#' @details This functions has been taken and modified from the `dplyr` package,
#' however, to reduce dependecies they are not imported.
#'
#'
#' @importFrom stats lag
#' @importFrom rlang as_function
#'
#' @template return-template
#'
#' @name leadx-lagx
#' @rdname leadx-lagx
#' @export
lagx <- function(x, n = 1L, fill = NA, fill_fun = NULL, ...) {
  asserts(x, n)
  asserts_fill(n, fill, fill_fun)
  out <- lagx_(x, n, fill = fill, fill_fun = fill_fun, ...)
  with_attrs(out, x)
}

lagx_ <- function(x, n, fill = NA, fill_fun = NULL, ...) {
  xlen <- length(x)
  n <- pmin(n, xlen)
  idx <- 1:n
  body <- x[seq_len(xlen - n)]
  out <- fill_(body, idx, fill, fill_fun, ...)
}


#' @rdname leadx-lagx
#' @export
leadx <- function(x, n = 1L, fill = NA, fill_fun = NULL, ...) {
  asserts(x, n)
  asserts_fill(n, fill, fill_fun)
  out <- leadx_(x, n, fill = fill, fill_fun = fill_fun)
  with_attrs(out, x)
}

leadx_ <- function(x, n, fill = NA, fill_fun = NULL, ...) {
  xlen <- length(x)
  n <- pmin(n, xlen)
  body <- x[-seq_len(n)]
  idx <- (xlen - n + 1):xlen
  fill_(body, idx, fill, fill_fun, ...)
}


# Differencing ------------------------------------------------------------

#' Compute lagged differnces
#'
#' @description Returns suitably lagged and iterated differences.
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
diffx <- function(x, n = 1L, order = 1L, rho = 1, fill = NA, fill_fun = NULL, ...) {
  asserts(x, n)
  asserts_fill(n, fill, fill_fun)
  out <- diffx_(x, n, order = order, fill = fill, fill_fun = fill_fun, ...)
  with_attrs(out, x)
}

diffx_ <- function(x, n, order = 1L, rho = NULL, fill = NA, fill_fun = NULL, ...) {
  xlen <- length(x)
  n <- pmin(n, xlen)
  idx <- 1:n
  for (i in seq_len(order))  {
    body <- x - rho*lagx_(x, n)
  }
  fill_(body[-idx], idx, fill, fill_fun)
}

#' @rdname diffx-rdiffx-ldiffx
#' @export
rdiff <- function(x, n = 1L, order = 1L, fill = NA, fill_fun = NULL, ...) {
  asserts(x, n, order = order)
  asserts_fill(n, fill, fill_fun)
  out <- rdiffx_(x, n, order = order, fill = fill, fill_fun = fill_fun, ...)
  with_attrs(out, x)
}

rdiffx_ <- function(x, n, order = 1L, fill = NA, fill_fun = NULL, ...) {
  xlen <- length(x)
  n <- pmin(n, xlen)
  idx <- 1:n
  len <- length(x)
  for (i in seq_len(order))  {
    body <- diffx_(x, n)/lagx_(x, n)
  }
  idx_fill(body, idx, fill, fill_fun, ...)
}

#' @rdname diffx-rdiffx-ldiffx
#' @export
ldiffx <- function(x, n = 1L, order = 1L, default = NA, ...) {
  asserts(x, n, order = order)
  asserts_fill(n, fill, fill_fun)
  out <- ldiff_(x, n, order = order, fill = fill, fill_fun = fill_fun, ...)
  with_attrs(out, x)
}

ldiff_ <- function(x, n, order = 1L, rho = NULL, fill = NA, fill_fun = NULL, ...) {
  xlen <- length(x)
  n <- pmin(n, xlen)
  idx <- 1:n
  len <- length(x)
  for (i in seq_len(order))  {
    body <- diffx_(log(x), n, rho = rho)
  }
  fill_(body, idx, fill, fill_fun, ...)
}

# Rescaling ---------------------------------------------------------------


#' Remove the measure of centrality from the series
#'
#' @desciption removes the mean or the median from the series.
#'
#' @param x `[univariate vector]`
#'
#' Univariate vector, numeric or ts object with only one dimension.
#'
#' @param na.rm `[logical(1): getOption("transx.na.rm")]`
#'
#' A value indicating whether NA values should be stripped before the computation proceeds.
#'
#' @name demean-demedian
#' @export
#' @template return-template
demean <- function(x, na.rm = getOption("transx.na.rm")) {
  asserts(x)
  x <- na_rm_if(x, na.rm)
  out <- demean_(x)
  with_attrs(out, x)
}

demean_ <- function(x) {
  x - mean(x)
}


#' @rdname demean-demedian
demedian <- function(x, na.rm = getOption("transx.na.rm")) {
  asserts(x)
  x <- na_rm_if(x, na.rm)
  out <- x - median(x)
  with_attrs(out, x)
}

demode <- function(x, na.rm = getOption("transx.na.rm")) {
  asserts(x)
  x <- na_rm_if(x, na.rm)
  out <- x - modex(x)
  with_attrs(out, x)
}



#' Change the base year of an index
#'
#' @description goes here
#'
#' @name rebase
#' @export
#' @template return-template
rebase <- function(x, n = NULL) {
  asserts(x, n)
  out <- rebase_(x, n)
  with_attrs(out, x)
}

rebase_ <- function(x, n) {
  x/x[n]
}

#' @rdname rebase
rebase_origin <- function(x) {
  asserts(x, n)
  out <- rebase_(x, n = 1L)
  with_attrs(out, x)
}


#' Normalise
#'
#' @inheritParams demean-demedian
#'
#' @name norm
#' @export
#' @template return-template
norm_minmax <- function(x, na.rm = getOption("transx.na.rm")) {
  asserts(x)
  x <- na_rm_if(x, na.rm)
  out <- norm_range(x, min = 0, max = 1)
  with_attrs(out, x)
}

#' @rdname norm
norm_logistic <- function(x) {
  asserts(x)
  out <- 1/(1 + exp(-x))
  with_attrs(out, x)
}

#' @rdname norm
norm_range <- function(x, min, max) {
  min + (max - min)/(max(x) - min(x))*(x - min(x))
}


#' Standarize
#'
#' @inheritParams demean-demedian
#'
#' @name std
#' @export
#' @template return-template
std_mean  <- function(x, na.rm = getOption("transx.na.rm")) {
  asserts(x)
  x <- na_rm_if(x, na.rm)
  out <- std_mean_(x)
  with_attr(out, x)
}

std_mean_ <- function(x) {
  (x - meanx(x))/sdx(x)
}

#' @rdname std
std_median <- function(x, na.rm = getOption("transx.na.rm")) {
  asserts(x)
  x <- with_na_rm(x, na.rm)
  out <- std_median_(x)
  with_attr(out, x)
}

std_median_ <- function(x) {
  (x - median(x) )/ mad(x)
}


#' Score transformation
#'
#' @name score
#' @export
#' @seealso outlier::scores
#' @template return-template
score_z <- function(x, na.rm = getOption("transx.na.rm")) {
  asserts(x)
  x <- with_na_rm(x, na.rm)
  out <- std_mean_(x)
  with_attr(out, x)
}

#' @rdname score
score_mad <- function(x, na.rm = getOption("transx.na.rm")) {
  asserts(x)
  x <- with_na_rm(x, na.rm)
  out <- std_median_(x)
  with_attr(out, x)
}

#' @rdname score
score_t <- function(x, na.rm = getOption("transx.na.rm")) {
  asserts(x)
  x <- with_na_rm(x, na.rm)
  n <- length(x)
  t <- std_mean_(x)
  out <- (t * sqrt(n - 2))/sqrt(n - 1 - t^2)
  with_attr(out, x)
}

#' @rdname score
score_chisq <- function(x, na.rm = getOption("transx.na.rm")){
  asserts(x)
  x <- with_na_rm(x, na.rm)
  out <- (x - mean(x))^2/var(x)
  with_attr(out, x)
}


