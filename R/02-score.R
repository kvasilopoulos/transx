#' Score transformation
#'
#' @description These functions calculater the scores according to:
#'
#' * `score_z`: Normal(z) distribution
#' * `score_mad`: Mean absolute deviation
#' * `score_t`: t-distribution
#' * `score_chi`: chi-distribution
#'
#' @details Because function are known with different names:
#' * `score_z` is identical to `std_mean`
#' * `score_mad` is identical to `std_median`
#'
#'
#' @template x-na
#'
#' @name score
#' @export
#' @seealso \code{\link[outliers]{scores}}
#' @template return
score_z <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  x <- with_na_rm(x, na.rm)
  out <- zscore_(x)
  with_attrs(out, x)
}

#' @importFrom stats sd var
zscore_ <- function(x) {
  (x - mean(x))/sd(x)
}

#' @rdname score
score_mad <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  x <- with_na_rm(x, na.rm)
  out <- madscore_(x)
  with_attrs(out, x)
}

#' @importFrom stats median mad
madscore_ <- function(x) {
  (x - median(x) )/ mad(x)
}

#' @rdname score
score_t <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  x <- with_na_rm(x, na.rm)
  n <- length(x)
  t <- zscore_(x)
  out <- (t * sqrt(n - 2))/sqrt(n - 1 - t^2)
  with_attrs(out, x)
}

#' @rdname score
score_chisq <- function(x, na.rm = getOption("transx.na.rm")){
  assert_uni_ts(x)
  x <- with_na_rm(x, na.rm)
  out <- (x - mean(x))^2/var(x)
  with_attrs(out, x)
}


