# https://www.tutorialspoint.com/statistics/continuous_series_arithmetic_mode.htm

#' Geometric Mean value
#'
#' Compute the sample geometric mean.
#'
#' @inheritParams demean-demedian
#' @export
#' @template return
gmean <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  x <- with_na_rm(x, na.rm)
  out <- exp(mean(log(x)))
  out
}

#' Mode value
#'
#' Compute the sample median.
#'
#' @inheritParams demean-demedian
#' @export
#' @importFrom stats density
#'
modex <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  x <- with_na_rm(x, na.rm)
  if(length(x) == 1) return(x)
  d <- density(x)
  d$x[which.max(d$y)]
}

#' @rdname modex
modex_int <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  # stopifnot(is.integer(x))
  x <- with_na_rm(x, na.rm)
  ux <- unique(x)
  ux[which.max(table(match(x, ux)))]
}

#' Skewness/Kurtosis Value
#'
#' Compute the sample skewness/kurtosis
#'
#' @inheritParams demean-demedian
#' @export
#'
skewness <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  x <- with_na_rm(x, na.rm)
  n <- length(x)
  m1 <- mean(x)
  m2 <- sum((x - m1)^2)/n
  m3 <- sum((x - m1)^3)/n
  m3/m2^(3/2)
}

#' @rdname skewness
kurtosis <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_uni_ts(x)
  x <- with_na_rm(x, na.rm)
  n <- length(x)
  m1 <- mean(x)
  n * sum((x - m1)^4)/(sum((x - m1)^2)^2)
}

# moments:: other measure of kurtosis


# Moments
moment <- function(x, order =1) {
  sum(x^order)/length(x)
}
