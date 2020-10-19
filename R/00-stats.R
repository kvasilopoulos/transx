# https://www.tutorialspoint.com/statistics/continuous_series_arithmetic_mode.htm

# Geometric Mean
gmean <- function(x, na.rm = getOption("transx.na.rm")) {
  x <- na_rm_if(x, na.rm)
  exp(mean(log(x)))
}

#' Double
#'
#'
modex <- function(x, na.rm = getOption("transx.na.rm")) {
  x <- na_rm_if(x, na.rm)
  d <- density(x)
  d$x[which.max(d$y)]
}

#' @rdname modex
modex_int <- function(x, na.rm = getOption("transx.na.rm")) {
  x <- na_rm_if(x, na.rm)
  ux <- unique(x)
  ux[which.max(table(match(x, ux)))]
}

# maybe n - 1
# mg <- function(x, order = 1) {
#
#   n <- length(x)
#   if(order == 1) { #raw
#     mean(x)
#   } else if(order == 2) { #central
#      sd(x)
#   }else{ # standarised
#     sum(x-mean(x))^order/n
#   }
#
# }

#' Title
#'
#' Descreiption
#'
#'
#'
#'
skewness <- function(x, na.rm = getOption("transx.na.rm")) {
  x <- na_rm_if(x, na.rm)
  n <- length(x)
  m1 <- mean(x)
  m2 <- sum((x - m1)^2)/n
  m3 <- sum((x - m1)^3)/n
  m3/m2^(3/2)
}

#' @rdname skewness
kurtosis <- function(x, na.rm = getOption("transx.na.rm")) {
  x <- na_rm_if(x, na.rm)
  n <- length(x)
  n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
}

# moments:: other measure of kurtosis



