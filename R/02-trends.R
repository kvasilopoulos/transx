

# Deterministic Trend -----------------------------------------------------


split_at <- function(x, bp) {
  split(x, cumsum(seq_along(x) %in% bp))
}


# TODO not sure bp works here
# https://uk.mathworks.com/help/matlab/ref/detrend.html

dtrend_ <- function(x, degree = 1, bp = NULL, raw = FALSE) {
  if(!is.null(bp)) {
    xsplit <- split_at(x, bp)
    res <- list()
    for(i in 1:(length(bp)+1)) {
      res[[i]] <- lm(xsplit[[i]] ~ poly(xsplit[[i]], degree, raw = raw))$residuals
    }
    res <- Reduce(c, res)
  }else{
    res <- lm(x ~ poly(x, degree, raw = raw))$residuals
  }
  na_obs <- is.na(x)
  if(any(na_obs)) {
    out <- fill_(x, which(na_obs), fill = NA, fill_fun = NULL)
  }else {
    out <- res
  }
  out
}


#' Deterministic Trends
#'
#' @description Remove global trend information from the series.
#'
#' * `dt_lin` removes the linear trend.
#' * `dt_quad` removes the quadratic trend.
#' * `dt_poly` removes the nth-degree polynomial trend.
#'
#' @param degree `[positive integer(1)]`
#'
#' Value indicating the degree of polynomial
#'
#' @param bp `[positive integer(1)]`
#'
#' Break points to define piecewise segments of the data.
#'
#' @name dtrend
#' @export
#' @examples
#'
#' set.seed(123)
#' vec2 <- cumsum(c(rnorm(20), 1+rnorm(20)))
#' plot.ts(vec2)
#'
#' plot.ts(cbind(dtrend_lin(vec2), dtrend_lin(vec2, bp = 20)))
#' dtrend_quad(vec2)
#'
dtrend_lin <- function(x, bp = NULL, na.rm = getOption("transx.na.rm")) {
  # asserts_dtrend(x, degree, bp = bp)
  x <- na_rm_if(x, na.rm)
  out <- dtrend_(x, 1, bp = bp)
  with_attrs(out, x)
}


#' @rdname dtrend
dtrend_quad <- function(x, bp = NULL, na.rm = getOption("transx.na.rm")) {
  # asserts_dtrend(x, degree, bp = bp)
  x <- na_rm_if(x, na.rm)
  out <- dtrend_(x, 2, bp = bp)
  with_attrs(out, x)
}

#' @rdname dtrend
dtrend_poly <- function(x, degree, bp = NULL, raw = FALSE, na.rm = getOption("transx.na.rm")) {
  # asserts_dtrend(x, degree, bp = bp, raw = raw)
  x <- na_rm_if(x, na.rm)
  out <- dtrend_(x, degree, raw = raw,  bp = bp)
  with_attrs(out, x)
}




# Stochastic Trend --------------------------------------------------------

#  d Order of first-differencing. If missing, will choose a value based

# auto_ar <- function(x, d, max.p = 5, ic = "aic", ...) {
#   p <- max.p
#   bestfit <- arima2(x, order = c(p, 0, 0), include.mean = FALSE)
#   while (p >= 0) {
#     fit <- arima2(x, order = c(p, 0, 0), include.mean = FALSE)
#     if (fit[[ic]] < bestfit[[ic]]) {
#       bestfit <- fit
#     }
#     p <- p - 1
#   }
#   bestfit
# }
#
# arima2 <- function(x, order = c(0, 0, 0), ic = "aic", method = "CSS", ...) {
#   fit <- arima(x = x, order = order, ...)
#   npar <- length(fit$coef[fit$mask]) + 1
#   n <- NROW(x)
#   m <- frequency(x)
#   nstar <- n - m
#   dots <- list(...)
#   if (is.null(dots$xreg)) {
#     nxreg <- 0
#   } else {
#     nxreg <- ncol(as.matrix(xreg))
#   }
#
#   if (!is.na(fit$aic)) {
#     fit$bic <- fit$aic + npar * (log(nstar) - 2)
#     fit$aicc <- fit$aic + 2 * npar * (npar + 1) / (nstar - npar - 1)
#     fit$ic <- switch(ic, bic = fit$bic, aic = fit$aic, aicc = fit$aicc)
#   }
#   else {
#     fit$aic <- fit$bic <- fit$aicc <- fit$ic <- Inf
#   }
#   fit
# }

# # Stochastic Trends
# strend_arima <- function(x, ...) {
#   t <- seq_along(x)
#   arima(x, order = c(0L, 0L, 1L), xreg = t, ...)
# }
#
# # Unobserved Component
# # https://faculty.washington.edu/ezivot/econ584/notes/trendcycle.pdf
# strend_uc <- function() {
#
# }
