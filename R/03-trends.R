

# Deterministic Trend -----------------------------------------------------


split_at <- function(x, bp) {
  split(x, cumsum(seq_along(x) %in% bp))
}

# https://uk.mathworks.com/help/matlab/ref/detrend.html

#' @importFrom stats lm poly
dtrend_ <- function(x, degree = 1, bp = NULL) {
  t <- seq_along(x)
  if(!is.null(bp)) {
    xsplit <- split_at(x, bp)
    tsplit <- split_at(t, bp)
    res <- list()
    for(i in 1:length(xsplit)) {
      res[[i]] <- lm(xsplit[[i]] ~ poly(tsplit[[i]], degree))$residuals
    }
    res <- Reduce(c, res)
  }else{
    res <- lm(x ~ poly(t, degree))$residuals
  }
  na_obs <- is.na(x)
  if(any(na_obs)) {
    out <- fill_(x, which(na_obs), fill = NA, fill_fun = NULL)
  }else {
    out <- res
  }
  out
}

#' Deterministic Trend
#'
#' @description Remove global deterministic trend information from the series.
#'
#' * `dt_lin` removes the linear trend.
#' * `dt_quad` removes the quadratic trend.
#' * `dt_poly` removes the nth-degree polynomial trend.
#'
#' @template x-na
#'
#' @param degree `[positive integer(1)]`
#'
#' Value indicating the degree of polynomial
#'
#' @param bp `[positive integer(1)]`
#'
#' Break points to define piecewise segments of the data.
#'
#' @template return
#' @name dtrend
#' @export
#' @examples
#'
#' set.seed(123)
#' t <- 1:20
#'
#' # Linear trend
#' x <-  3*sin(t) + t
#' plotx(cbind(x, dtrend_lin(x)))
#'
#' # Quadratic trend
#' x2 <-  3*sin(t) + t + t^2
#' plotx(cbind(raw = x2, quad = dtrend_quad(x2)))
#'
#' # Introduce a breaking point at point = 10
#' xbp <- 3*sin(t) + t
#' xbp[10:20] <- x[10:20] + 15
#' plotx(cbind(raw = xbp, lin = dtrend_lin(xbp), lin_bp = dtrend_lin(xbp, bp = 10)))
#'
dtrend_lin <- function(x, bp = NULL, na.rm = getOption("transx.na.rm")) {
  asserts_dtrend(x, bp = bp)
  x <- with_na_rm(x, na.rm)
  out <- dtrend_(x, 1, bp = bp)
  with_attrs(out, x)
}


#' @rdname dtrend
#' @export
dtrend_quad <- function(x, bp = NULL, na.rm = getOption("transx.na.rm")) {
  asserts_dtrend(x, bp = bp)
  x <- with_na_rm(x, na.rm)
  out <- dtrend_(x, 2, bp = bp)
  with_attrs(out, x)
}

#' @rdname dtrend
#' @export
dtrend_poly <- function(x, degree, bp = NULL, na.rm = getOption("transx.na.rm")) {
  asserts_dtrend(x, degree, bp = bp, raw = raw)
  x <- with_na_rm(x, na.rm)
  out <- dtrend_(x, degree,  bp = bp)
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
