
#' Hamilton Filter
#'
#' This function computes the cyclical component of the Hamilton filter.
#'
#' @template x
#'
#' @param n `[integer(1): 4]`
#'
#' A value indicating the number of lags
#'
#' @param horizon `[integer(1): 8]`
#'
#' A value indicationg the number of periods to look ahead.
#'
#' @template fill
#' @template return
#'
#' @export
#'
#'
filter_hamilton <- function(x, n = 4, horizon = 8, fill = NA) {
  lagmatrix <- embed(c(rep(NA, p - 1), x) , p)
  y <- leadx_(x, h)
  out <- filter_hamilton_(x, n = n, horizon = horizon, fill = fill)
  with_attrs(out, x)
}

filter_hamilton_ <- function(x, n = 4, horizon = 8, fill = NA) {
  idx <- 1:(h + p - 1)
  body <- unname(stats::glm(yt ~ lagmatrix)$residuals)
  out <- fill_(body, idx, fill = fill)
  out
}


# Hodirck-Prescot ---------------------------------------------------------


#' Selecting lambda
#'
#' @description
#'
#' Approaches to selecting lambda.
#'
#' @details
#'
#' Rule of thumb is from Hodrick and Prescot (1997):
#'
#' - Lambda = 100*(number of periods in a year)^2
#'
#' * Annual data = 100 x 1^2 = 100
#' * Quarterly data = 100 x 4^2 = 1,600
#' * Monthly data = 100 x 12^2 = 14,400
#' * Weekly data = 100 x 52^2 = 270,400
#' * Daily data = 100 x 365^2 = 13,322,500
#'
#' Ravn and Uhlig (2002) state that lambda  should vary by the fourth power of the frequency observation ratio;
#'
#' - Lambda = 6.25 x (number of periods in a year)^4
#'
#' Thus, the rescaled default values for lambda are:
#'
#' * Annual data = 1600 x 1^4 = 6.25
#' * Quarterly data = 1600 x 4^4= 1600
#' * Monthly data = 1600 x 12^4= 129,600
#' * Weekly data = 1600 x 12^4 = 33,177,600
#'
#' @references Hodrick, R. J., & Prescott, E. C. (1997). Postwar US business cycles:
#' an empirical investigation. Journal of Money, credit, and Banking, 1-16.
#' @references Ravn, M. O., & Uhlig, H. (2002). On adjusting the Hodrick-Prescott
#' filter for the frequency of observations. Review of economics and statistics, 84(2), 371-376.
#'
#' @export
select_lambda <- function(freq = c("quarterly", "annual", "monthly", "weekly"),
                           type = c("rot", "ru2002")) {
  freq <- match.arg(freq)
  type <- match.arg(type)
  freq <- switch(
    freq,
    annual = 1,
    quarterly = 4,
    monthly = 12,
    weekly = 52
  )

  if (type == "rot") {
    lambda <- 100 * freq^2
  } else {
    lambda <- freq^4 * 6.25
  }
  lambda
}

#' Hodrick-Prescot Filter
#'
#' This function computes the cyclical component of the Hodrick-Prescot filter.
#'
#'
#' @template x
#' @param ...
#'
#' Further arguments passed to \code{\link[mFilter]{hpfilter}}.
#'
#'
#'
#' @seealso select_lambda
#' @export
#' @examples
#' \donttest{
#' x <- cumsum(rnorm(100))
#' filter_hp(x)
#' select_lambda("monthly")
#' filter_hp(x, freq = 100)
#'}
filter_hp <- function(x, ...) {
  need_pkg("mFilter")
  assert_uni_ts(x)
  dots <- rlang::dots_list(...)
  if(is.null(dots$freq) && (is.null(dots$type) || dots$type == "lambda")) {
    freq <- 1600
    type <- "lambda"
    disp_info("Using `lambda = 1600`.")
  }else{
    freq <- dots$freq
    type <- dots$type
  }
  if(is.null(dots$drift)) {
    drift <- FALSE
  }else{
    drift <- dots$drift
  }
  out <- mFilter::hpfilter(x, freq, type = type, drift = drift)$cycle
  with_attrs(out, x)
}


# TODO learn why it is 3 top and 3 bottom

#' Baxter-King Filter
#'
#' This function computes the cyclical component of the Baxter-King filter.
#'
#' @template x
#' @template fill
#' @param ...
#'
#' Further arguments passed to \code{\link[mFilter]{bkfilter}}.
#'
#' @export
#' @examples
#' \donttest{
#' x <- cumsum(rnorm(100))
#' filter_bk(x)
#'}
filter_bk <- function(x, fill = NA, ...) {
  need_pkg("mFilter")
  assert_uni_ts(x)
  pre <- mFilter::bkfilter(x, ...)$cycle
  idx <- which(is.na(pre))
  body <- body_(pre, idx)
  out <- fill_(body, idx, fill)
  with_attrs(out, x)
}

#' Christiano-Fitzgerald Filter
#'
#' This function computes the cyclical component of the Christiano-Fitzgerald filter.
#'
#' @template x
#' @param ...
#'
#' Further arguments passed to \code{\link[mFilter]{cffilter}}.
#'
#' @export
#' @examples
#' \donttest{
#' x <- cumsum(rnorm(100))
#' filter_cf(x)
#'}
filter_cf <- function(x) {
  need_pkg("mFilter")
  assert_uni_ts(x)
  out <- mFilter::cffilter(x)$cycle[,1]
  with_attrs(out, x)
}

#' Butterworth  Filter
#'
#' This function computes the cyclical component of the Butterworth  filter.
#'
#' @template x
#' @param ...
#'
#' Further arguments passed to \code{\link[mFilter]{bwfilter}}.
#'
#' @export
#' @examples
#' \donttest{
#' x <- cumsum(rnorm(100))
#' filter_bw(x)
#'}
filter_bw <- function(x) {
  need_pkg("mFilter")
  assert_uni_ts(x)
  out <- mFilter::bwfilter(x)$cycle
  with_attrs(out, x)
}

#' Trigonometric regression Filter
#'
#' This function computes the cyclical component of the trigonometric regression filter.
#'

#' @template x
#' @param ...
#'
#' Further arguments passed to \code{\link[mFilter]{trfilter}}.
#'
#' @export
#' @examples
#' \donttest{
#' x <- cumsum(rnorm(100))
#' filter_tr(x)
#'}
filter_tr <- function(x, ...) {
  need_pkg("mFilter")
  assert_uni_ts(x)
  out <- mFilter::trfilter(x)$cycle[,1]
  with_attrs(out, x)
}