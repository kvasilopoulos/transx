#' Moving-average smoothing
#'
#' @description
#'
#' `r rlang:::lifecycle("experimental")`
#'
#' Computes a simple moving average smoother.
#'
#' @template x
#' @param order `[integer(1): NULL]`
#'
#' Order of moving average smoother.
#'
#' @param centre `[logical(1): TRUE]`
#'
#' Centers the moving average for even orders.
#'
#' @template fill
#'
#' @template return
#'
#' @importFrom stats filter approx
#' @examples
#'
#' x <- co2
#' x <- c(1:3, 5, 4, 7:3, 2*(2:5), rep(10, 4))#sin(1:100)
#' plotx(x)
#' lines(smooth_ma(x, 4), col = "red")
#' lines(smooth_spline(x), col = "purple")
#' lines(smooth_loess(x), col = "green")
#' @export
smooth_ma <- function(x, order = NULL, centre = TRUE, fill = NA)  {

  if (abs(order - round(order)) > 1e-08) {
    stop("order must be an integer")
  }
  if (order %% 2 == 0 && centre) {
    w <- c(0.5, rep(1, order - 1), 0.5)/order
  } else {
    w <- rep(1, order)/order
  }
  body <- filter(x, w)
  na <- which(is.na(body))
  out <- fill_(body_(body, na), na, fill)
  with_attrs(out, x)
}

#' Kernel Regression Smoother
#'
#' @description
#'
#' `r rlang:::lifecycle("experimental")`
#'
#' @template x
#' @param ...
#'
#' Further arguments passed to \code{\link[stats:ksmooth]{smooth_kernel}}
#'
#' @importFrom stats ksmooth
#' @examples
#'
#' x <- co2
#' plotx(smooth_kernel(x))
#' @export
smooth_kernel <- function(x, ...) {
  ksmooth(x, 1:length(x), n.points = length(x), ...)$y
}

#' LOWESS smoother
#'
#' @description
#'
#' `r rlang:::lifecycle("experimental")`
#'
#'Locally-weighted polynomial regression.
#'
#' @template x
#' @param ...
#'
#' Further arguments passed to \code{\link[stats:lowess]{lowess}}
#'
#' @importFrom stats lowess
#' @examples
#'
#' x <- co2
#' plotx(smooth_loess(x))
#' @export
smooth_loess <- function(x, ...) {
    lowess(x, ...)$y
}

#' Fit a Smoothing Spline
#'
#' @description
#'
#' `r rlang:::lifecycle("experimental")`
#'
#' @template x
#' @param ...
#'
#' Further arguments passed to \code{\link[stats:smooth.spline]{smooth.spline}}
#'
#'
#' @importFrom stats smooth.spline
#' @examples
#'
#' x <- co2
#' plotx(smooth_spline(x))
#' @export
smooth_spline <- function(x, ...) {
  smooth.spline(x, ...)$y
}

# smooth_hw <- function(x) {
#   HoltWinters(x)
# }
