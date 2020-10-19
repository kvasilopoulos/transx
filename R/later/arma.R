#' @importFrom tseries arma
tsf_arma <- function(x, ar = 1, ma = 1, ...) {
  arima(x, order = c(ar, 0, ma), ...)$residuals
}
#' @importFrom tseries garch
tsf_garch <- function(x, arch = 1, garch = 1, ...) {
  tseries::garch(x, order = c(1, 0), ...)$residuals
}

# sma
# or other financial
