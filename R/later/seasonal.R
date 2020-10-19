
#' Seasonal adjustment with `decompose`
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#'
#' @param x A time series.
#'
#' @importFrom stats decompose
#' @export
seas_decompose <- function(x, ...) {
  opts <- rlang::dots_list(...)
  seas <- decompose(x, ...)$seasonal
  if (opts$type == "additive" || is.null(opts$type)) {
    deseas <- x - seas
  }else {
    deseas <- x/seas
  }
  deseas
}

#' Seasonal adjustment with `stl`
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#'
#' @inheritParams seas_decompose
#' @inheritParams stats::stl
#'
#' @importFrom stats stl
#' @export
seas_stl <- function(x, s.window = "periodic", ...) {
  x - stats::stl(x, s.window = s.window, ...)$time.series[,"seasonal"]
}

seas_sarima <- function() {

}

seas_struct <- function(x) {
  opts <- rlang::dots_list(...)
  fitted(StructTS(x, type =  "level"), ...)
}

# decompor::


# TODO StructTS
# TODO tsSmooth
# TODO kalmanSmooth
# TODO steinarv/predX
