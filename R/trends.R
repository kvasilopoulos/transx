
#' @importFrom rlang eval_bare enexpr
custom_trend <- function(n, .f) {
  if (missing(.f)) {
    .expr <- rlang::expr(.t)
  }else{
    .expr <- rlang::enexpr(.f)
  }
  .env <- rlang::env(.t = 1:n)
  rlang::eval_bare(.expr, env = .env)
}

trend_fun <- function(type = c("linear", "quadratic")) {
  function(x) {
    if (type == "linear") {
      fun <- custom_trend(x, .t)
    }else{
      fun <- custom_trend(x, .t + .t^2)
    }
    fun
  }
}



#' Deterministic Trends
#'
dt_lin <- function(x) {
  t <- seq_along(x)
  unname(lm(x ~ t)$residuals)
}

dt_quad <- function(x) {
  t <- seq_along(x)
  unname(lm(x ~ t + I(t^2))$residuals)
}

dt_poly <- function(x) {
  trend <- seq_along(x)
  unname(lm(x ~ trend)$residuals)
}


#' Stochastic Trends
#'
st_arima <- function(x, ...) {
  t <- seq_along(x)
  arima(x, order = c(0L, 0L, 1L), xreg = t, ...)
}



