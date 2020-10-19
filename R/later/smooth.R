sm_ma <- function(x, order, centre = TRUE)  {

  # forecast::ma
  if (abs(order - round(order)) > 1e-08) {
    stop("order must be an integer")
  }
  if (order %% 2 == 0 && centre) {
    w <- c(0.5, rep(1, order - 1), 0.5)/order
  }
  else {
    w <- rep(1, order)/order
  }
  filter(x, w)
  # imputeTS::na_ma
}


sm_holt <- function(x) {
  # stats::HoltWinters()
}

sm_kalman <- function(x, model = c("StructTS", "arima"), nit = - 1, ...) {


  model <- match.arg(model)
  if(model == "StructTS") {
    mod <- stats::StructTS(x, ...)$model0
  }else {

  }
  # Smoother
  kal <- stats::KalmanSmooth(x, mod, nit)
  erg <- kal$smooth
  erg %*% as.matrix(mod$Z)



  # kalman
  # imputeTS::na_kalman
  # ?stats::KalmanSmooth()
}


# browseURL("https://www.stat.cmu.edu/~cshalizi/dst/18/lectures/02/lecture-02.html#solutions-to-the-spline-problem-are-piecewise-cubic-polynomials")
