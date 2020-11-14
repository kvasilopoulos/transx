
#' forecast::ma
smooth_ma <- function(x, order, center = TRUE)  {

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
  out <- filter(x, w)
  with_attrs(out, x)
  # imputeTS::na_ma
}


smooth_hw <- function(x) {
  # stats::HoltWinters()
}

smooth_kalman <- function(x, model = c("StructTS", "arima"), nit = - 1, ...) {


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


smooth_poly_periodic <- function(x) {
  wk = time(cmort) - mean(time(cmort)) # Normalize data
  #Specify polynomial functions
  wk2 = wk^2;
  wk3 = wk^3
  cs = cos(2*pi*wk); sn = sin(2*pi*wk) # estimate curves for monthly average

  reg1 = lm(x ~ wk + wk2 + wk3, na.action=NULL)
  reg2 = lm(x ~ wk + wk2 + wk3 + cs + sn, na.action=NULL)
}

smooth_kernel <- function(x) {
  ksmooth(x)
}

smooth_knn <- function(x) {
  supsmu(time(cmort), cmort, span=0.5)
}

smooth_lowess <- function(x) {
  lowess(cmort, f=0.02)
}

smooth_spline <- function(x) {
  smooth.spline(x)
}

# browseURL("https://www.stat.cmu.edu/~cshalizi/dst/18/lectures/02/lecture-02.html#solutions-to-the-spline-problem-are-piecewise-cubic-polynomials")
