# Cycle Trend Decomposition -----------------------------------------------


# bk Baxter–King[TS] transx bk
# bw Butterworth[TS] transx bw
# cf Christiano–Fitzgerald[TS] transx cf
# hp Hodrick–Prescott[TS] transx hp


ct_hp <- function(x, freq = NULL, type = c("lambda", "frequency"),
                  drift = FALSE, verbose = FALSE) {
  if (is.null(drift))
    drift <- FALSE
  # xname = deparse(substitute(x))
  type = match.arg(type)
  if (is.null(type))
    type <- "lambda"
  if (is.ts(x)) {
    tsp.x <- tsp(x)
    frq.x <- frequency(x)
    if (type == "lambda") {
      if (is.null(freq)) {
        if (frq.x == 1)
          lambda = 6
        if (frq.x == 4)
          lambda = 1600
        if (frq.x == 12)
          lambda = 129600
      }
      else lambda = freq
    }
  }
  else {
    if (type == "lambda") {
      if (is.null(freq))
        stop("freq is NULL")
      else lambda = freq
    }
  }
  if (type == "frequency") {
    if (is.null(freq))
      stop("freq is NULL")
    else lambda = (2 * sin(pi/freq))^-4
  }
  xo = x
  x = as.matrix(x)
  if (drift)
    x = undrift(x)
  n = length(x)
  imat = diag(n)
  Ln = rbind(matrix(0, 1, n), diag(1, n - 1, n))
  Ln = (imat - Ln) %*% (imat - Ln)
  Q = t(Ln[3:n, ])
  SIGMA.R = t(Q) %*% Q
  SIGMA.n = diag(n - 2)
  g = t(Q) %*% as.matrix(x)
  b = solve(SIGMA.n + lambda * SIGMA.R, g)
  x.cycle = c(lambda * Q %*% b)
  x.trend = x - x.cycle
  if (is.ts(xo)) {
    tsp.x = tsp(xo)
    x.cycle = ts(x.cycle, start = tsp.x[1], frequency = tsp.x[3])
    x.trend = ts(x.trend, start = tsp.x[1], frequency = tsp.x[3])
    x = ts(x, start = tsp.x[1], frequency = tsp.x[3])
  }
  A = lambda * Q %*% solve(SIGMA.n + lambda * SIGMA.R) %*%
    t(Q)
  # res <- list(cycle = x.cycle, trend = x.trend, fmatrix = A,
  #             title = "Hodrick-Prescott Filter", xname = xname,
  #             call = as.call(match.call()), type = type, lambda = lambda,
  #             method = "hpfilter", x = x)
  structure(
    res,
    class = "hp")
}

hp_ <- function(x, lambda = c(100, 1600, 14400)) {
  lambda <- match.arg(lambda)
  x <- as.matrix(x)
  nr <- nrow(x)
  imat <- diag(nr)
  Ln = rbind(matrix(0, 1, nr), diag(1, nr - 1, nr))
  Ln = (imat - Ln) %*% (imat - Ln)
  Q = t(Ln[3:nr, ])
  sigma_r = t(Q) %*% Q
  sigma_n = diag(nr - 2)
  g = t(Q) %*% x
  b = solve(sigma_n + lambda * sigma_r, g)
  x_cycle = c(lambda * Q %*% b)
  # x_trend = x - x_cycle
  A = lambda * Q %*% solve(sigma_n + lambda * sigma_r) %*% t(Q)
  x.cycle
}


tsf_bp <- function(x) {

}

tsf_bk <- function(x) {

}



ct_hp <- function(x) {

}

ct_bk <- function(x) {

}

ct_cf <- function(x) {

}

ct_bw <- function(x) {

}

ct_trig <- function(x) {

}

ct_ham <- function(x, horizon, p, default = NA) {
  neverhpfilter::yth_filter(x)
}
