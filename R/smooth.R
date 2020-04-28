sm_ma <- function(x, order, centre = TRUE)  {
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
}


sm_holt <- function(x) {

}


# browseURL("https://www.stat.cmu.edu/~cshalizi/dst/18/lectures/02/lecture-02.html#solutions-to-the-spline-problem-are-piecewise-cubic-polynomials")
