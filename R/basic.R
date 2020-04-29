assert_length_default <- function(n, default) {
  if (length(default) != n)
    stop("`default` exceeds replacement length")
}


# Basic -------------------------------------------------------------------

#' Compute lagged or leading values
#'
#' Find the "previous" (`lag()`) or "next" (`lead()`) values in a vector.
#' Useful for comparing values behind of or ahead of the current values.
#' This functions has been taken as they are from `dply` package, but to reduce
#' dependecies they are not imported, but copied instead.
#'
#'
#' @param x Vector of values
#' @param n Positive integer of length 1, giving the number of positions to
#'   lead or lag by
#' @param default Value used for non-existent rows. Defaults to `NA`.
#' @importFrom stats lag
#'
#' @name leadx-lagx


#' @rdname leadx-lagx
#' @export
lagx <- function(x, n = 1L, default = NA, ...) {
  # assert_length_default(n, default)
  vec_assert(x, numeric())
  xlen <- length(x)
  n <- pmin(n, xlen)
  out <- c(rep(default, n), x[seq_len(xlen - n)])
  attributes(out) <- attributes(x)
  new_vctr(out, class = "lagx")
}

#' @rdname leadx-lagx
#' @export
leadx <- function(x, n = 1L, default = NA, ...) {
  if (n == 0) {
    return(x)
  }
  xlen <- length(x)
  n <- pmin(n, xlen)
  out <- c(x[-seq_len(n)], rep(default, n))
  attributes(out) <- attributes(x)
  out
}


# Differencing ------------------------------------------------------------

#' Lagged Differnces
#'
#' Returns
#'
#' @name diffx-rdiffx-ldiffx
#' @export
diffx <- function(x, n = 1L, order = 1L, default = NA) {
  x - lag(x, n = n)
}

#' @rdname diffx-rdiffx-ldiffx
#' @export
rdiffx <- function(x, n = 1L, order = 1L, default = NA) {
  x/x[-length(x)] - 1
}

#' @rdname diffx-rdiffx-ldiffx
#' @export
ldiffx <- function(x, n = 1L, order = 1L, default = NA) {
  diff(log(x))
}


# Normalisation -----------------------------------------------------------

#' Some title
#'
#' @param x some param
#'
#' @export
demean <- function(x, ...) {
  x - mean(x, ...)
}

#' Some title
#'
#' @param x some param
#'
#' @export
normalise <- normalize <- function(x, n = 1L) {
  x/x[n]
}

#' Some title
#'
#' @param x some param
#'
#' @export
normalise0 <- normalize0 <- function(x, n = 1L) {
  x/x[n] - 1
}

#' Some title
#'
#' @param x some param
#'
#' @export
minmax <- function(x, ...) {
  (x - min(x, ...))/(max(x, ...) - min(x, ...))
}

#' Some title
#'
#' @param x some param
#'
#' @export
standardise <- standarize <- function(x, ...) {
  # center <- centerFun(x)
  # x <- x - center
  # if (identical(centerFun, mean) && identical(scaleFun,
  #                                             sd)) {
  #   scale <- sqrt(sum(x^2)/max(1, length(x) - 1))
  # }
  # else if (identical(centerFun, median) && identical(scaleFun,
  #                                                    mad)) {
  #   scale <- mad(x, center = 0)
  # }
  # else scale <- scaleFun(x)
  # x <- x/scale
  (x - mean(x, ...))/sd(x, ...)
}


# Other -------------------------------------------------------------------

#' Some title
#'
#' @param x some param
#'
#' @export
box_cox <- function(x, lam1, lam2 = NULL) {

  lam2 <- ifelse(is.null(lam2), 0, lam2)
  if (lam1 == 0L) {
    log(y + lam2)
  } else {
    (((y + lam2)^lam1) - 1) / lam1
  }

}

#' Some title
#'
#' @param x some param
#'
#' @export
tukey <- function(x, lam) {
  y^lam
}

powerTransform <- function(y, lambda1, lambda2 = NULL, method = "boxcox") {

  boxcoxTrans <- function(x, lam1, lam2 = NULL) {

    # if we set lambda2 to zero, it becomes the one parameter transformation
    lam2 <- ifelse(is.null(lam2), 0, lam2)

    if (lam1 == 0L) {
      log(y + lam2)
    } else {
      (((y + lam2)^lam1) - 1) / lam1
    }
  }

  switch(method
         , boxcox = boxcoxTrans(y, lambda1, lambda2)
         , tukey = y^lambda1
  )
}




gmean <- function() {

}

bc1 <- function(U, lambda,  jacobian.adjusted = FALSE, gamma = NULL) {
  if (any(U[!is.na(U)] <= 0))
    stop("First argument must be strictly positive.")
  z <- if (abs(lambda) <= 1e-06)
    log(U)
  else ((U^lambda) - 1)/lambda
  if (jacobian.adjusted == TRUE) {
    z * (exp(mean(log(U), na.rm = TRUE)))^(1 - lambda)
  }
  else z
}

yj <- function(U, lambda, jacobian.adjusted = FALSE) {
  nonnegs <- U >= 0
  z <- rep(NA, length(U))
  z[which(nonnegs)] <- bcPower(U[which(nonnegs)] + 1, lambda,
                               jacobian.adjusted = FALSE)
  z[which(!nonnegs)] <- -bcPower(-U[which(!nonnegs)] +
                                   1, 2 - lambda, jacobian.adjusted = FALSE)
  if (jacobian.adjusted == TRUE)
    z * (exp(mean(log((1 + abs(U))^(2 * nonnegs - 1)),
                  na.rm = TRUE)))^(1 - lambda)
  else z
}

#' Some title
#'
#' @param x some param
#'
#' @export
yeo_johnson <- function(x) {

}
