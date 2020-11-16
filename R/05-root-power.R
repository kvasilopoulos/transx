

# Basic Root --------------------------------------------------------------


#' nth Root Transformation
#'
#' @description
#'
#' * `root`: nth root
#' * `root_sqrt`: square root
#' * `root_cubic`: cubic root
#'
#' @template x
#' @param root `[numeric(1): NA]`
#'
#' The nth root.
#'
#' @param modulus `[logical(1): FALSE]`
#'
#' Transformation will work for data with both positive and negative `root`.
#'
#' @name root
root <- function(x, root = NULL, modulus = FALSE) {
  assert_uni_ts(x)
  if(modulus) {
    sign(x) * abs(x)^(1/root)
  }else{
    x^(1/root)
  }
  with_attrs(out, x)
}


#' @rdname root
#' @export
#' @param ... Further arguments passed to `root`.
root_sq <- function(x, ...) {
  root(x , 2, ...)
}

#' @rdname root
#' @export
root_cubic <- function(x, ...) {
  root(x, 3, ...)
}



# Box-cox family ----------------------------------------------------------

# lam <- forecast::BoxCox.lambda(x)
# forecast::BoxCox(x, lam)
# pow_box_cox(x, lam)


#' Exponent or the nth Power
#'
#' @template x
#' @param pow `[numeric(1): NA]`
#'
#' The nth power
#'
#' @param modulus positive
#' @template return
#'
pow <- function(x, pow = NA, modulus = FALSE) {
  assert_uni_ts(x)
  out <- pow_(x, pow, modulus = modulus)
  with_attrs(out, x)
}

pow_ <- function(x, pow = NA, modulus = FALSE) {
  if(modulus) {
    out <- sign(x) * abs(x)^pow
  }else{
    out <- x^pow
  }
  out
}



#' Tukey Transformations
#'
#' @template x
#'
#' @param lambda `[numeric(1): NULL]`
#'
#' Transformation exponent, \eqn{\lambda}.
#'
#' @param ...
#'
#' Further arguments passed to `pow`.
#'
#' @template return
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#'
pow_tukey <- function(x, lambda = NULL, ...) {
  assert_uni_ts(x)
  if (lambda >  0){
    out <- pow_(x, lambda, ...)
  } else if(abs(lambda) <= 1e-06){
    out <- log(x)
  }else {
    out <- -1 * pow_(x, lambda)
  }
  with_attrs(out, x)
}

#' Box-Cox Transformations
#'
#' @template x
#'
#' @param lambda `[numeric(1): NULL]`
#'
#' Transformation exponent, \eqn{\lambda}.
#'
#' @param lambda2 `[numeric(1): NULL]`
#'
#' Transformation exponent, \eqn{\lambda_2}.
#'
#' @param ...
#'
#' Further arguments passed to `pow`.
#'
#' @template return
#' @export
#'
#' @references Box, G. E., & Cox, D. R. (1964). An analysis of transformations.
#' Journal of the Royal Statistical Society. Series B (Methodological), 211-252.
#' \url{https://www.jstor.org/stable/2984418}
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#'
pow_boxcox <- function(x, lambda = NULL, lambda2 = NULL, ...) {

  assert_uni_ts(x)
  lambda2 <- ifelse(is.null(lambda2), 0, lambda2)
  if (abs(lambda) <= 1e-06) {
    out <- log(x + lambda2)
  } else {
    out <- (pow_(x + lambda2, lambda, ...) - 1) / lambda
  }
  with_attrs(out, x)
}

#' Yeo and Johnson(2000)
#'
#' @param lambda `[numeric(1): NULL]`
#'
#' Transformation exponent, \eqn{\lambda}.
#'
#' @param ...
#'
#' Further arguments passed to `pow`.
#' @template return
#'
#' @references Yeo, I., & Johnson, R. (2000).
#' A New Family of Power Transformations to Improve Normality or Symmetry. Biometrika, 87(4), 954-959.
#' \url{http://www.jstor.org/stable/2673623}
#'
#' @export
pow_yj <- function(x, lambda = NULL, ...) {

  assert_uni_ts(x)
  eps <- 1e-06
  if (abs(lambda) < eps) {
    out <- log(x + 1)
  } else {
    out <- (pow_(x + 1, lamda, ...) - 1)/lamda
  }
  if (abs(2 - lamda) < eps) {
    out <-  -log(-x + 1)
  }  else {
    out <- -(pow_(-x + 1, 2 - lamda, ...) - 1)/(2 -lamda)
  }
  with_attrs(out, x)
}


#' Manly(1971)
#'
#' The transformation was reported to be successful in transform
#' unimodal skewed distribution into normal distribution, but is
#' not quite useful for bimodal or U-shaped distribution.
#'
#' @template x
#' @param lambda `[numeric(1): NULL]`
#'
#' Transformation exponent, \eqn{\lambda}.
#'
#' @template return
#' @export
pow_manly <- function(x, lambda) {
  assert_uni_ts(x)
  if(abs(lambda) < 1e-06) {
    out <- x
  }else{
    out <- (exp(lambda*x) - 1)/lambda
  }
  with_attrs(out, x)
}



# more power transformations ----------------------------------------------

# pow_box_cox_scaled <- function(x, lam1, lam2) {}

# pow_tukey_lambda <- function(x) {}
#
# pow_john_draper <- function(x) {}
#
# pow_bickel_doksum <- function(x) {}

# boxcoxTrans <- function(x, lambda1, lambda2 = NULL) {
#
#   # if we set lambda2 to zero, it becomes the one parameter transformation
#   lam2 <- ifelse(is.null(lambda2), 0, lambda2)
#
#   if (lam1bda == 0L) {
#     log(y + lambda2)
#   } else {
#     (((y + lambda2)^lambda1) - 1) / lambda1
#   }
# }
#
# powTransform <- function(y, lambda1, lambda2 = NULL, method = "boxcox") {
#
#   switch(
#     method,
#     boxcox = boxcoxTrans(y, lambda1, lambda2),
#     tukey = y^lambda1
#   )
# }
#
#
#
# bc1 <- function(U, lambda,  jacobian.adjusted = FALSE, gamma = NULL) {
#   if (any(U[!is.na(U)] <= 0))
#     stop("First argument must be strictly positive.")
#   z <- if (abs(lambda) <= 1e-06)
#     log(U)
#   else ((U^lambda) - 1)/lambda
#   if (jacobian.adjusted == TRUE) {
#     z * (exp(mean(log(U), na.rm = TRUE)))^(1 - lambda)
#   }
#   else z
# }
#
# yj <- function(U, lambda, jacobian.adjusted = FALSE) {
#   nonnegs <- U >= 0
#   z <- rep(NA, length(U))
#   z[which(nonnegs)] <- bcPower(U[which(nonnegs)] + 1, lambda,
#                                jacobian.adjusted = FALSE)
#   z[which(!nonnegs)] <- -bcPower(-U[which(!nonnegs)] +
#                                    1, 2 - lambda, jacobian.adjusted = FALSE)
#   if (jacobian.adjusted == TRUE)
#     z * (exp(mean(log((1 + abs(U))^(2 * nonnegs - 1)),
#                   na.rm = TRUE)))^(1 - lambda)
#   else z
# }

