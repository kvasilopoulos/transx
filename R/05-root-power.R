



# Basic Root --------------------------------------------------------------



root <- function(x, root, modulus = FALSE) {
  if(modulus) {
    sign(x) * abs(x)^(1/root)
  }else{
    x^(1/root)
  }
}

#' Square root transformation
#'
#' @template x
#' @param ...
#'
#' Further arguments passed to `root`.
#'
root_sq <- function(x, ...) {
  root(x , 2, ...)
}

#' Cubic root transformation
#'
#' @inheritParams root_sq
#'
root_cubic <- function(x, ...) {
  root(x, 3, ...)
}




# Box-cox family ----------------------------------------------------------

# lam <- forecast::BoxCox.lambda(x)
# forecast::BoxCox(x, lam)
# pow_box_cox(x, lam)


#' Raise in the power
#'
#' @template x
#' @param pow power to raise to
#' @param modulus positive
#' @template return
#'
pow <- function(x, pow, modulus = FALSE) {
  if(modulus) {
    sign(x) * abs(x)^pow
  }else{
    x^pow
  }
}




#' Some title
#'
#' @template x
#' @param lam1 lambda1
#' @param lam2 lambda2
#' @template return
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#'
pow_boxcox <- function(x, lam1, lam2 = NULL) {

  lam2 <- ifelse(is.null(lam2), 0, lam2)
  if (lam1 == 0L) {
    log(x + lam2)
  } else {
    (pow(x, lam1) - 1) / lam1
  }

}

pow_tukey <- function(x, lambda = NULL) {
  out <- pow_tukey_(x, lambda)
  with_attrs(out, x)
}

pow_tukey_ <- function(x, lambda = NULL) {
  if (lambda >  0){
    out <- pow(x, lambda)
  } else if(lambda == 0){
    out <- log(x)
  }else {
    out <- -1 * pow(x, lambda)
  }
  out
}


#' Yeo and Johnson(2000)
#'
#' @template x
#' @param lam lambda
#' @template return
#'
#' @export
pow_yj <- function(x, lam = NULL) {

}


#' Manly(1971)
#'
#' The transformation was reported to be successful in transform
#' unimodal skewed distribution into normal distribution, but is
#' not quite useful for bimodal or U-shaped distribution.
#'
#' @template x
#' @param lam lambda
#' @template return
#' @export
pow_manly <- function(x, lam) {
  if(lam == 0) {
    out <- x
  }else{
    out <- (exp(lam*x) - 1)/lam
  }
  out
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

