# Power Transformation ----------------------------------------------------



#' Square root transformation
#'
#'
power_sqrt <- function(x) {
  sqrt(x)
}


#' Cubic root transformation
#'
#'
power_cube <- function(x) {
  sign(x) * abs(x)^(1/3)
}



#' Some title
#'
#' @param x some param
#'
#' @export
power_box_cox <- function(x, lam1, lam2 = NULL) {

  lam2 <- ifelse(is.null(lam2), 0, lam2)
  if (lam1 == 0L) {
    log(y + lam2)
  } else {
    (((y + lam2)^lam1) - 1) / lam1
  }

}

power_tukey <- function(x, lambda = NULL) {
  out <- power_tukey_(x, lambda)
  out_atrrs(out, x)
}

power_tukey_ <- function(x, lambda = NULL) {
  # if (lambda >  0){
  #   out <- x ^ lambda
  # } else if (lambda == 0){
  #   out <- log(x)
  # } else  (lambda <  0){
  #   out <- -1 * x ^ lambda
  # }
  out
}




#' Yeo and Johnson(2000)
#'
#' @param x some param
#'
#' @export
power_yeo_johnson <- function(x, lambda = NULL) {

}


#' Manly(1971)
#'
#' The transformation was reported to be successful in transform
#' unimodal skewed distribution into normal distribution, but is
#' not quite useful for bimodal or U-shaped distribution.
#'
#'
power_manly <- function(x) {
  if(lambda == 0) {
    out <- x
  }else{
    out <- (exp(lambda*y) - 1)/lambda
  }
}



# power_box_cox_scaled <- function(x, lam1, lam2) {}

# power_tukey_lambda <- function(x) {}
#
# power_john_draper <- function(x) {}
#
# power_bickel_doksum <- function(x) {}

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
# powerTransform <- function(y, lambda1, lambda2 = NULL, method = "boxcox") {
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

