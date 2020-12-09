# fill functions ----------------------------------------------------------

#' Fill with "Last Observation Carried Forward"
#'
#'
#' @param body `[numeric vector]`
#'
#' The body of the vector.
#'
#' @param idx `[integer vector]`
#'
#' the index to replace with.
#'
#' @param fail `[numeric(1) or numeric vector: fill]`
#'
#' In case it fails to fill some values.
#'
#' @export
#' @template return
#' @examples
#' x <- c(5,3,2,2,5)
#'
#' lagx(x, n = 2, fill = fill_locf)
#'
#' # A not so very neat way to deal with NA when `fill_locf` fails is
#' lagx(x, n = 2, fill = ~ fill_locf(.x,.y, fail = 0))
#'
#' leadx(x, n = 2, fill = fill_locf)
#'
#' lagx(x, n = 2, fill = fill_nocb)
#'
#' leadx(x, n = 2, fill = fill_nocb)
#'
#' leadx(x, n = 2, fill = ~ fill_nocb(.x,.y, fail = 0))
fill_locf <- function(body, idx, fail = NA) {
  vec <- new_vec(body, idx)
  n <- length(vec)
  for (i in idx) {
    # cannot index first obs
    if(i == 1) {
      vec[i] <- fail
    }else {
      vec[i] <- vec[i - 1]
    }
  }
  vec[idx]
}



#' Fill with "Next observation carried backwards"
#'
#' @inheritParams fill_locf
#'
#' @export
#' @template return
#' @examples
#' x <- c(5,3,2,2,5)
#' leadx(x, n = 2, fill = fill_locf)
#'
#' xlen <- length(x)
#' n <- 2
#' n <- pmin(n, xlen)
#' idx <- (xlen - n + 1):xlen
#' body <- x[-seq_len(n)]
#' fill_nocb(body, idx, NA)
#' fill_both(body, idx, order = "nocb")
#'
#'
fill_nocb <- function(body, idx, fail = NA){
  vec <- new_vec(body, idx)
  for (i in rev(idx)) { # reverse order
    # cannot index last observation
    if(i == length(vec)) {
      vec[i] <- fail
    }else{
      vec[i] <- vec[i + 1]
    }
  }
  vec[idx]
}

#' Fill with `locf` and `nocb`
#'
#' @inheritParams fill_locf
#' @param order `[character: "locf"]`
#'
#' Select which filling algorithms will occur first "locf" or "nocb".
#'
#' @template return
#'
#' @examples
#'
#' leadx(1:4, fill = fill_both)
#' leadx(1:4, fill = ~ fill_both(.x,.y, order = "nocb"))
#'
#' lagx(1:4, fill = fill_both)
#' lagx(1:4, fill = ~ fill_both(.x,.y, order = "nocb"))
#'
#'
fill_both <- function(body, idx, order = c("locf", "nocb")) {

  order <- match.arg(order)
  if(order[1] == "locf") {
    fn1 <- fill_locf
    fn2 <- fill_nocb
  }else{
    fn1 <- fill_nocb
    fn2 <- fill_locf
  }
  vec <-  new_vec(body, idx)
  vec[idx] <- fn1(body, idx, fail = NA)

  idx2 <- which(is.na(vec))
  if(rlang::is_bare_integer(idx2, 0))
    return(vec[idx])
  body2 <- body_(vec, idx2)

  vec2[idx2] <- fn2(body2, idx2)
  vec2[union(idx, idx2)]
}


#' Fill with "linear approximation"
#'
#' @inheritParams fill_locf
#' @param ...
#'
#' Further arguments passed to `\link[stats]{approx}`
#'
#' @export
#' @template return
#' @examples
#' x <- c(5,3,2,2,5)
#' xlen <- length(x)
#' n <- 2
#' n <- pmin(n, xlen)
#' idx <- 1:n
#' body <- x[seq_len(xlen - n)]
#' fill_linear(body, idx)
#'
fill_linear <- function(body, idx, ...) {
  vec <- new_vec(body, idx)
  all_idx <- 1:length(vec)
  stats::approx(all_idx[-idx], vec[-idx], xout = all_idx, rule = 2, ...)$y[idx]
}

#' Fill with "cubic spline interpolation"
#'
#' @inheritParams fill_locf
#' @param ...
#'
#' Further arguments passed to `\link[stats]{spline}`
#'
#' @template return
#' @export
#' @examples
#' x <- c(5,3,NA,2,5)
#' fill_spline(x, 3)
fill_spline <- function(body, idx, ...) {
  vec <- new_vec(body, idx)
  n <- length(vec)
  all_idx <- 1:n
  stats::spline(all_idx[-idx], vec[-idx], n = n, ...)$y[idx]
}



# fill vector -------------------------------------------------------------

#' Replaces
#'
#'
#' @examples
#'
#' lagx(c(1:5), fill = ~ fill_vec(.x, .y, 1:5))
#'
#' lagx(c(1:5), fill = ~ fill_vec(.x, .y, roll_mean(.x)))
fill_vec_ <- function(body, idx, vec) {
  vec[idx]
}

#' @examples
#'
#' lagx(c(1:5), fill = fill_window2(rec_mean))
#'
fill_window <- function(fn, ...) {
  fn <- as_fn(fn, ...)
  ~ fill_vec_(.x, .y , fn(.x))
}


#' @examples
#'
#' lagx(c(1:5), fill = fill_vec2(1:5))
#'
fill_vec <- function(vec) {
  ~ fill_vec_(.x, .y, vec)
}

# TODO asserts in new fill_functions


# Kalman filter -----------------------------------------------------------

# @noRd
# @examples
# first(c(1,2,3))
# first(c(1,2,4))
# first(c(2,3,4))
#
# first <- function(x) {
#   n1 <-if(x[1] == 1)  1 else 0
#   if(n1 == 0) {}
#   return(vector(length = length(x)))
#   c(n1, diff(x)) == 1
# }

# @noRd
# @examples
# first(c(1,2,3))
# first(c(1,2,4))
# first(c(2,3,4))
#
# last <- function(x, n) {
#   nn <- if(x[n] == n) 1 else 0
#   nn
# }


# fill_kalman <- function(body, idx, ...) {
#   mod <- stats::StructTS(body)$model0
#   kal <- stats::KalmanSmooth(body, mod)
#   erg <- kal$smooth
#   msidx <- erg[idx, , drop = TRUE] %*% as.matrix(mod$Z)
#   x[idx] <- msidx
# }
