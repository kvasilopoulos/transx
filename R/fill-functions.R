# fill functions ----------------------------------------------------------

#' Fill with "Last Observation Carried Forward"
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
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
#' # A not so very neat way to deal with NA when `fill_locf` fails is (WIP)
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
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
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
#' fill_both(body, idx, first = "nocb")
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
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#' @inheritParams fill_locf
#'
#' @param first `[character: "locf"]`
#'
#' Select which filling algorithms will occur first "locf" or "nocb".
#'
#' @template return
#'
#' @examples
#'
#' leadx(1:4, fill = fill_both)
#' leadx(1:4, fill = ~ fill_both(.x,.y, first = "nocb"))
#'
#' lagx(1:4, fill = fill_both)
#' lagx(1:4, fill = ~ fill_both(.x,.y, first = "nocb"))
#'
#' set.seed(123)
#' x <- rnorm(10)
#' smooth_ma(x, 4, fill = fill_both)
#' @export
fill_both <- function(body, idx, first = c("locf", "nocb")) {

  first <- match.arg(first)
  if(first[1] == "locf") {
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

  vec2 <-  new_vec(body2, idx2)
  vec2[idx2] <- fn2(body2, idx2)
  vec2[union(idx, idx2)]
}


#' Fill with "linear approximation"
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
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
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
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


fill_vec_ <- function(body, idx, vec) {
  vec[idx]
}

#' Fill with values
#'
#' @description
#'
#' `r rlang:::lifecycle("experimental")`
#'
#' @param vec `[numeric]`
#'
#' Numeric vector of the same length
#'
#' @examples
#'
#' lagx(c(1:5), fill = fill_vec(1:5))
#' \dontrun{
#'
#' lagx(c(1:5), fill = fill_window(roll_mean(.x)))
#' }
#' @export
fill_vec <- function(vec) {
  ~ fill_vec_(.x, .y, vec)
}

#' Fill window functions
#'
#' @description
#'
#' `r rlang:::lifecycle("experimental")`
#'
#' @param fn `[function]`
#'
#' Window function, usually of the `roll`, `rec` or `blck` families.
#'
#' @param ... Further arguments passed to `fn`.
#'
#'
#' @examples
#' \dontrun{
#' lagx(c(1:5), fill = fill_window(rec_mean))
#' }
#' @export
fill_window <- function(fn, ...) {
  fn <- as_fn(fn, ...)
  ~ fill_vec_(.x, .y , fn(.x))
}


# TODO make better fill documentation
# TODO asserts in new fill_functions
# TODO make more fill test

# Kalman filter -----------------------------------------------------------

# fill_kalman <- function(body, idx, ...) {
#   mod <- stats::StructTS(body)$model0
#   kal <- stats::KalmanSmooth(body, mod)
#   erg <- kal$smooth
#   msidx <- erg[idx, , drop = TRUE] %*% as.matrix(mod$Z)
#   x[idx] <- msidx
# }
