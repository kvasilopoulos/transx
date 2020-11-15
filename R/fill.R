
check_idx <- function(x) {
  !rlang::is_bare_atomic(x, 0)
}

body_ <- function(x, idx) {

  if(!check_idx(idx)) {
    return(x)
  }
  if(is.numeric(idx)) {
    out <- x[-idx]
  }
  if(is.logical(idx)) {
    if(length(x)!=length(idx)) {
      stop("The idx is going to recycle.") # more of an internal check
    }
    out <- x[!idx]
  }
  out
}

# TODO we have to rethink it
# Fill can be nnumeric(1) or numeric(n):
# Can be a lambda_function or a function
# if it a lambda then fill_fail = NA
# remake with new functionality
# more tests and error-handling
# 4 cases
# 1. numeric(1)
# 2. numeric(n)
# 3. mean
# 4. fill_locf(body, idx fail)

#' @importFrom rlang as_function is_formula is_function
#' @noRd
#' @examples
#' fill_(c(2,3,4,5), 1, mean)
#'
#'
fill_  <- function(body, idx, fill) {

  if(!check_idx(idx)) {
    disp_info("fill option is not available")
    return(seq_along(body))
  }
  if(is_formula(fill)) {
    fill <- as_function(fill)
  }
  vec <- new_vec(body, idx)
  if(is_function(fill)) {
    if(any(rlang::fn_fmls_names(fill) %in% c("idx", ".y"))) {
      vec[idx] <- fill(body, idx)
      # print("hooray")
    }else {
      vec[idx] <- fill(body)
    }
  }else{
    vec[idx] <- fill
  }
  disp_info("Filling {length(idx)} value{?s}.", .envir = parent.frame())
  vec
}

# TODO possibly needs new name
new_vec <- function(body, idx, default = NA_real_) {
  vec_len <- length(body) + length(idx)
  vec <- rep(default, vec_len) # vector("numeric", vec_len)
  vec[-idx] <- body
  vec
}


# impute ------------------------------------------------------------------

# this will be used latern for roll_mean etc
# impute_idx <- function(x, idx, fn) {}

# impute_na <- function(x) {
#   idx <- is.na(x)
#   impute_idx(x, idx)
# }

# idx functions -----------------------------------------------------------

# idx_enc <- function(x) {
#   start <- x[c(TRUE, diff(x) != 1)]
#   end <- x[c(diff(x) != 1, TRUE)]
#   duration <- end - start  + 1
#   list(start = start, end = end, duration = duration)
# }
#
# idx_locf <- function(idx) {
#   enc <- idx_enc(idx)
#   rep(enc$start - 1, enc$duration)
# }
#
# idx_nocb <- function(idx) {
#   enc <- idx_enc(idx)
#   rep(enc$end + 1, enc$duration)
# }


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
#' lagx(x, n = 2, fill = fill_locf)
#' leadx(x, n = 2, fill = fill_locf)
#'
#' lagx(x, n = 2, fill = fill_nocb)
#' leadx(x, n = 2, fill = fill_nocb)
#'
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
  # vec
}
#' @noRd
#' @examples
#' first(c(1,2,3))
#' first(c(1,2,4))
#' first(c(2,3,4))
#'
first <- function(x) {
  n1 <-if(x[1] == 1)  1 else 0
  if(n1 == 0) {}
    return(vector(length = length(x)))
  c(n1, diff(x)) == 1
}

#' @noRd
#' @examples
#' first(c(1,2,3))
#' first(c(1,2,4))
#' first(c(2,3,4))
#'
last <- function(x, n) {
  nn <- if(x[n] == n) 1 else 0
  nn
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
#' fill_locf(body, idx, NA)
#'
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

# TODO fill both sides
# fill_both <- function(body, idx, default = NULL) {
#   vec <- new_vec(body, idx)
#   n <- length(vec)
#   # idx and rev(idx)
#   for (i in idx) {
#     if(i == 1) {
#       vec[i] <- vec[i + 1]
#     }else {
#       vec[i] <- vec[i - 1]
#     }
#   }
#   vec[idx]
# }


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


# Kalman filter -----------------------------------------------------------

fill_kalman <- function(body, idx, ...) {
  mod <- stats::StructTS(body)$model0
  kal <- stats::KalmanSmooth(body, mod)
  erg <- kal$smooth
  msidx <- erg[idx, , drop = TRUE] %*% as.matrix(mod$Z)
  x[idx] <- msidx
}
