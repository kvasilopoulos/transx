
check_idx <- function(idx) {
  if(length(idx) == 0) {
    FALSE
  }else{
    TRUE
  }
}

#' @importFrom rlang as_function is_formula
fill_  <- function(body, idx, fill, fill_fun, ...) {
  idx_ok <- check_idx(idx)
  if(!idx_ok) {
    display("transformation is not available")
    return(seq_along(x))
  }
  if(is_formula(fill_fun)) {
    fill_fun <- as_function(fill_fun)
  }
  vec <- new_vec(body, idx)
  if (!is.null(fill_fun)) {
    if (is.null(formals(fill_fun)$idx)) {
      vec[idx] <- fill_fun(body, ...) # user_defined function like mean
    }else{
      vec[idx] <- fill_fun(vec, body, idx, ...) #created function
    }
  }else{
    vec[idx] <- fill
  }
  assert_univariate_output(vec)
  vec
}

# TODO vec is not necessary to use in fill_fun
# TODO we can create vec inside the function

new_vec <- function(body, idx) {
  vec_len <- length(body) + length(idx)
  vec <- vector("numeric", vec_len)
  vec[-idx] <- body
  vec
}


# impute ------------------------------------------------------------------

impute_idx <- function(x, idx) {

}

impute_na <- function(x) {
  idx <- is.na(x)
  impute_idx(x, idx)

}

# idx functions -----------------------------------------------------------

idx_enc <- function(x) {
  start <- x[c(TRUE, diff(x) != 1)]
  end <- x[c(diff(x) != 1, TRUE)]
  duration <- end - start  + 1
  list(start = start, end = end, duration = duration)
}

idx_locf <- function(idx) {
  enc <- idx_enc(idx)
  rep(enc$start - 1, enc$duration)
}

idx_nocb <- function(idx) {
  enc <- idx_enc(idx)
  rep(enc$end + 1, enc$duration)
}

idx_both <- function(idx) {

}

# fill functions ----------------------------------------------------------

#' Fill with `Last Observation Carried Forward`
#'
#'
#' @export
fill_locf <- function(body, idx) {
  vec <- new_vec(body, idx)
  n <- length(vec)
  for (i in idx_locf(idx)) {
    vec[i] <- vec[i - 1]
  }
  vec[idx]
}



# TODO does this work??
#' Next observation carried backwards
#' @export
fill_nocb <- function(body, idx) {
  vec <- new_vec(body, idx)
  for (i in rev(idx)) {
    vec[i] <- vec[i + 1]
  }
  vec[idx]
}

fill_both <- function(body, idx) {
  vec <- fill_locf(body, idx)
  vec <- fill_nocb(body, idx)
  vec
}


#' Fill with linear approximation
#'
#' @export
#' @template return-template
fill_linear <- function(vec, body, idx) {
  stats::approx(idx, vec, xout = 1:n, rule = 2, ...)$y
}

#' Fill with cubic spline interpolation
#'
#' @export

fill_spline <- function(vec, body, idx) {
  stats::spline(idx, vec, n = n, ...)$y
}


# na.approx.vec <- function(x, y, xout = x, ...) {
#   na <- is.na(y)
#   if (sum(!na) < 2L) {
#     yf <- rep.int(NA, length(xout))
#     mode(yf) <- mode(y)
#     if (any(!na)) {
#       if (x[!na] %in% xout) {
#         yf[xout == x[!na]] <- y[!na]
#       }
#     }
#     return(yf)
#   }
#   if (all(!na) && (length(xout) > maxgap) && !all(xout %in%
#                                                   x)) {
#     xf <- sort(unique(c(x, xout)))
#     yf <- rep.int(NA, length(xf))
#     yf[MATCH(x, xf)] <- y
#     x <- xf
#     y <- yf
#   }
#   yf <- approx(x[!na], y[!na], xout, ...)$y
#   if (maxgap < length(y)) {
#     ygap <- .fill_short_gaps(y, seq_along(y), maxgap = maxgap)
#     ix <- approx(x, seq_along(y), xout, ...)$y
#     yx <- ifelse(is.na(ygap[floor(ix)] + ygap[ceiling(ix)]),
#                  NA, yf)
#     yx
#   }
#   else {
#     yf
#   }
# }



