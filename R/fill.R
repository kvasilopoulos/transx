
impute <- function(x, idx, fn ,...) {
  x[idx] <- fn(x)
  x
}

impute_na <- function(x, fn, ...) {
  out <- x
  na <- is.na(x)
  x <- x[!na]
  out[na] <- fn(x, ...)
  out
}


# fill_ -------------------------------------------------------------------


check_idx <- function(x) {
  !rlang::is_bare_atomic(x, 0)
}

nidx <- function(x) {
  if(!check_idx(x)) {
    return(0)
  }
  if(is.logical(x)) {
    return(sum(x))
  }
  length(x)
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

is_identity <- function(fn) {
  x <- c(-10,2,4,10) # random numnbers
  all(fn(x) == x)
}

# is_fill_fn <- function(x) {
#   if(is_function(fill) && any(rlang::fn_fmls_names(fill) %in% c("idx", ".y"))) {
#      return(TRUE)
#   }
#   FALSE
# }


#' @importFrom rlang as_function is_formula is_function
#' @noRd
#' @examples
#' fill_(c(2,3,4,5), 1, mean)
#'
#'
fill_  <- function(body, idx, fill, msg_on_na = NULL, msg_on_success = NULL, internal = FALSE) {

  if(!check_idx(idx)) {
    if(is.null(msg_on_na)) {
      disp_info("Fill option is not available", internal = internal)
    }else{
      stopifnot(is.character(msg_on_na))
      msg_on_na
    }
    return(body)
  }
  if(is_formula(fill)) {
    fill <- as_function(fill)
  }
  vec <- new_vec(body, idx)
  if(is_function(fill)) {

    if(any(rlang::fn_fmls_names(fill) %in% c("idx", ".y"))) { #work on this part
      vec[idx] <- fill(body, idx)
    }else {
      if(is_identity(fill)) { # not in the specs but whatever
        return(vec[-idx])
      }
      vec[idx] <- fill(body)
    }
  }else{
    vec[idx] <- fill
  }
  if(is.null(msg_on_success)) {
    disp_success("Filling {length(idx)} value{?s}.", internal = internal)
  }else{
    msg_on_success
  }
  vec
}

new_vec <- function(body, idx, default = NA_real_) {
  vec_len <- length(body) + length(idx)
  vec <- rep(default, vec_len) # vector("numeric", vec_len)
  vec[-idx] <- body
  vec
}


# fill vector -------------------------------------------------------------

#' @examples
#'
#' lagx(c(1:5), fill = ~ fill_window(.x, .y, roll_mean))
fill_window <- function(body, idx, fn, ...) {
  fill_vec(body, idx, fn(body, ...))
}

#' Replaces
#'
#'
#' @examples
#'
#' lagx(c(1:5), fill = ~ fill_vec(.x, .y, 1:5))
#'
#' lagx(c(1:5), fill = ~ fill_vec(.x, .y, roll_mean(.x)))
fill_vec <- function(body, idx, vec) {
  vec[idx]
}


