

# predicate ---------------------------------------------------------------

is_uni_ts <- function(x) {
  if(is.atomic(x) && is.numeric(x) && is.null(dim(x))) {
    TRUE
  }else{
    FALSE
  }
}

# assert-basic ------------------------------------------------------------

#'@importFrom rlang %||%
assert_lx <- function(x, n) {
  assert_uni_ts(x)
  assert_positive_scalar(n)
}

asserts_diff <- function(x, n, order, rho = NULL) {
  assert_uni_ts(x)
  assert_positive_scalar(n)
  assert_positive_scalar(order)
  # Optional arguments
  # seed %||% assert_positive_scalar(seed)
  rho %||% assert_numeric(rho)
}

# asserts_dtrend ----------------------------------------------------------

asserts_dtrend <- function(x, degree = NULL, bp = NULL, raw = NULL) {
  assert_uni_ts(x)
  degree %!||% assert_positive_scalar(degree)
  bp %!||% assert_positive_scalar(bp)
  raw %!||% assert_logical(raw)
}


# generic -----------------------------------------------------------------

assert_na <- function(x) {
  if(!is.logical(x)) {
    stop("`na.rm` must be a logical value", call. = FALSE)
  }
}

# fill --------------------------------------------------------------------

#' @importFrom rlang is_function is_na is_formula as_function
asserts_fill <- function(n, fill) {
  if (is.numeric(fill) && length(fill) > 1 && length(fill) != n) {
    stop("`fill` exceeds replacement length", call. = FALSE)
  }
  if(is_formula(fill)) {
    fill <- as_function(fill)
  }
  if(!is_function(fill) && !is.numeric(fill) && !rlang::is_na(fill)) { # is.na because some times it assu
    stop("fill should be numeric vector or a function")
  }
}

# individual --------------------------------------------------------------

# TODO have to enclose argument errors with backticks ``

assert_uni_ts <- function(x) {
  if (!is_uni_ts(x))  {
    stop(paste(deparse(substitute(x, env = parent.frame())), "must be a univariate series"), call. = FALSE)
  }
}

assert_logical <- function(x) {
  if(!is.logical(x)) {
    stop(paste(deparse(substitute(x, env = parent.frame())), "must be logical"), call. = FALSE)
  }
}

assert_numeric <- function(x) {
  if (!is.numeric(x))  {
    stop(paste(deparse(substitute(x, env = parent.frame())), "must be a numeric vector"), call. = FALSE)
  }
}


assert_positive_scalar <- function(n) {
  if (length(n) != 1L || !is.numeric(n) || n < 0L)  {
    stop("`n` must be a nonnegative integer scalar", call. = FALSE)
  }
}

assert_positive_vector <- function(x) {
  if(!is.numeric(x) || any(x>0)) {
    stop(paste(deparse(substitute(x, env = parent.frame())),
               "must be a positive vector"), call. = FALSE)
  }
}





