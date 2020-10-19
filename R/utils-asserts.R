

# predicate ---------------------------------------------------------------

is_univariate <- function(x) {
  if(is.atomic(x) && is.numeric(x) && is.null(dim(x))) {
    TRUE
  }else{
    FALSE
  }
}


# TODO use abort, warn, signal


# asserts -----------------------------------------------------------------

asserts <- function(x, n = NULL, order = NULL, rho = NULL, seed = NULL) {
  assert_univariate(x)
  n %||% assert_positive_scalar(n)
  order %||% assert_positive_scalar(order)
  seed %||% assert_positive_scalar(seed)
  rho %||% assert_numeric(rho)
}

assert_na <- function(x) {
  if(!is.logical(x)) {
    stop("`na.rm` must be a logical value", call. = FALSE)
  }
}

asserts_fill <- function(n, fill, fill_fun) {
  assert_fill_lenth(n, fill)
  assert_fill_opts(fill, fill_fun)
}


# asserts_dtrend ----------------------------------------------------------

asserts_dtrend <- function(x, degree, pb = NULL, raw = NULL) {
  assert_univariate(x)
  assert_positive_scalar(degree)
  pb %||% assert_positive_vector(pb)
  raw %||% assert_logical(raw)
}

# individual --------------------------------------------------------------

# TODO have to enclose argument errors with backticks ``

assert_univariate <- function(x) {
  if (!is_univariate(x))  {
    stop(paste(deparse(substitute(x, env = parent.frame())),
               "must be a univariate series"), call. = FALSE)
  }
}

assert_logical <- function(x) {
  if(!is.logical(x)) {
    stop(paste(deparse(substitute(x, env = parent.frame())), "must be logical"), call. = FALSE)
  }
}

assert_numeric <- function(x) {
  if (!is.numeric(x))  {
    stop(paste(deparse(substitute(x, env = parent.frame())),
               "must be a numeric vector"), call. = FALSE)
  }
}

assert_fill_lenth <- function(n, fill) {
  if (length(fill) != n)
    stop("`fill` exceeds replacement length", call. = FALSE)
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

assert_fill_opts <- function(fill, fill_fun) {
  if (!is.na(fill)) {
    assert_numeric(fill)
  }
  if (!is.na(fill) && !is.null(fill_fun)) {
    stop("`fill` or `fill_fun` should be specified.", call. = FALSE)
  }
  if(!is.null(fill_fun) && !rlang::is_function(fill_fun) && !rlang::is_formula(fill_fun)) {
    stop("`fill_fun` should be a function.", call. = FALSE)
  }
}

assert_univariate_output <- function(x) {
  if (!is.numeric(x) && !typeof(x) %in% c("double", "integer"))  {
    stop(paste0("`fill_fun` coerces to ", typeof(x), ", output not a numeric vector."), call. = FALSE)
  }
}


