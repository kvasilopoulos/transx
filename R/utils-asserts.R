

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

asserts_fill <- function(n, fill, fill_fun) {
  assert_fill_lenth(n, fill)
  assert_fill_opts(fill, fill_fun)
}


assert_fill_lenth <- function(n, fill) {
  if (length(fill) > 1 && length(fill) != n)
    stop("`fill` exceeds replacement length", call. = FALSE)
}

assert_fill_opts <- function(fill, fill_fun) {
  if (length(fill) == 1 && !is.na(fill)) {
    assert_numeric(fill)
  }
  if(!is.na(fill) && !is.null(fill_fun)) {
    fn_names <- rlang::fn_fmls_names(fill_fun)
    if(!all(fn_names %in% c("body", "idx", "default","..."))) {
      stop("`fill` or `fill_fun` should be specified together unless for fill_* function.",
           call. = FALSE)
    }
  }

  if(!is.null(fill_fun) && !rlang::is_function(fill_fun) && !rlang::is_formula(fill_fun)) {
    stop("`fill_fun` should be a function.", call. = FALSE)
  }
}


# individual --------------------------------------------------------------

# TODO have to enclose argument errors with backticks ``

assert_uni_ts <- function(x) {
  if (!is_uni_ts(x))  {
    stop(paste(deparse(substitute(x, env = parent.frame())), "must be a univariate series"), call. = FALSE)
  }
}


assert_uni_ts_output <- function(x) {
  if (!is.numeric(x) && !typeof(x) %in% c("double", "integer"))  {
    stop(paste0("`fill_fun` coerces to ", typeof(x), ", output not a numeric vector."), call. = FALSE)
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





