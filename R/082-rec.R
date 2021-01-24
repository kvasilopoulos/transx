
# https://community.rstudio.com/t/tidiest-way-to-do-recursion-safely-in-r/1408

# TODO handle NA
# TODO handle init
# TODO step?

# align = "left"

# idx -------------------------------------------------------------------


#' @examples
#'
#' # x is odd
#'
#' rec_idx(1:9, 2, 1, align = "left")
#' rec_idx(1:9, 2, 1, align = "center")
#' rec_idx(1:9, 2, 1, align = "right") # reduces
#'
#' # This works
#' rec_idx(1:9, 3, 1, align = "left")
#' rec_idx(1:9, 3, 1, align = "center")
#' rec_idx(1:9, 3, 1, align = "right")
#'
#' rec_idx(1:9, 2, 2, align = "left")
#' rec_idx(1:9, 2, 2, align = "center")
#' rec_idx(1:9, 2, 2, align = "right")
#'
#' rec_idx(1:9, 3, 2, align = "left")
#' rec_idx(1:9, 3, 2, align = "center")
#' rec_idx(1:9, 3, 2, align = "right") # reduces
#'
#' # x is even
#'
#' rec_idx(1:8, 2, 2, align = "left")
#' rec_idx(1:8, 2, 2, align = "center")
#' rec_idx(1:8, 2, 2, align = "right") # reduces
#'
#' rec_idx(1:8, 2, 1, align = "left")
#' rec_idx(1:8, 2, 1, align = "center")
#' rec_idx(1:8, 2, 1, align = "right")
#'
#' rec_idx(1:8, 3, 1, align = "left")
#' rec_idx(1:8, 3, 1, align = "center")
#' rec_idx(1:8, 3, 1, align = "right") # reduces
#'
#' rec_idx(1:8, 3, 1, align = "left")
#' rec_idx(1:8, 3, 1, align = "center")
#' rec_idx(1:8, 3, 1, align = "right") # reduces
#'
rec_idx <- function(x, step = 1, init = 1, align = c("left", "center", "right")) {

  xlen <- length(x)
  if(is_even(step) & align == "center")
    disp_info("`center` alignment collapses to `left` when there is an odd number of observations.")
  if(align == "right") {
    len_diff <- length(seq.int(init, xlen, by = step)) - length(seq.int(init + step - 1, xlen, by = step))
    if(len_diff != 0) {
      disp_info("To retain the sample size, right` alignment give one less observation.")
    }
  }

  align <- match.arg(align)
  begin <- switch(
    align,
    left = init,
    center = init + ceiling(step/2) - 1 ,
    right = init + step -1
  )
  seq.int(begin, xlen, by = step)
}

rec_fill_idx <- function(x, step = 1, init = 0, align= c("left", "center", "right")) {
  xlen <- length(x)
  xseq <- suppressMessages(rec_idx(x, step, init = init, align = align))
  which(!1:xlen %in% xseq)
}

# main --------------------------------------------------------------------


rec_data <- function(x, step = 1, init = 1, align = c("left", "center", "right")) {
  align <- match.arg(align)
  idx <- rec_idx(x,  step, init = init, align = align)
  sapply(idx, function(n, y) y[1:n], y = x)
}

rec <- function(x, fn, step = 1, init = 1, align = c("left", "center", "right"), fill = NA, ...) {
  align <- match.arg(align)
  vec_data <- rec_data(x, step, init = init, align = align)
  vec <- lapply(vec_data, as_fn(fn, ...))
  body <- Reduce(c, vec)
  idx  <- rec_fill_idx(x, step, init = init, align = align)
  out  <- fill_(body, idx, fill = fill)
  out <- inherit_na(out, x)
  with_attrs(out, x)
}

inherit_na <- function(out, x) {
  na <- is.na(x)
  out[na] <- NA_real_
  out
}


# rec statistics ----------------------------------------------------------



#' Recursive
#'
#' @param x numeric vector
#'
#' @export
rec_sum <- function(x) {
  cumsum(x)
}

#' Recursive
#'
#' @inheritParams rec_sum
#'
#' @export
rec_min <- function(x) {
  cummin(x)
}

#' Recursive
#'
#' @inheritParams rec_sum
#'
#' @export
rec_max <- function(x) {
  cummax(x)
}

#' Recursive
#'
#' @inheritParams rec_sum
#'
#' @export
rec_prod <- function(x) {
  cumprod(x)
}

# Centrality ----


#' Recursive
#'
#' @inheritParams rec_sum
#'
#' @export
rec_mean <- function(x) {
  cumsum(as.numeric(x))/seq_along(x)
}
# cumstats::cummean


#' Recursive
#'
#' @inheritParams rec_sum
#'
#' @export
rec_median <- function(x) {
  sapply(seq_along(x), function(n, y) median(y[1:n]), y = x)
}

#' Recursive
#'
#' @inheritParams rec_sum
#'
#' @export
rec_mode <- function(x) {
  sapply(seq_along(x), function(n, y) modex(y[1:n]), y = x)
}

# Dispersion ----

#' Recursive
#'
#' @inheritParams rec_sum
#'
#' @export
rec_sd <- function(x) {
  sapply(seq_along(x), function(k, z) sd(z[1:k]), z = x)

}

#' Recursive
#'
#' @inheritParams rec_sum
#'
#' @export
rec_var <- function(x) {
  sapply(seq_along(x), function(k, z) var(z[1:k]), z = x)

}

# Shape ----

rec_skewnes <- function(x) {
  sapply(seq_along(x), function(k, z) var(z[1:k]), z = x)
}

rec_kurtosis <- function(x) {
  sapply(seq_along(x), function(k, z) kurtosis(z[1:k]), z = x)
}
