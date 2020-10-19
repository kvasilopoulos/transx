
# Helper ------------------------------------------------------------------

not_between <- function(x, t_low, t_high) {
  which((x < t_low) | (x > t_high))
}


display_outlier <- function(x) {

}


# library(glue)
# x1 <- c(1,2,3,4)
# x2 <- 5
# out_min <- x1>x2
# glue("For min: {x1} to {x2}\n For max: {NULL} to {x2}")
# glue_if("For max {x1} to {FALSE}", out_min)

# TODO out_grubs_chochran_asdasdad with cutoff
# TODO outliers function have to print a message for identification reasons

#' Winsorize
#'
#' @export
out_winsorise <- function(x, min = quantile(x, 0.05), max = quantile(x, 0.95)) {
  asserts(x)
  out_min <- x < min
  out_max <- x < max
  x[out_min] <- min
  x[out_max] <- max
  x
}

# End helper --------------------------------------------------------------

#' Detect outliers with upper and lower threshold
#'
#' @param x mpla
#'
#'
#' @examples
#' x <- c(1, 3, -1, 5, 10, 100)
#' out_trim(x, tlow = 0, fill = 0)
#' out_trim(x, thigh = 9, fill_fun = function(x) quantile(x, 0.9))
#'
#' @export
out_threshold <- function(x, tlow = NULL, thigh = NULL, fill = NA, fill_fun = NULL) {
  # threshold might be a function quantile(0.9)
  if (is.null(tlow) && is.null(thigh)) {
    stop("`threshold` should be specified", call. = FALSE)
  }
  if (!is.null(tlow) && !is.null(thigh)) {
    idx <- not_between(x, tlow, thigh)
  }
  if (!is.null(tlow)) {
    idx <- which(x < tlow)
  }
  if (!is.null(thigh)) {
    idx <- which(x > thigh)
  }
  body <- x[-idx]
  out <- fill_(body, idx, fill, fill_fun)
  with_attrs(out, x)
}


#' Detect outliers with Percentiles
#'
#' @param x mpla
#' @param pt percentile
#'
#' @export
out_pt <- function(x, pt_low = 0.1, pt_high = 0.9, fill = NA, fill_fun = NULL, ...) {
  tpt <- quantile(x, probs = c(pt_low, pt_high), ...)
  display(sprintf("Acceptable range (%s, %s)", tpt[1], tpt[2]))
  out <- out_trim(x, tlow = tpt[1], thigh = tpt[2], fill = fill, fill_fun = fill_fun)
  with_attrs(out, x)
}


#' Detect outliers with zscore
#'
#' @param cutoff `[numeric(1): 3]`
#' @param ... further arguments passed to `score_z`.
#'
#' @export
out_score_z <- function(x, cutoff = 3, fill = NA, fill_fun = NULL, ...) {
  scores <- score_z(x, ...)
  idx <- which(abs(score) > cutoff) # TODO integer[0] creates a problem (I can't think of better alternative)
  body <- x[-idx]
  out <- idx_fill(body, idx, fill, fill_fun)
  with_attrs(out, x)
}

#' Detect outliers Iglewicz and Hoaglin (1993) robust z-score method
#'
#' @param cutoff `[numeric(1): 3.5]`
#' @param ... further arguments passed to `score`.
#'
#' @export
out_score_zrob <- function(x, cutoff = 3.5, fill = NA, fill_fun = NULL, ...) {
  score <- 0.6745*score_mad(x, ...) # TODO robust zscore
  idx <- which(abs(score) > cutoff) # TODO integer[0] creates a problem (I can't think of better alternative)
  body <- x[-idx]
  out <- fill_(body, idx, fill, fill_fun)
  with_attrs(out, x)
}

out_score_t <- function(x, cutoff = 3.5, fill = NA, fill_fun = NULL, ...) {
  score <- score_t(x, ...)
  idx <- which(abs(score) > cutoff) # TODO integer[0] creates a problem (I can't think of better alternative)
  body <- x[-idx]
  out <- idx_fill(body, idx, fill, fill_fun)
  attributes(out) <- attributes(x)
  out
}

out_score_chisq <- function(x, cutoff = 3.5, fill = NA, fill_fun = NULL, ...) {
  score <- score_chisq(x, ...)
  idx <- which(abs(score) > cutoff) # TODO integer[0] creates a problem (I can't think of better alternative)
  body <- x[-idx]
  out <- fill_(body, idx, fill, fill_fun)
  with_attrs(out, x)
}

#' Detect outliers with Tukey's method
#'
#' @param cutoff `[numeric(1): 1.5]`
#' @param ... further arguments passed to `quantile`.
#'
#' @export
#'
out_iqr <- function(x, cutoff = 1.5, fill = NA, fill_fun = NULL, ...) {
  q1 <- quantile(0.25)
  q3 <- quantile(0.75)
  idx <- which(x < q1 - cutoff*iqr | x > q3 + cutoff*iqr)
  body <- x[-idx]
  out <- fill_(body, idx, fill, fill_fun)
  with_attrs(out, x)
}
