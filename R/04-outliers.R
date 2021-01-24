
# Helper ------------------------------------------------------------------

not_between <- function(x, t_low, t_high) {
  which((x < t_low) | (x > t_high))
}


# TODO out_grubs_chochran_asdasdad with cutoff

#' Winsorize
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#' Replace extremely values that are defined by `min` and `max`.
#'
#' @template x
#' @param min `[numeric(1): quantile(x, 0.05)]`
#'
#' The lower bound, all values lower than this will be replaced by this value.
#'
#' @param max `[numeric(1): quantile(x, 0.95)]`
#'
#' The upper bound, all values above than this will be replaced by this value.
#'
#' @template return
#'
#' @seealso \code{\link[DescTools]{Winsorize}}
#' @export
#'
#' @examples
#' x <- c(1, 3, -1, 5, 10, 100)
#' out_winsorise(x)
#'
out_winsorise <- function(x, min = quantile(x, 0.05), max = quantile(x, 0.95)) {
  assert_uni_ts(x)
  out_min <- x < min
  out_max <- x > max
  out <- x
  out[out_min] <- min
  out[out_max] <- max
  n_idx <- length(union(out_min, out_max))
  disp_info("{n_idx} value{?s} winsorized.")
  with_attrs(out, x)
}

#' @rdname out_winsorise
#' @export
out_winsorize <- out_winsorise


#' Detect outliers with upper and lower threshold
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#' @template x
#' @template fill
#'
#' @param tlow `[numeric(1): NULL]`
#'
#' The lower threshold.
#'
#' @param thigh `[numeric(1): NULL]`
#'
#' The upper threshold.
#'
#'
#' @template return
#'
#' @examples
#' x <- c(1, 3, -1, 5, 10, 100)
#' out_threshold(x, tlow = 0, fill = 0)
#' out_threshold(x, thigh = 9, fill = function(x) quantile(x, 0.9))
#'
#' @export
out_threshold <- function(x, tlow = NULL, thigh = NULL, fill = NA) {
  # threshold might be a function quantile(0.9)
  if (is.null(tlow) && is.null(thigh)) {
    stop("`threshold` should be specified", call. = FALSE)
  }
  if (!is.null(tlow) && !is.null(thigh)) {
    idx <- not_between(x, tlow, thigh)
  }else if (!is.null(tlow)) {
    idx <- which(x < tlow)
  } else if (!is.null(thigh)) {
    idx <- which(x > thigh)
  }
  body <- body_(x, idx)
  out <- fill_outliers(body, idx, fill)
  with_attrs(out, x)
}


#' Detect outliers with Percentiles
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#' @template x
#' @template fill
#' @param pt_low the lowest quantile
#' @param pt_high the highest quantile
#'
#' @export
#' @examples
#' x <- c(1, 3, -1, 5, 10, 100)
#' out_pt(x)
#'
out_pt <- function(x, pt_low = 0.1, pt_high = 0.9, fill = NA) {
  tpt <- quantile(x, probs = c(pt_low, pt_high))
  disp(sprintf("Acceptable range (%s, %s)", tpt[1], tpt[2]))
  out <- out_threshold(x, tlow = tpt[1], thigh = tpt[2], fill = fill)
  with_attrs(out, x)
}

#' Detect outliers with zscore
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#' @template x
#' @template fill
#' @param ... Further arguments passed to `score`.
#' @param cutoff `[numeric(1): 3]`
#'
#' Cutoff point that determines the number of score quantities after which an
#' observation is considered outlier.
#'
#' @examples
#' out_score_z(c(0,0.1,2,1,3,2.5,2,.5,6,4,100))
#'
#' @export
out_score_z <- function(x, cutoff = 3, fill = NA, ...) {
  scores <- score_z(x, ...)
  idx <- which(abs(scores) > cutoff)
  body <- body_(scores, idx)
  out <- fill_outliers(body, idx, fill)
  with_attrs(out, x)
}

#' Detect outliers Iglewicz and Hoaglin (1993) robust z-score method
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#' @template x
#' @template fill
#' @param cutoff `[numeric(1): 3.5]`
#'
#' Cutoff point that determines the number of score quantities after which an
#' observation is considered outlier.
#'
#' @param ... further arguments passed to `score`.
#'
#' @export
#' @examples
#' out_score_zrob(c(0,0.1,2,1,3,2.5,2,.5,6,4,100))
#'
out_score_zrob <- function(x, cutoff = 3.5, fill = NA, ...) {
  scores <- 0.6745*score_mad(x, ...)
  idx <- which(abs(scores) > cutoff)
  body <- body_(scores, idx)
  out <- fill_outliers(body, idx, fill)
  with_attrs(out, x)
}

out_score_t <- function(x, cutoff = 3.5, fill = NA, ...) {
  scores <- score_t(x, ...)
  idx <- which(abs(scores) > cutoff)
  body <- body_(x, -idx)
  out <- fill_outliers(body, idx, fill)
  attributes(out) <- attributes(x)
  out
}

out_score_chisq <- function(x, cutoff = 3.5, fill = NA, ...) {
  scores <- score_chisq(x, ...)
  idx <- which(abs(scores) > cutoff)
  body <- body_(x, -idx)
  out <- fill_outliers(body, idx, fill)
  with_attrs(out, x)
}

#' Detect outliers with Tukey's method
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#' @template x
#' @template fill
#' @param cutoff `[numeric(1): 1.5]`
#'
#' Cutoff point that determines the number of score quantities after which an
#' observation is considered outlier.
#'
#' @param ... further arguments passed to `quantile`.
#'
#' @export
#' @importFrom stats quantile
#' @examples
#'
#' out_iqr(c(0,1,3,4,20))
#'
out_iqr <- function(x, cutoff = 1.5, fill = NA, ...) {
  q1 <- quantile(x, 0.25, ...)
  q3 <- quantile(x, 0.75, ...)
  iqr <- q3 - q1
  idx <- which(x < q1 - cutoff*iqr | x > q3 + cutoff*iqr)

  body <- body_(x, idx)
  out <- fill_outliers(body, idx, fill)
  with_attrs(out, x)
}


fill_outliers <- function(body, idx, fill) {
  fill_(
    body, idx, fill,
    msg_on_success = disp_info("{nidx(idx)} outlier{?s} detected."),
    msg_on_na = disp_info("{nidx(idx)} outliers detected.")
    )
}

