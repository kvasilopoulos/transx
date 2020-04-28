# TODO outliers function have to print a message for identification reasons

out_winsorise <- out_winsorize <- function(x) {
  # tandardized = FALSE, centerFun = median, scaleFun = mad,
  # const = 2, return = c("data", "weights"), ...
  standardized <- isTRUE(standardized)
  if (standardized)
    return <- match.arg(return)
  else {
    x <- robStandardize(x, centerFun = centerFun, scaleFun = scaleFun, ...)
    center <- attr(x, "center")
    scale <- attr(x, "scale")
  }
  weights <- pmin(const/abs(x), 1)
  if (standardized && return == "weights")
    return(weights)
  x <- weights * x
  if (!standardized) {
    x <- c(x * scale + center)
  }
  x

  # minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
  # na.rm = FALSE, type = 7
  # if (is.null(minval) || is.null(maxval)) {
  #   xq <- quantile(x = x, probs = probs, na.rm = na.rm, type = type)
  #   if (is.null(minval))
  #     minval <- xq[1L]
  #   if (is.null(maxval))
  #     maxval <- xq[2L]
  # }
  # x[x < minval] <- minval
  # x[x > maxval] <- maxval
  # return(x)
}

out_trim_pt <- function(x, default = NA, pt = c(0.1, 0.9)) {
  threshold_pt <- quantile(x, probs = pt)
  out_trim(x, default = default, threshold_low = threshold_pt[1], threshold_high = threshold_pt[2])
}

out_trim <- function(x, default = NA, threshold_low = NULL, threshold_high = NULL, ...) {
  # threshold might be a function quantile(0.9)
  if (is.null(threshold_low) && is.null(threshold_high)) {
    stop("`threshold` should be specified", call. = FALSE)
  }
  if (!is.null(threshold_low) && !is.null(threshold_high)) {
    out <- not_between(x, threshold_low, threshold_high)
  }
  if (!is.null(threshold_low)) {
    out <- which(x < threshold_low)
  }
  if (!is.null(threshold_high)) {
    out <- which(x > threshold_high)
  }
  x[out] <- default
  x
}

not_between <- function(x, t_low, t_high) {
  which((x < t_low) | (x > t_high))
}

out_drop <- function(x, default = NA, rule = NULL) {

}

z_scores <- function(x, default = NA, cutoff = 3) {
  scores <- (x - mean(x))/sd(x)
  out <- which(abs(zscores) > cutoff)
  x[out] <- default
  x
}

#' Iglewicz and Hoaglin (1993) robust z-score method
z_score_mod <- function(x, default = NA, cutoff = 3.5, ...) {
  scores <- 0.6745*(x - median(x))/mad(x, ...)
  out <- which(abs(zscores) > cutoff)
  x[out] <- default
  x
}

#' Tukey's method
iqr_score <- function(x, default = NA, cutoff = 1.5) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  out <- which(x < q1 - cutoff*iqr | x > q3 + cutoff*iqr)
  x[out] <- default
  x
}

mad_mean_score <- function(x) {

}

mad_median_score <- function(x) {

}

detect_outliers <- function(x, rule = NULL, ...) {
  # type = c("z", "t", "chisq", "iqr",
  # "mad"), prob = NA, lim = NA

  n <- length(x)
  s <- match.arg(type)
  ty <- switch(s, z = 0, t = 1, chisq = 2, iqr = 3, mad = 4)
  if (ty == 0) {
    res <- (x - mean(x))/sd(x)
    if (is.na(prob))
      res
    else {
      if (prob == 1)
        pnorm(res)
      else if (prob == 0)
        abs(res) > (n - 1)/sqrt(n)
      else abs(res) > qnorm(prob)
    }
  }
  else if (ty == 1) {
    t <- (x - mean(x))/sd(x)
    res <- (t * sqrt(n - 2))/sqrt(n - 1 - t^2)
    if (is.na(prob))
      res
    else {
      if (prob == 1)
        pt(res, n - 2)
      else if (prob == 0)
        abs(res) > (n - 1)/sqrt(n)
      else abs(res) > qt(prob, n - 2)
    }
  }
  else if (ty == 2) {
    res <- (x - mean(x))^2/var(x)
    if (is.na(prob))
      res
    else {
      if (prob == 1)
        pchisq(res, 1)
      else abs(res) > qchisq(prob, 1)
    }
  }
  else if (ty == 3) {
    res <- x
    Q1 <- quantile(x, 0.25)
    Q3 <- quantile(x, 0.75)
    res[x >= Q1 & res <= Q3] <- 0
    res[x < Q1] <- (res[x < Q1] - Q1)/IQR(x)
    res[x > Q3] <- (res[x > Q3] - Q3)/IQR(x)
    if (is.na(lim))
      res
    else abs(res) > lim
  }
  else if (ty == 4) {
    res <- (x - median(x))/mad(x)
    if (is.na(prob))
      res
    else {
      if (prob == 1)
        pnorm(res)
      else if (prob == 0)
        abs(res) > (n - 1)/sqrt(n)
      else abs(res) > qnorm(prob)
    }
  }
}


out_detection_window <- function(x, n, fun = c("mean", "median")) {
  # can be blck/roll/rec
}
