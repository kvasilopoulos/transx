
#' Hamilton Filter
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#' This function computes the cyclical component of the Hamilton filter.
#'
#' @template x
#'
#' @param p `[integer(1): 4]`
#'
#' A value indicating the number of lags
#'
#' @param horizon `[integer(1): 8]`
#'
#' A value indicating the number of periods to look ahead.
#'
#' @template fill
#' @template return
#'
#'
#' @importFrom stats embed
#' @export
#'
#' @examples
#' unemp <- ggplot2::economics$unemploy
#' unemp_cycle <- filter_hamilton(unemp)
#' plotx(cbind(unemp, unemp_cycle))
filter_hamilton <- function(x, p = 4, horizon = 8, fill = NA) {
  lagmatrix <- embed(c(rep(NA, p - 1), x) , p)
  y <- leadx_(x, horizon)
  idx <- 1:(horizon + p - 1)
  body <- unname(stats::glm(y ~ lagmatrix)$residuals)
  out <- fill_(body, idx, fill = fill)
  with_attrs(out, x)
}

#' Selecting lambda
#'
#' @description
#'
#' Approaches to selecting lambda.
#'
#' @param freq `[character: "quarterly"]`
#'
#' The frequency of the dataset.
#'
#' @param type `[character: "rot"]`
#'
#' The methodology to select lambda.
#'
#' @details
#'
#' Rule of thumb is from Hodrick and Prescot (1997):
#'
#' - Lambda = 100*(number of periods in a year)^2
#'
#' * Annual data = 100 x 1^2 = 100
#' * Quarterly data = 100 x 4^2 = 1,600
#' * Monthly data = 100 x 12^2 = 14,400
#' * Weekly data = 100 x 52^2 = 270,400
#' * Daily data = 100 x 365^2 = 13,322,500
#'
#' Ravn and Uhlig (2002) state that lambda  should vary by the fourth power of the frequency observation ratio;
#'
#' - Lambda = 6.25 x (number of periods in a year)^4
#'
#' Thus, the rescaled default values for lambda are:
#'
#' * Annual data = 1600 x 1^4 = 6.25
#' * Quarterly data = 1600 x 4^4= 1600
#' * Monthly data = 1600 x 12^4= 129,600
#' * Weekly data = 1600 x 12^4 = 33,177,600
#'
#' @references Hodrick, R. J., & Prescott, E. C. (1997). Postwar US business cycles:
#' an empirical investigation. Journal of Money, credit, and Banking, 1-16.
#' @references Ravn, M. O., & Uhlig, H. (2002). On adjusting the Hodrick-Prescott
#' filter for the frequency of observations. Review of economics and statistics, 84(2), 371-376.
#'
#' @export
select_lambda <- function(freq = c("quarterly", "annual", "monthly", "weekly"),
                          type = c("rot", "ru2002")) {
  freq <- match.arg(freq)
  type <- match.arg(type)
  freq <- switch(
    freq,
    annual = 1,
    quarterly = 4,
    monthly = 12,
    weekly = 52
  )

  if (type == "rot") {
    lambda <- 100 * freq^2
  } else {
    lambda <- freq^4 * 6.25
  }
  lambda
}

#' Hodrick-Prescot Filter
#'
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#' This function computes the cyclical component of the Hodrick-Prescot filter.
#'
#' @template x
#' @param ... Further arguments passed to \code{\link[mFilter]{hpfilter}}.
#'
#' @seealso select_lambda
#' @export
#' @examples
#' \donttest{
#' unemp <- ggplot2::economics$unemploy
#' unemp_cycle <- filter_hp(unemp, freq = select_lambda("monthly"))
#' plotx(cbind(unemp, unemp_cycle))
#'}
filter_hp <- function(x, ...) {
  need_pkg("mFilter")
  assert_uni_ts(x)
  dots <- rlang::dots_list(...)
  if(is.null(dots$freq) && (is.null(dots$type) || dots$type == "lambda")) {
    freq <- 1600
    type <- "lambda"
    disp_info("Using `lambda = 1600`.")
  }else{
    freq <- dots$freq
    type <- dots$type
  }
  if(is.null(dots$drift)) {
    drift <- FALSE
  }else{
    drift <- dots$drift
  }
  out <- mFilter::hpfilter(x, freq, type = type, drift = drift)$cycle
  with_attrs(out, x)
}


# TODO learn why it is 3 top and 3 bottom

#' Baxter-King Filter
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#'
#' This function computes the cyclical component of the Baxter-King filter.
#'
#' @template x
#' @template fill
#' @param ... Further arguments passed to \code{\link[mFilter]{bkfilter}}.
#'
#' @export
#' @examples
#' \donttest{
#' unemp <- ggplot2::economics$unemploy
#' unemp_cycle <- filter_bk(unemp)
#' plotx(cbind(unemp, unemp_cycle))
#'}
filter_bk <- function(x, fill = NA, ...) {
  need_pkg("mFilter")
  assert_uni_ts(x)
  pre <- mFilter::bkfilter(x, ...)$cycle
  idx <- which(is.na(pre))
  body <- body_(pre, idx)
  out <- fill_(body, idx, fill)
  with_attrs(out, x)
}

#' Christiano-Fitzgerald Filter
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#' This function computes the cyclical component of the Christiano-Fitzgerald filter.
#'
#' @template x
#' @param ... Further arguments passed to \code{\link[mFilter]{cffilter}}.
#'
#' @export
#' @examples
#' \donttest{
#' unemp <- ggplot2::economics$unemploy
#' unemp_cycle <- filter_cf(unemp)
#' plotx(cbind(unemp, unemp_cycle))
#'}
filter_cf <- function(x, ...) {
  need_pkg("mFilter")
  assert_uni_ts(x)
  out <- mFilter::cffilter(x, ...)$cycle[,1]
  with_attrs(out, x)
}

#' Butterworth Filter
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#' This function computes the cyclical component of the Butterworth  filter.
#'
#' @template x
#' @param ... Further arguments passed to \code{\link[mFilter]{bwfilter}}.
#'
#' @export
#' @examples
#' \donttest{
#' unemp <- ggplot2::economics$unemploy
#' unemp_cycle <- filter_bw(unemp, freq = 10)
#' plotx(cbind(unemp, unemp_cycle))
#'}
filter_bw <- function(x, ...) {
  need_pkg("mFilter")
  assert_uni_ts(x)
  out <- mFilter::bwfilter(x, ...)$cycle
  with_attrs(out, x)
}

#' Trigonometric regression Filter
#'
#' @description
#'
#' `r rlang:::lifecycle("maturing")`
#'
#' This function computes the cyclical component of the trigonometric regression filter.
#'
#' @template x
#' @param ...  Further arguments passed to \code{\link[mFilter]{trfilter}}.
#'
#' @export
#' @examples
#' \donttest{
#' unemp <- ggplot2::economics$unemploy
#' unemp_cycle <- filter_tr(unemp, pl=8, pu=40)
#' plotx(cbind(unemp, unemp_cycle))
#'}
filter_tr <- function(x, ...) {
  need_pkg("mFilter")
  assert_uni_ts(x)
  out <- mFilter::trfilter(x, ...)$cycle[,1]
  with_attrs(out, x)
}

#' Boosted HP filter
#'
#'  `r rlang:::lifecycle("experimental")`
#'
#' @template x
#'
#' @param lambda `[numeric(1): 1600]`
#'
#' Smoothness penalty parameter.
#'
#' @param stopping: `[character: "nonstop"]`
#'
#' * If stopping = "adf" or "BIC", used stopping criteria.
#' * If stopping = "nonstop", iterated until max_iter
#'
#' @param sig_p: `[numeric(1): 0.05]`
#'
#' The significance level of the ADF test as the stopping criterion.
#' It is used only when stopping == "adf".
#'
#' @param max_iter: `[numeric(1): 100]`
#'
#' the maximum number of iterations.
#'
#' @return
#'
#' @references Phillips, P.C.B. and Shi, Z. (2021), BOOSTING: WHY YOU CAN USE THE HP FILTER.
#'  International Economic Review. https://doi.org/10.1111/iere.12495
#'
#' @source This function has been retrieved and rewritten from
#' https://github.com/zhentaoshi/Boosted_HP_filter/blob/master/R/BoostedHP.R
#'
#'
#' @template return
#'
#' @examples
#' unemp <- ggplot2::economics$unemploy
#' unemp_cycle <- filter_boosted_hp(unemp)
#' plotx(cbind(unemp, unemp_cycle))
#' @export
filter_boosted_hp <- function(x, lambda = 1600, iter = TRUE,
                              stopping = "nonstop", sig_p = 0.050, max_iter = 100) {
  assert_uni_ts(x)

  n <- length(x)
  I_n <- diag(n)
  D_temp <- rbind(matrix(0, 1, n), diag(1, n - 1, n))
  D_temp <- (I_n - D_temp) %*% (I_n - D_temp)
  D <- t(D_temp[3:n, ])
  S <- solve(I_n + lambda * D %*% t(D))
  mS <- diag(n) - S

  ### ADF test as the stopping criterion
  if (stopping == "adf") {
    r <- 1
    stationary <- FALSE
    x_c <- x

    x_f <- matrix(0, n, max_iter)
    adf_p <- rep(0, max_iter)

    while ((r <= max_iter) & (stationary == FALSE)) {
      x_c <- (diag(n) - S) %*% x_c # update
      x_f[, r] <- x - x_c
      adf_p_r <- adf.test(x_c, alternative = "stationary")$p.value
      adf_p[r] <- adf_p_r

      if (stopping == "adf") {
        stationary <- (adf_p_r <= sig_p)
      }

      # Truncate the storage matrix and vectors
      if (stationary == TRUE) {
        R <- r
        x_f <- x_f[, 1:R]
        adf_p <- adf_p[1:R]
        break
      }
      r <- r + 1
    }
    if (r > max_iter) {
      R <- max_iter
      warning("The number of iterations exceeds the limit. The residual cycle remains non-stationary.")
    }
  } else {

    r <- 0
    x_c_r <- x
    x_f <- matrix(0, n, max_iter)
    IC <- rep(0, max_iter)
    IC_decrease <- TRUE
    I_S_0 <- diag(n) - S
    c_HP <- I_S_0 %*% x
    I_S_r <- I_S_0

    # while (r < max_iter) {
    for(r in 1:max_iter) {
      # r <- r + 1
      x_c_r <- I_S_r %*% x # this is the cyclical component after m iterations
      x_f[, r] <- x - x_c_r
      B_r <- diag(n) - I_S_r
      IC[r] <- var(x_c_r) / var(c_HP) + log(n) / (n - sum(diag(S))) * sum(diag(B_r))
      # I_S_r <- I_S_0 %*% I_S_r # update for the next round
      I_S_r <- crossprod(I_S_0, I_S_r) # TODO eigenmatmultiplication in Rcpp
      if ((r >= 2) & (stopping == "BIC")) {
        if (IC[r - 1] < IC[r]) break
      }
    }
    R <- r - 1
    x_f <- as.matrix(x_f[, 1:R])
    x_c <- x - x_f[, R]
  }
  x_c
}

adf.test <- function(x, alternative = c("stationary", "explosive"),
                     k = trunc((length(x) - 1)^(1 / 3))) {

  alternative <- match.arg(alternative)
  k <- k + 1
  x <- as.vector(x, mode = "double")
  y <- diff(x)
  n <- length(y)
  z <- embed(y, k)
  yt <- z[, 1]
  xt1 <- x[k:n]
  tt <- k:n
  if (k > 1) {
    yt1 <- z[, 2:k]
    res <- lm(yt ~ xt1 + 1 + tt + yt1)
  }
  else {
    res <- lm(yt ~ xt1 + 1 + tt)
  }
  res.sum <- summary(res)
  STAT <- res.sum$coefficients[2, 1] / res.sum$coefficients[2, 2]
  table <- cbind(
    c(4.38, 4.15, 4.04, 3.99, 3.98, 3.96),
    c(3.95, 3.8, 3.73, 3.69, 3.68, 3.66),
    c(3.6, 3.5, 3.45, 3.43, 3.42, 3.41),
    c(3.24, 3.18, 3.15, 3.13, 3.13, 3.12),
    c(1.14, 1.19, 1.22, 1.23, 1.24, 1.25),
    c(0.8, 0.87, 0.9, 0.92, 0.93, 0.94),
    c(0.5, 0.58, 0.62, 0.64, 0.65, 0.66),
    c(0.15, 0.24, 0.28, 0.31, 0.32, 0.33)
  )
  table <- -table
  table <- -table
  tablen <- dim(table)[2]
  tableT <- c(25, 50, 100, 250, 500, 1e+05)
  tablep <- c(0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99)
  tableipl <- numeric(tablen)
  for (i in (1:tablen)) {
    tableipl[i] <- approx(tableT, table[,i], n, rule = 2)$y
  }
  interpol <- approx(tableipl, tablep, STAT, rule = 2)$y
  if (!is.na(STAT) && is.na(approx(tableipl, tablep, STAT, rule = 1)$y)) {
    if (interpol == min(tablep)) {
      warning("p-value smaller than printed p-value")
    } else {
      warning("p-value greater than printed p-value")
    }
  }
  if (alternative == "stationary") {
    PVAL <- interpol
  } else if (alternative == "explosive") {
    PVAL <- 1 - interpol
  } else {
    stop("irregular alternative")
  }
  PARAMETER <- k - 1

  list(
    statistic = STAT,
    parameter = PARAMETER,
    alternative = alternative,
    p.value = PVAL
  )

}
