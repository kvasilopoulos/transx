
# Hodrick Prescot ---------------------------------------------------------


# Rule of thumb is:
#   Lambda = 100*(number of periods in a year)^2
#
# In this respect, for:
# Annual data = 100*1^2 = 100
# Quarterly data = 100*4^2 = 1,600
# Monthly data = 100*12^2 = 14,400
# Weekly data = 100*52^2 = 270,400
# find_lambda_ts <- function(x, type = c("rot", "rh2002")) {
#   freq <- find_freq(x)
#   if (type == "rot") {
#     lambda <- 100 * freq^2
#   } else {
#     lambda <- freq^2 * 6.25
#   }
#   lambda
# }


# c(100, 1600, 14400, 270400)


filter_hp2 <- function(x, ...) {
  need_pkg("mFilter")
  assert_uni_ts(x)
  dots <- rlang::dots_list(...)
  # if(is.null(lambda)) {
  #   lambda <- 1600
  #   disp("Using `lambda = 1600`.")
  # }
  out <- mFilter::hpfilter(x, freq = lambda)$cycle
  with_attrs(out)
}

#' Hodrick Prescot Filter
#'
#' This function computes the cyclical component of the Hodrick-Prescot filter.
#'
#' @param x `[univariate vector]`
#'
#' Univariate vector, numeric or ts object with only one dimension.
#'
#' @param lambda `[positive numeric(1): 1600]`
#'
#' Smoothing parameter.
#'
#' @template return-template
#' @seealso `mFilter::hpfilter`
filter_hp <- function(x, lambda = NULL) {
  assert_uni_ts(x)
  if(is.null(lambda)) {
    lambda <- 1600
    disp("Using `lambda = 1600`.")
  }
  out <- filter_hp_(x, lambda)
  with_attrs(out, x)
}

filter_hp_ <- function(x, lambda) {
  xmat <- as.matrix(x)
  nr <- nrow(xmat)
  imat <- diag(nr)
  Ln <- rbind(matrix(0, 1, nr), diag(1, nr - 1, nr))
  Ln <- (imat - Ln) %*% (imat - Ln)
  Q <- t(Ln[3:nr, ])
  sigma_r <- t(Q) %*% Q
  sigma_n <- diag(nr - 2)
  g <- t(Q) %*% xmat
  b <- solve(sigma_n + lambda * sigma_r, g)
  cycle <- c(lambda * Q %*% b)
  cycle
}


# Baxter King -------------------------------------------------------------

#' Baxter King Filter
#'
#' This function computes the cyclical component of the Baxter-King filter.
#'
#' @inheritParams filter_hp
#'
#' @param min `[numeric(1): NULL]`
#'
#' Minimum period of oscillation.
#'
#' @param max `[numeric(1): NULL]`
#'
#' Maximum period of oscillation.
#'
#' @param nfix `[numeric(1): NULL]`
#'
#' @param drift `[logical(1): FALSE]`
#'
#' @template return-template
#'
#' @seealso \code{\link[mFilter::bkfilter]{mFilter::bkfilter}}
#'
filter_bk <- function(x, min = NULL, max = NULL, nfix = NULL,
                      type = c("fixed", "variable"), drift = FALSE) {
  assert_uni_ts(x)
  if (min <= max) {
    stop("`max` must be larger than `min`", call. = FALSE)
  }
  if (is.null(nfix)) {
    nfix <- frequency(x) * 3
  }
  if (nfix >= n / 2) {
    stop("fixed lag length must be < n/2", call. = FALSE)
  }
  type <- match.arg(type)
  out <- filter_bk_(x, min, max, nfix= nfix, type = type, drift = drift)
  with_attrs(out, x)
}

filter_bk_ <- function(x, min, max, nfix, type, drift) {
  if(is.null(min)) {

  }

  a <- 2 * pi/max
  b <- 2 * pi/min
  n <- length(x)
  AA <- matrix(0, n, n)
  B <- as.matrix(c((b - a) / pi, (sin(j * b) - sin(j * a)) / (j *pi)))
  if (type == "fixed") {
    j <- 1:(2 * n)
    bb <- matrix(0, 2 * nfix + 1, 1)
    bb[(nfix + 1):(2 * nfix + 1)] <- B[1:(nfix + 1)]
    bb[nfix:1] <- B[2:(nfix + 1)]
    bb <- bb - sum(bb) / (2 * nfix + 1)
    for (i in (nfix + 1):(n - nfix)) {
      AA[i, (i - nfix):(i +
                          nfix)] <- t(bb)
    }
  }else if (type == "variable") {
    for (i in (nfix + 1):(n - nfix)) {
      j <- min(c(i - 1, n - i))
      bb <- matrix(0, 2 * j + 1, 1)
      bb[(j + 1):(2 * j + 1)] <- B[1:(j + 1)]
      bb[j:1] <- B[2:(j + 1)]
      bb <- bb - sum(bb) / (2 * j + 1)
      AA[i, (i - j):(i + j)] <- t(bb)
    }
  }
  cycle <- AA %*% as.matrix(x)
  cycle
}


# Hamilton ----------------------------------------------------------------

#' Hamilton Filter
#'
#' This function computes the cyclical component of the Hamilton filter.
#'
#' @inheritParams filter_hp
#'
#' @param n `[numeric(1): NULL]`
#'
#' @param horizon `[numeric(1): NULL]`
#'
#' @param nfix `[numeric(1): NULL]`
#'
#' @param nfix `[logical(1): FALSE]`
#'
#' @template return-template
#' @export
#'
#' @seealso
#'
filter_hamilton <- function(x, n = 4, horizon = 8, fill = NA, fill_fun = NULL) {
  lagmatrix <- embed(c(rep(NA, p - 1), x) , p)
  y <- leadx_(x, h)
  out <- filter_hamilton_(x, )
  with_attrs(out, x)
}

filter_hamilton_ <- function(x, n = 4, horizon = 8, fill = NA, fill_fun = NULL) {
  body <- unname(stats::glm(yt ~ lagmatrix)$residuals)
  idx <- 1:(h + p - 1)
  out <- fill_(body, idx, fill = fill, fill_fun = fill_fun)
  out
}

#  Butterworth ------------------------------------------------------------

# filter_bw <- function (x, freq = NULL, nfix = NULL, drift = FALSE) {
#
#   freq %||% ifelse(frequenc(x) > 1, trunc(frequency(x) * 2.5), 2)
#   nfix %||% 2
#
#   xo = x
#   x = as.matrix(x)
#   if (drift)
#     x = undrift(x)
#   n = length(x)
#   cut.off = 2 * pi/freq
#   mu = (1/tan(cut.off/2))^(2 * nfix)
#   imat = diag(n)
#   Ln = rbind(matrix(0, 1, n), diag(1, n - 1, n))
#   Ln = imat - Ln
#   if (nfix > 1) {
#     for (i in 1:(nfix - 1)) Ln = (imat - Ln) %*% Ln
#   }
#   Q = t(Ln[3:n, ])
#   SIGMA.R = t(Q) %*% Q
#   SIGMA.n = abs(SIGMA.R)
#   g = t(Q) %*% as.matrix(x)
#   b = solve(SIGMA.n + mu * SIGMA.R, g)
#   x.cycle = c(mu * Q %*% b)
#   x.trend = x - x.cycle
#   if (is.ts(xo)) {
#     tsp.x = tsp(xo)
#     x.cycle = ts(x.cycle, start = tsp.x[1], frequency = tsp.x[3])
#     x.trend = ts(x.trend, start = tsp.x[1], frequency = tsp.x[3])
#     x = ts(x, start = tsp.x[1], frequency = tsp.x[3])
#   }
#   A = mu * Q %*% solve(SIGMA.n + mu * SIGMA.R) %*% t(Q)
#   if (is.ts(xo)) {
#     tsp.x = tsp(xo)
#     x.cycle = ts(x.cycle, start = tsp.x[1], frequency = tsp.x[3])
#     x.trend = ts(x.trend, start = tsp.x[1], frequency = tsp.x[3])
#     x = ts(x, start = tsp.x[1], frequency = tsp.x[3])
#   }
#   res
# }


# Christiano–Fitzgerald ---------------------------------------------------






# Other filters -----------------------------------------------------------




# spectral and wavelet: https://kevinkotze.github.io/ts-5-tut/

# filter_wavelet <- function(x) {
#   # https://www.r-econometrics.com/timeseries/economic-cycle-extraction/
#
#   y <- diffx_(x)
#   wave <- mra(as.numeric(y[!is.na(y)]), J = 5)
#   out <- cumsum(wave$D3 + wave$D4 + wave$D5)
#   out
# }

# filter_emd <- function(x) {
#   # Empirical mode decomposition (EMD)
#   # https://www.r-econometrics.com/timeseries/economic-cycle-extraction/
#   EMD::emd(as.numeric(x))$imf[,2]
# }

# filter_gc <- function(x, iterations = 1000, burnin = 100) {
#   # Grant, A. L., & Chan, J. C. C. (2017)
#   # https://www.r-econometrics.com/timeseries/economic-cycle-extraction/
#   # Rescaled data
#   y <- x #na.omit(x) * 100
#   tt <- length(y) # T
#   p <- 2 # Lags of phi
#
#   # Priors of phi
#   prior_phi_mu <- matrix(c(1.3, -.7))
#   prior_phi_v_i <- diag(1, p)
#
#   # Priors of gamma
#   prior_gamma_mu <- matrix(c(850, 850)) # Should be close to first value of the series
#   prior_gamma_v_i <- diag(1 / 100, p)
#
#   # Priors for sigma2_tau
#   prior_s_tau <- .01
#
#   # Priors for sigma2_c
#   prior_s_c_shape <- 3
#   prior_s_c_rate <- 2
#
#   # Initial Values
#   # X_gamma
#   x_gamma <- cbind(2:(tt + 1), -1:-tt)
#
#   # H_2
#   h2 <- diag(1, tt)
#   diag(h2[-1, -tt]) <- -2
#   diag(h2[-(1:2), -((tt - 1):tt)]) <- 1
#   h2h2 <- crossprod(h2)
#
#   # H_phi
#   h_phi <- diag(1, tt)
#   phi <- matrix(c(1.34, -.7))
#   for (i in 1:p) {
#     diag(h_phi[-(1:i), -((tt - i):tt)]) <- -phi[i,]
#   }
#
#   # Inverse of sigma tau
#   s_tau_i <- 1 / .001
#
#   # Inverse of sigma c
#   s_c_i <- 1 / .5
#
#   # gamma
#   gamma <- t(rep(y[1], 2)) # Should be close to first value of the series
#
#
#
#   # Data containers for draws
#   draws_tau <- matrix(NA, tt, iterations - burnin)
#   draws_c <- matrix(NA, tt, iterations - burnin)
#
#   # TODO start progress_bar
#
#   # Start Gibbs sampler
#   for (draw in 1:iterations) {
#     # Draw tau
#     alpha <- solve(h2, matrix(c(2 * gamma[1] - gamma[2], -gamma[1], rep(0, tt - 2))))
#     sh2 <- s_tau_i * h2h2
#     shphi <- s_c_i * as.matrix(crossprod(h_phi))
#     K_tau <- sh2 + shphi
#     mu_tau <- solve(K_tau, sh2 %*% alpha + shphi %*% y)
#     tau <- mu_tau + solve(chol(K_tau), rnorm(tt))
#
#     # Draw phi
#     c <- c(rep(0, p), y - tau)
#     temp <- embed(c, 1 + p)
#     c <- matrix(temp[, 1])
#     x_phi <- temp[, -1]
#     K_phi <- prior_phi_v_i  + s_c_i * crossprod(x_phi)
#     mu_phi <- solve(K_phi, prior_phi_v_i %*% prior_phi_mu + s_c_i * crossprod(x_phi, c))
#     phi_can <- mu_phi + solve(chol(K_phi), rnorm(p))
#     if (sum(phi_can) < .99 & phi_can[2] - phi_can[1] < .99 & phi_can[2] > -.99) {
#       phi <- phi_can
#       for (i in 1:p) {
#         diag(h_phi[-(1:i), -((tt - i):tt)]) <- -phi[i,]
#       }
#     }
#
#     # Draw variance c
#     s_c_i <- rgamma(1, shape = 3 + tt / 2, rate = 2 + crossprod(c - x_phi %*% phi) / 2)
#
#     # Draw variance tau
#     tausq_sum <- sum(diff(diff(c(gamma[2:1], tau)))^2)
#     s_tau_can <- seq(from = runif(1) / 1000,
#                      to = prior_s_tau - runif(1) / 1000, length.out = 500)
#     lik <- -tt / 2 * log(s_tau_can) - tausq_sum / (2 * s_tau_can)
#     plik <- exp(lik - max(lik))
#     plik <- plik / sum(plik)
#     plik <- cumsum(plik)
#     s_tau_i <- 1 / s_tau_can[runif(1) < plik][1]
#
#     # Draw gamma
#     sxh2 <- s_tau_i * crossprod(x_gamma, h2h2)
#     K_gamma <- as.matrix(prior_gamma_v_i + sxh2 %*% x_gamma)
#     mu_gamma <- solve(K_gamma, prior_gamma_v_i %*% prior_gamma_mu + sxh2 %*% tau)
#     gamma <- mu_gamma + solve(chol(K_gamma), rnorm(2))
#
#     # Save draws
#     if (draw > burnin) {
#       pos_draw <- draw - burnin
#       draws_tau[, pos_draw] <- tau
#       draws_c[, pos_draw] <- c
#     }
#   }
#
#   mean_c <- apply(draws_c, 1, mean) / 100
#   mean_c
#
# }



# decompose_bn <- function() {
  #' Beveridge-Nelson decomposition
  #'
  # @param data A vector of first-order integrated numeric values
  # @param nlag A numberic scalar for the lag-order of the autoregressive (cyclical) part
  # @return Values for the stochastic trend and the stationary cycle
  # @examples
  #

  # bnd  <- function(data,nlag){
  #
  #   y=matrix(data,ncol=1)
  #   #nlag=8
  #
  #   yd=diff(y,lag=1)
  #   yl=matrix(rep(0,length(y)*nlag),ncol=nlag)
  #   yl[,1] = c(0,yd)
  #   for (i in 2:nlag){
  #     yl[,i] = c(0,yl[1:(length(y)-1),i-1])
  #   }
  #   x=yl[(nlag+1):(length(y)-1),1:nlag]
  #   yy=matrix(yd[(1+nlag):length(yd),],ncol=1)
  #
  #   # OLS
  #   beta <- lm(yy ~ x)$coefficients
  #
  #   # Companion form of matrix
  #   eye=diag(1,nlag)
  #   coef.tmp=matrix(beta[2:length(beta)],nrow=1)
  #   betac.tmp=rbind(coef.tmp,eye)
  #   betac=betac.tmp[1:nlag,]
  #
  #
  #   c1=betac %*% solve(diag(nlag)-betac)
  #   ydd=c(rep(0,1+nlag),yd)-beta[1]
  #   ydd.len=length(ydd)
  #
  #   # Construct matrix of historical lags
  #   yD = matrix(rep(0,nlag*(ydd.len-nlag)),nrow=nlag)
  #
  #   for (i in 1:nlag){
  #     yD[i,]=matrix(ydd[(1+i):(ydd.len-(nlag)+i)],nrow=1)
  #   }
  #
  #   yD.tmp=apply(yD,2,rev)
  #   yD=yD.tmp[nrow(yD.tmp):1,]
  #
  #   # Selection vector
  #   sel=rep(0,nlag)
  #   sel[1]=1
  #
  #   # Compute trend and cycle
  #   ytr=y+t(sel%*%c1%*%yD)
  #   yc=t(sel%*%c1%*%yD)
  #   out=cbind(ytr,yc)
  #   colnames(out) = c("trend","cycle")
  #   out
  # }
# }









