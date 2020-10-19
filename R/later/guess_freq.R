# tsibble has guess_frequency
# anytime can be a good resource here
guess_freq <- function(x) {
  UseMethod("guess_freq")
}

guess_freq.default <- function() {

}

guess_freq.Date <- function(x) {
  diff_days <- diff(x)
  if(diff_days %in% c(365, 365.25)) {
    freq <- "annual"
  } else if (diff_days %in% c(89:91)){
    freq <- "quarterly"
  } else if (diff_days %in% c(28:31)){
    freq <- "monthly"
  } else if (diff_days %in% c(7)){
    freq <- "weekly"
  }else if (diff_days %in% 1){
    freq <- "daily"
  }else{
    stop("irregular date")
  }
  common_freq()[freq]
}

common_freq <- function() {
  c(
    # biannual = 0.5,
    annual	= 1,
    quarterly	= 4,
    monthly	= 12,
    weekly = 52,
    daily = 365
  )
}

guess_freq.ts <- function(x) {
  frequency(x)
}

datamat <- function (..., flatten = TRUE, functions = TRUE) {
  vars <- list(...)
  if (length(vars) == 0) {
    return(data.frame())
  }
  if (!is.null(names(vars))) {
    names(vars)[!nzchar(names(vars))] <- as.character(substitute(list(...))[-1])[!nzchar(names(vars))]
  }
  else {
    names(vars) <- as.character(substitute(list(...))[-1])
  }
  if (flatten) {
    i <- 1
    while (i <= length(vars)) {
      if (is.data.frame(vars[[i]])) {
        vars <- c(vars, c(vars[[i]]))
        vars[[i]] <- NULL
      }
      else if (is.matrix(vars[[i]])) {
        for (j in 1:NCOL(vars[[i]])) {
          vars[[length(vars) + 1]] <- vars[[i]][, j]
          names(vars)[length(vars)] <- make.names(colnames(vars[[i]])[j])
        }
        i <- i + 1
      }
      else {
        i <- i + 1
      }
    }
  }
  class(vars) <- "data.frame"
  row.names(vars) <- 1:max(sapply(vars, NROW))
  return(vars)
}

tslm <- function (formula, data, subset, lambda = NULL, biasadj = FALSE, ...){
  cl <- match.call()
  if (!("formula" %in% class(formula))) {
    formula <- stats::as.formula(formula)
  }
  if (missing(data)) {
    mt <- try(terms(formula))
    if (is.element("try-error", class(mt))) {
      stop("Cannot extract terms from formula, please provide data argument.")
    }
  }
  else {
    mt <- terms(formula, data = data)
  }
  vars <- attr(mt, "variables")
  tsvar <- match(c("trend", "season"), as.character(vars),
                 0L)
  fnvar <- NULL
  for (i in 2:length(vars)) {
    term <- vars[[i]]
    if (!is.symbol(term)) {
      if (typeof(eval(term[[1]])) == "closure") {
        fnvar <- c(fnvar, i)
      }
    }
  }
  attr(formula, ".Environment") <- environment()
  if (sum(c(tsvar, fnvar)) > 0) {
    rmvar <- c(tsvar, fnvar)
    rmvar <- rmvar[rmvar != attr(mt, "response") +
                     1]
    if (any(rmvar != 0)) {
      vars <- vars[-rmvar]
    }
  }
  if (!missing(data)) {
    vars <- vars[c(TRUE, !as.character(vars[-1]) %in% colnames(data))]
    dataname <- substitute(data)
  }
  if (!missing(data)) {
    data <- datamat(do.call(datamat, as.list(vars[-1]), envir = parent.frame()),
                    data)
  }
  else {
    data <- do.call(datamat, as.list(vars[-1]), envir = parent.frame())
  }
  if (is.null(dim(data)) && length(data) != 0) {
    cn <- as.character(vars)[2]
  }
  else {
    cn <- colnames(data)
  }
  if (is.null(tsp(data))) {
    if ((attr(mt, "response") + 1) %in% fnvar) {
      tspx <- tsp(eval(attr(mt, "variables")[[attr(mt,
                                                   "response") + 1]]))
    }
    tspx <- tsp(data[, 1])
  }
  else {
    tspx <- tsp(data)
  }
  if (is.null(tspx)) {
    stop("Not time series data, use lm()")
  }
  tsdat <- match(c("trend", "season"), cn, 0L)
  if (tsdat[1] == 0) {
    trend <- 1:NROW(data)
    cn <- c(cn, "trend")
    data <- cbind(data, trend)
  }
  if (tsdat[2] == 0) {
    if (tsvar[2] != 0 && tspx[3] <= 1) {
      stop("Non-seasonal data cannot be modelled using a seasonal factor")
    }
    season <- as.factor(cycle(data[, 1]))
    cn <- c(cn, "season")
    data <- cbind(data, season)
  }
  colnames(data) <- cn
  if (!missing(subset)) {
    if (!is.logical(subset)) {
      stop("subset must be logical")
    }
    else if (NCOL(subset) > 1) {
      stop("subset must be a logical vector")
    }
    else if (NROW(subset) != NROW(data)) {
      stop("Subset must be the same length as the number of rows in the dataset")
    }
    warning("Subset has been assumed contiguous")
    timesx <- time(data[, 1])[subset]
    tspx <- recoverTSP(timesx)
    if (tspx[3] == 1 && tsdat[2] == 0 && tsvar[2] != 0) {
      stop("Non-seasonal data cannot be modelled using a seasonal factor")
    }
    data <- data[subset, ]
  }
  if (!is.null(lambda)) {
    data[, 1] <- BoxCox(data[, 1], lambda)
    lambda <- attr(data[, 1], "lambda")
  }
  if (tsdat[2] == 0 && tsvar[2] != 0) {
    data$season <- factor(data$season)
  }
  fit <- lm(formula, data = data, na.action = na.exclude, ...)
  fit$data <- data
  responsevar <- deparse(formula[[2]])
  fit$residuals <- ts(residuals(fit))
  fit$x <- fit$residuals
  fit$x[!is.na(fit$x)] <- model.frame(fit)[, responsevar]
  fit$fitted.values <- ts(fitted(fit))
  tsp(fit$residuals) <- tsp(fit$x) <- tsp(fit$fitted.values) <- tsp(data[,
                                                                         1]) <- tspx
  fit$call <- cl
  fit$method <- "Linear regression model"
  if (exists("dataname")) {
    fit$call$data <- dataname
  }
  if (!is.null(lambda)) {
    attr(lambda, "biasadj") <- biasadj
    fit$lambda <- lambda
    fit$fitted.values <- InvBoxCox(fit$fitted.values, lambda,
                                   biasadj, var(fit$residuals))
    fit$x <- InvBoxCox(fit$x, lambda)
  }
  class(fit) <- c("tslm", class(fit))
  return(fit)
}


findfrequency <- function (x) {
  n <- length(x)
  x <- as.ts(x)
  x <- residuals(tslm(x ~ trend))
  n.freq <- 500
  spec <- spec.ar(c(na.contiguous(x)), plot = FALSE, n.freq = n.freq)
  if (max(spec$spec) > 10) {
    period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5)
    if (period == Inf) {
      j <- which(diff(spec$spec) > 0)
      if (length(j) > 0) {
        nextmax <- j[1] + which.max(spec$spec[(j[1] +
                                                 1):n.freq])
        if (nextmax < length(spec$freq)) {
          period <- floor(1/spec$freq[nextmax] + 0.5)
        }
        else {
          period <- 1L
        }
      }
      else {
        period <- 1L
      }
    }
  }
  else {
    period <- 1L
  }
  return(as.integer(period))
}



# tsibble -----------------------------------------------------------------


# interval_pull.Date <- function (x) {
#   dttm <- as.numeric(x)
#   ndays <- gcd_interval(dttm)
#   init_interval(day = ndays)
# }

init_interval <- function (year = 0, quarter = 0, month = 0, week = 0, day = 0,
          hour = 0, minute = 0, second = 0, millisecond = 0, microsecond = 0,
          nanosecond = 0, unit = 0) {
  structure(list(year = year, quarter = quarter, month = month,
                 week = week, day = day, hour = hour, minute = minute,
                 second = second, millisecond = millisecond, microsecond = microsecond,
                 nanosecond = nanosecond, unit = unit), class = "interval")
}

# gcd_interval <- function (x) {
#   if (length(x) < 2) {
#     0
#   }
#   else {
#     unique_x <- vec_unique(round(abs(diff(x)), digits = 6))
#     gcd_vector(unique_x)
#   }
# }
#
# gcd_vector <- function (x) {
#   Reduce(gcd, x)
# }
#
# gcd <- function (a, b){
#   if (isTRUE(all.equal(b, 0)))
#     a
#   else gcd(b, a %% b)
# }
