trig_asn <- function(x, ...) {
  2 * asin(sqrt(x), ...)
}

trig_atahn <- function(x, ...) {
  atanh(x, ...)
}


# Probability -------------------------------------------------------------


dist_logit <- function(x, ...) {
  assert_uni_ts(x)
  out <- dist(x, "logis", ...)
  with_attrs(out, x)
}

dist_probit <- function(x, ...) {
  assert_uni_ts(x)
  out <- dist(x, "norm", ...)
  with_attrs(out, x)
}


dist <- function(x, dist, ...) {
  qfun <- match.fun(paste0("q", dist))
  qfun(x, ...)
}

dist_inv <- function(x, dist, ...) {
  pfun <- match.fun(paste0("p", dist))
  pfun(x, ...)
}
