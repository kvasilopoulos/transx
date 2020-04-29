#' Bootstrap resampling
#'
#' @param x numeric vector
#'
#' @export
resample <- function(x) {
  nr <- length(x)
  boot_idx <- sample(1:nr, replace = TRUE)
  x[boot_idx]
}

#' Bootstrap resampling
#'
#' @param x numeric vector
#'
#' @export
resample_norm <- function(x) {
  nr <- length(x)
  gauss <- rnorm(nr)
  x*gauss
}

#' Bootstrap resampling
#'
#' @param x numeric vector
#'
#' @export
resample_rademacher <- function(x) {
  nr <- length(x)
  rad <- sample(c(-1, 1), nr, replace = TRUE)
  x*rad
}

#' Bootstrap resampling
#'
#' @param x numeric vector
#'
#' @export/
resample_mammen <- function(x) {
  nr <- length(x)
  mam <- sample(
    x = c(-(sqrt(5) - 1)/2, (sqrt(5) + 1)/2),
    prob = c((sqrt(5) + 1)/(2 * sqrt(5)), (sqrt(5) - 1)/(2 * sqrt(5))),
    size = nr,
    replace = TRUE)
  x*mam
}


# not sure about these two ------------------------------------------------


resample_jackknife <- function(x, omit = 1L, default = NA, position = NULL) {
  rs <- resample(x, ...)
  nr <- length(x)
  if (is.null(position)) {
    position <- sample.int(nr, omit)
  }
  rs[position] <- NA
  rs
}

resample_permutation <- function(x) {

}
