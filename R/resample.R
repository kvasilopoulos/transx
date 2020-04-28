resample <- function(x) {
  nr <- length(x)
  boot_idx <- sample(1:nr, replace = TRUE)
  x[boot_idx]
}

resample_norm <- function(x) {
  nr <- length(x)
  gauss <- rnorm(nr)
  x*gauss
}

resample_rademacher <- function(x) {
  nr <- length(x)
  rad <- sample(c(-1, 1), nr, replace = TRUE)
  x*rad
}

resample_mammen <- function(x) {
  nr <- length(x)
  mam <- sample(
    x = c(-(sqrt(5) - 1)/2, (sqrt(5) + 1)/2),
    prob = c((sqrt(5) + 1)/(2 * sqrt(5)), (sqrt(5) - 1)/(2 * sqrt(5))),
    size = nr,
    replace = TRUE)
  x*mam
}
