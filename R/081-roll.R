
# length is not affected by n only in the beginning
# step = 2
# nx = 6
# nx/step


# idx ---------------------------------------------------------------------

#' @rdname roll
#' @importFrom rlang is_bare_integer
#' @examples
#'
#' # x is odd
#'
#' roll_idx(1:9, 2, 1, align = "left")
#' roll_idx(1:9, 2, 1, align = "center")
#' roll_idx(1:9, 2, 1, align = "right") # reduces
#'
#' # This works
#' roll_idx(1:9, 3, 1, align = "left")
#' roll_idx(1:9, 3, 1, align = "center")
#' roll_idx(1:9, 3, 1, align = "right")
#'
#' roll_idx(1:9, 2, 2, align = "left")
#' roll_idx(1:9, 2, 2, align = "center")
#' roll_idx(1:9, 2, 2, align = "right")
#'
#' roll_idx(1:9, 3, 2, align = "left")
#' roll_idx(1:9, 3, 2, align = "center")
#' roll_idx(1:9, 3, 2, align = "right") # reduces
#'
#' # x is even
#'
#' roll_idx(1:8, 2, 2, align = "left")
#' roll_idx(1:8, 2, 2, align = "center")
#' roll_idx(1:8, 2, 2, align = "right") # reduces
#'
#' roll_idx(1:8, 2, 1, align = "left")
#' roll_idx(1:8, 2, 1, align = "center")
#' roll_idx(1:8, 2, 1, align = "right")
#'
#' roll_idx(1:8, 3, 1, align = "left")
#' roll_idx(1:8, 3, 1, align = "center")
#' roll_idx(1:8, 3, 1, align = "right") # reduces
#'
#' roll_idx(1:8, 3, 1, align = "left")
#' roll_idx(1:8, 3, 1, align = "center")
#' roll_idx(1:8, 3, 1, align = "right") # reduces
#'
roll_idx <- function(x, n = 1, step = 1, align = c("left", "center", "right"), complete = TRUE) {

  # align works only if step > 1

  xlen <- length(x)
  align <- match.arg(align)
  # if n is odd else
  begin <- switch(
    align,
    left = 1L,
    center = ceiling(n/2),
    right = n
  )
  seq.int(begin, xlen, by = step)
  # seq.int(begin, xlen - begin + 1, by = step)

  # nf <- if(n == 1L) n else n - 1
  # n_idx  <- if(nf > 1L) 1:nf else integer(0)
  # if(step > 1L) {
  #   step_idx <- seq_along(x)[-seq(nf, length(x), by = step)]
  #   idx <- if(is_bare_integer(n_idx, 0)) step_idx else c(n_idx, step_idx)
  # }
}

roll_fill_idx <- function(x, n = 1, step = 1, align = c("left", "center", "right")) {
  xlen <- length(x)
  align <- match.arg(align)
  xseq <- roll_idx(x, n, step = step, align = align)
  which(!1:xlen %in% xseq)
}




#' Rolling operations
#'
#' Apply rolling operations over a moving window for size `n` and increment `step`.
#'
#' @template x
#'
#' @param fn `[function]`
#'
#' @param n `[positive integer(1):1L]`
#'
#' Window size.
#'
#' @param step `[positive integer(1):1L]`
#'
#' Rolling window step.
#'
#' @template fill
#'
#' @param align `[character(1): "left"]`
#'
#' Specifying whether the index of the result should be left- or right-aligned
#' or centered (default) compared to the rolling window of observations.
#'
#' @param ... Further arguments passed to the function `fn`.
#'
#' @return
#'
#' * `roll()`: returns a vector with the same class and attributes as the input vector.
#' * `roll_idx()`: Returns the index that is used to calculate the subsets.
#' * `roll_data()`: Returns a list subsets of length length(x)/step.
#'
#' @export
#' @examples
#'
#' x <- seq(10, 1, -1)
#'
#' roll_data(x, 2, align = "left")
#' roll_data(x, 2, align = "right")
#'
#' roll(x, max, 3)
#' roll(x, max, 3, align = "right")
#'
roll <- function(x, fn, n = 1L, step = 1L, fill = NA, align = c("left", "center", "right"), ...) {

  vec_data <- roll_data(x, n, step = step)
  vec  <- lapply(vec_data, as_fn(fn, ...))
  body <- Reduce(c, vec)
  align <- match.arg(align)
  idx <- roll_fill_idx(x, n, step = step, align = align)
  out  <- fill_(body, idx, fill = fill)
  with_attrs(out, x)
}

#' @rdname roll
#' @export
#' @examples
#'
#' x <- 1:6
#' roll_data(x, 2)
#' roll(x, mean, 2)
#'
#'roll_data(x, 2, 2)
#' roll(x, mean, 2, 2)
roll_data <- function(x, n = 1L, step = 1L, align = c("left", "center", "right")) {
  # xlen <- NROW(x)
  align <- match.arg(align)
  idx <- roll_idx(x, n, step = step, align = align) #seq.int(1L, xlen - n + 1, by = step)

  out <- vector("list", length(roll_idx))
  for(i in 1:length(idx)) {
    out[[i]] <- x[idx[i]:(idx[i] + (n - 1))]
  }
  out
}

as_fn <- function(fn, ...) {
  fn <- as_function(fn)
  function(x) fn(x, ...)
}



# Roll statistics ---------------------------------------------------------

roll_sum <- function(x, n = 1L, step = 1L, fill = NA) {
  roll(x, sum, n = n, fill = fill)
}

roll_min <- function(x, n = 1L, step = 1L, fill = NA) {
  roll(x, min, n = n, fill = fill)
}

roll_max <- function(x, n = 1L, step = 1L, fill = NA) {
  roll(x, max, n = n, step = step, fill = fill)
}

roll_prod <- function(x, n = 1L, step = 1L, fill = NA) {
  roll(x, prod, n = n, fill = fill)
}

# Centrality ----

roll_mean <- function(x, n = 1L, step = 1L, fill = NA){
  roll(x, mean, n = n, step = step, fill = fill)
}

roll_median <- function(x, n = 1L, step = 1L, fill = NA) {
  roll(x, mean, n = n, fill = fill)
}

roll_mode <- function(x, n = 1L, step = 1L, fill = NA) {
  roll(x, modex, n = n, fill = fill)
}

# Dispersion ----

roll_var <- function(x, n = 1L, step = 1L, fill = NA){
  roll(x, var, n = n, step = step, fill = fill)
}

roll_sd <- function(x, n = 1L, step = 1L, fill = NA) {
  roll(x, sd, n = n, fill = fill)
}

# Shape ----

roll_skewness <- function(x, n = 1L, step = 1L, fill = NA) {
  roll(x, skewness, n = n, fill = fill)
}

roll_kurtosis <- function(x, n = 1L, step = 1L, fill = NA){
  roll(x, kurtosis, n = n, step = step, fill = fill)
}

