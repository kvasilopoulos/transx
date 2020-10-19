roll_data <- function(x, n = 1L) {
  xlen <- NROW(x)
  roll_seq <- seq(0, xlen, by = n)
  out <- vector("list", length(roll_seq))
  j <- 1
  for(i in roll_seq) {
    out[[j]] <- x[1:i]
    j <- j + 1
  }
  out[-1]
}

roll_f <- function(x, f, n = 1L, fill = NA, fill_fun = NULL) {
  vec_data <- roll_data(x, n)
  xlen <- length(x)
  vec <- lapply(vec_data, f)
  body <- Reduce(c, vec)
  roll_seq <- seq(0, xlen, by = n)
  total_seq <- 1:xlen
  idx <- total_seq[-roll_seq]
  out <- idx_fill(body, idx, fill = fill, fill_fun = fill_fun)
  out
}


roll_sum <- function() {

}


roll_mean <- function(x, n = 1L, fill = NA, fill_fun = NULL){
  roll_f(x, mean, n = n, fill = fill, fill_fun = fill_fun)
}

roll_median <- function(x) {

}

