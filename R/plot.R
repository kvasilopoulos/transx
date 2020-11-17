
#' Plotting wrapper aroung plot.default
#'
#' Helper function to only plot x as a line plot.
#'
#' @template x
#' @param ... Further arguments used in `plot`.
#' @importFrom stats plot.ts
#' @export
#' @keywords internal
plotx <- function(x, ...) {
  nc <- NCOL(x)

  if(nc == 1) {
    return(plot_one(x))
  }

  dots <- rlang::dots_list(...)
  oma_top <- if(!is.null(dots$main)) 5 else 2
  if(!is.null(dots$xlab) ) {
    stop("xlab is not available", call. = TRUE)
  }
  if(!is.null(dots$ylab) ) {
    stop("ylab is not available", call. = TRUE)
  }
  plot.ts(x, xlab = "", mar.multi = c(0, 5.1, 0, 2.1), oma.multi = c(3, 0, oma_top, 0))
  graphics::grid(lty = "dashed")
}

plot_one <- function(x, ...) {
  plot(x, xlab = "", ylab = "", type = "l", ...)
  graphics::grid(lty = "dashed")
}

