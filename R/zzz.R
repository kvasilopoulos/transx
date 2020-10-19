.onLoad <- function (libnames, pkgname) {
  op <- options()
  op.transx <- list(
    transx.display = TRUE,
    transx.na.rm = TRUE,
    transx.keep.attrs = TRUE
  )
  toset <- !(names(op.transx) %in% names(op))
  if (any(toset))
    options(op.transx[toset])
  invisible()
}
