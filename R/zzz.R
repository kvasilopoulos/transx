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

# Set Global Variables to avoid NOTES in cmdchecks
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("fill")
  )
}
