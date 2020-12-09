# TODO How to make new fill functions

new_fill <- function(..., body, idx) {
  vec <- new_vec(body, idx)
  structure(
    ...,
    class = "fill"
  )
}
