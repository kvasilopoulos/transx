# tidylog principle
#
display_fill <- function() {

}

display_na <- function(x) {
  is_na <- is.na(x)
  if(any(is_na)) {
    n_na <- sum(is_na)
    display(sprintf("The vector contains %d NA value(s).", n_na))
  }
}

display <- function(msg) {
  if(getOption("transx.display")) {
    message(msg)
  }
}


# glue --------------------------------------------------------------------

transform_glue <- function(transformer) {
  function(..., .sep = "", .envir = parent.frame(), .open = "{", .close = "}", .na = "NA") {
    glue( ..., .sep = .sep, .envir = .envir, .open = .open, .close = .close,
      .na = "NA", .transformer = transformer
    )
  }
}

conditional_msg <- function(msg, condition) {
  if(condition)
    msg
}

# glue_if <- transform_glue(conditional_msg)
