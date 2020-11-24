

# disp categoreis ------------------------------------------------------

# tidylog principle

disp_na <- function(x, na.rm) {
  n_na <- is.na(x)
  if(any(n_na)) {
    disp_info("The vector contains {sum(is.na(x))} NA value{?s}, `na.rm` is set to {na.rm}.",)
  }
}

# disp_outlier <- function(x) {
#   if(rlang::is_bare_atomic(x, 0)) {
#     disp_info("no outlier has been identified")
#   } else {
#     disp_success("{length(x)} outlier{?s} ha{?s/ve} been identified",)
#   }
# }

# disp -----------------------------------------------------------------

disp <- function(..., internal = FALSE) {
  with_disp(
    cli::cli_alert(..., .envir = parent.frame(1)),
    internal
  )
}

disp_info <- function(..., internal = FALSE) {
  with_disp(
    cli::cli_alert_info(..., .envir = parent.frame(1)),
    internal
  )
  # list(parent.frame(;1), parent.frame(2), parent.frame(3))
}

disp_success <- function(..., internal = FALSE) {
  with_disp(
    cli::cli_alert_success(..., .envir = parent.frame(1)),
    internal
  )
}

disp_warning <- function(..., internal = FALSE) {
  with_disp(
    cli::cli_alert_warning(..., .envir = parent.frame(1)),
    internal
  )
}

disp_danger <- function(..., internal = FALSE) {
  with_disp(
    cli::cli_alert_danger(..., .envir = parent.frame(1)),
    internal
  )
}


