

# disp categoreis ------------------------------------------------------

# tidylog principle

disp_na <- function(x, na.rm) {
  n_na <- is.na(x)
  if(any(n_na)) {
    n_na <- sum(n_na)
    disp_info("The vector contains {n_na} NA value{?s}, `na.rm` is set to {na.rm}.")
  }
}

disp_outlier <- function(x) {
  if(rlang::is_bare_atomic(x, 0)) {
    disp_info("no outlier has been identified")
  } else {
    disp_sucess("{length(x)} outlier{?s} ha{?s/ve} been identified")
  }
}

# disp -----------------------------------------------------------------

disp <- function(...) {
  with_disp(
    cli::cli_alert(...)
  )
}

disp_info <- function(...) {
  with_disp(
    cli::cli_alert_info(...)
  )
}

disp_sucess <- function(...) {
  with_disp(
    cli::cli_alert_success(...)
  )
}

disp_warning <- function(...) {
  with_disp(
    cli::cli_alert_warning(...)
  )
}

disp_danger <- function(...) {
  with_disp(
    cli::cli_alert_danger(...)
  )
}


