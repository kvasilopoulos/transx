

# pkgs --------------------------------------------------------------------

has_pkg <- function(pkg) {
  pkg %in% loadedNamespaces()
}

is_installed <- function(pkg) {
  system.file(package = pkg) != ""
}

need_pkg <- function(pkg) {
  if (!is_installed(pkg)) {
    stop("Please install ", pkg, " package", call. = FALSE)
  }
}

# with --------------------------------------------------------------------


with_attrs <- function(out, x, seed = NULL) {
  if(getOption("transx.keep.attrs")) {
    attributes(out) <- attributes(x)
    if(!is.null(seed)) {
      out <- set_seed_attr(out, seed)
    }
    out
  }else{
    out
  }
}

with_disp <- function(x, internal = FALSE) {
  if(has_pkg(c("cli")) && getOption("transx.display") && !isTRUE(internal)) {
    x
  }
}

na_opt <- function() {
  getOption("transx.na.rm")
}

with_na_rm <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_na(na.rm)
  if(na.rm) {
    disp_na(x, na.rm)
    x <- x[!is.na(x)]
  }
  x
}

# rng ---------------------------------------------------------------------

#' @importFrom stats runif
get_rng <- function(seed) {
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    runif(1)
  if (is.null(seed)) {
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  }else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  RNGstate
}

set_seed_attr <- function(x, seed) {
  seed %||% get_rng(seed)
  attr(x, "seed") <- seed
  x
}


# misc --------------------------------------------------------------------

`%NA% `<- function(x){
  if (!is_null(x)) {
    x
  }
}

#' @importFrom rlang is_null %||%
`%!||%` <- function (x, y){
  if (is_null(x))
    x
  else y
}




