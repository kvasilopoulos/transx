

with_attrs <- function(out, x, seed = NULL) {
  if(getOption("trans.keep.attrs")) {
    attributes(out) <- attributes(x)
    if(!is.null(seed)) {
      out <- set_seed_attr(out, seed)
    }
    out
  }else{
    out
  }
}

with_na_rm <- na_rm_if <- function(x, na.rm = getOption("transx.na.rm")) {
  assert_na(na.rm)
  display_na(x)
  if(na.rm)
    x <- x[!is.na(x)]
  x
}


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

`%NA% `<- function(x){
  if (!is_null(x)) {
    x
  }
}
