
<!-- README.md is generated from README.Rmd. Please edit that file -->

# transx <a href='https://kvasilopoulos.github.io/transx'><img src='man/figures/logo.png' align="right" height="138" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/transx)](https://CRAN.R-project.org/package=transx)
[![R build
status](https://github.com/kvasilopoulos/transx/workflows/R-CMD-check/badge.svg)](https://github.com/kvasilopoulos/transx/actions)
[![Codecov test
coverage](https://codecov.io/gh/kvasilopoulos/transx/branch/master/graph/badge.svg)](https://codecov.io/gh/kvasilopoulos/transx?branch=master)
<!-- badges: end -->

`transx` implements several univariate time series operations that
follow an opinionated design. The main principle of `transx` is to keep
the number of observations the same. Functions that reduce this number
have to `fill` the observations gap.

## Design Principles

-   The input and the output will always be a numeric vector.
-   The output retains the same length as the input.
-   Uses a filling logic, where `fill` is used to keep the length of
    vector identical.

### Optional:

-   `na.rm`: Which sets`na.rm  = TRUE` by default when needed to.
-   `keep.attrs`: Which after manipulations the new series would retain
    the same attributes.
-   `display`: Display informative message for the transformation
    procedure.

## Installation

You can install the development version from
[Github](https://github.com/kvasilopoulos/transx).

``` r
remotes::install_github("transx")
```

### Usage

This is a basic example with lagged and leading values. `fill` can be
achieved either by value or by function. The function can be a build-in
function such as mean, or median, that fill-in by a single values, or it
can be of the `fill_*` family such as `fill_locf` and `fill_nocb` that
consider the location of the observations before performing the filling.

``` r
library(transx)

x <- c(5,3,2,2,5)
lagx(x)
#> [1] NA  5  3  2  2
lagx(x, fill = 1)
#> [1] 1 5 3 2 2
lagx(x, fill = mean)
#> [1] 3 5 3 2 2
lagx(x, fill = fill_nocb)
#> [1] 5 5 3 2 2
```

## Code of Conduct

Please note that the transx project is released with a [Contributor Code
of
Conduct](https://kvasilopoulos.github.io/transx//CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
