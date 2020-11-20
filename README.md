
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

> **WARNING**: The package is still in early stages of development!

`transx` implements several time series operations that follow an
opiniated design. The main principle of `transx` is to keep the number
of observations the same. Functions that reduce this number have as a
default to `fill` the observation gap (default: NA). However, the user
may choose to choose anothe value to fill or even provide a function
through `fill_fun` to achieve the replacement. The `fill_fun` will use
the remaining observations as the body of the argument.

## Design Principles

-   The input and the output will always be a numeric vector
-   The output retains the same length as the input
-   Uses a filling logic, where `fill` is used to keep the length of
    vector identical

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

### Basic Example

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

### Outliers

``` r
x <- c(-2,0,3, 50)
out_iqr(x)
#> [1] -2  0  3 NA
# Identify outlier and give a value with fill option
out_iqr(x, fill = 0)
#> [1] -2  0  3  0

x2 <- rnorm(20)
x2[10] <- 12
out_score_z(x2)
#>  [1] -0.89889 -0.75576  0.72596  0.86864 -1.34219 -0.58945 -0.31021 -2.28387  0.99317
#> [10]       NA -0.07489 -1.41920  0.36181  0.51493 -0.45579  0.12874  0.39123  0.66809
#> [19]  1.02677 -0.02443
```

### Filters

``` r
unemp <- ggplot2::economics$unemploy
filtered <- cbind(filter_hamilton(unemp), filter_hp(unemp, select_lambda("monthly")))
plotx(filtered)
```

<img src="man/figures/README-filtered-1.png" width="100%" />

## Code of Conduct

Please note that the transx project is released with a [Contributor Code
of
Conduct](https://kvasilopoulos.github.io/transx//CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
