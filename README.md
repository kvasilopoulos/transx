
<!-- README.md is generated from README.Rmd. Please edit that file -->

# transx <a href='https://kvasilopoulos.github.io/transx'><img src='man/figures/logo.png' align="right" height="138" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/transx)](https://CRAN.R-project.org/package=transx)
<!-- badges: end -->

> **WARNING**: The package is still in early stages of development\!

`transx` implements several time series operations that follow an
opiniated design. The main principle of `transx` is to keep the number
of observations the same. Functions that reduce this number have as a
default to `fill` the observation gap (default: NA). However, the user
may choose to choose anothe value to fill or even provide a function
through `fill_fun` to achieve the replacement. The `fill_fun` will use
the remaining observations as the body of the argument.

## Design Principles

  - The input and the output will always be a numeric vector
  - The output retains the same length as the input
  - Uses a filling logic, where `fill` and `fill_fun` are used to keep
    the length of vector identical

Additionally,

  - `na.rm = TRUE` is the default when needed
  - Display informative message for the transformation procedure.

## Installation

You can install the development version from
[Github](https://github.com/kvasilopoulos/transx).

```` r
remotes::install_github("transx")
```## Motivation



### Example 


```r
library(transx)
````

``` r
x <- c(5,3,2,2,5)
lagx(x)
#> [1] NA  5  3  2  2
lagx(x, fill = 1)
#> [1] 1 5 3 2 2
lagx(x, fill_fun = mean)
#> [1] 3 5 3 2 2
# lagx(x, fill_fun = fill_nocb)
```
