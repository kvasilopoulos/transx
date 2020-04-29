
<!-- README.md is generated from README.Rmd. Please edit that file -->

# transx

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/transx)](https://CRAN.R-project.org/package=transx)
<!-- badges: end -->

> **WARNING**: The package is still in early stages of development\!

`transx` implements several time series operations that follow an
opiniated design. We abstract from the common `ts` object and we focus
on numeric series. \#\# Installation

You can install the development version from
[Github](https://github.com/kvasilopoulos/transx).

``` r
remotes::install_github("transx")
```

## Motivation

### Problem

``` r
library(dplyr)

illu_data <- gapminder::gapminder %>% 
  filter(country == "Afghanistan") 

illu_data %>% 
  mutate(pop_lag = stats::lag(pop))
#> # A tibble: 12 x 7
#>    country     continent  year lifeExp      pop gdpPercap  pop_lag
#>    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>    <int>
#>  1 Afghanistan Asia       1952    28.8  8425333      779.  8425333
#>  2 Afghanistan Asia       1957    30.3  9240934      821.  9240934
#>  3 Afghanistan Asia       1962    32.0 10267083      853. 10267083
#>  4 Afghanistan Asia       1967    34.0 11537966      836. 11537966
#>  5 Afghanistan Asia       1972    36.1 13079460      740. 13079460
#>  6 Afghanistan Asia       1977    38.4 14880372      786. 14880372
#>  7 Afghanistan Asia       1982    39.9 12881816      978. 12881816
#>  8 Afghanistan Asia       1987    40.8 13867957      852. 13867957
#>  9 Afghanistan Asia       1992    41.7 16317921      649. 16317921
#> 10 Afghanistan Asia       1997    41.8 22227415      635. 22227415
#> 11 Afghanistan Asia       2002    42.1 25268405      727. 25268405
#> 12 Afghanistan Asia       2007    43.8 31889923      975. 31889923

# dplyr has thought about this problem
illu_data%>% 
  mutate(pop_lag = dplyr::lag(pop))
#> # A tibble: 12 x 7
#>    country     continent  year lifeExp      pop gdpPercap  pop_lag
#>    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>    <int>
#>  1 Afghanistan Asia       1952    28.8  8425333      779.       NA
#>  2 Afghanistan Asia       1957    30.3  9240934      821.  8425333
#>  3 Afghanistan Asia       1962    32.0 10267083      853.  9240934
#>  4 Afghanistan Asia       1967    34.0 11537966      836. 10267083
#>  5 Afghanistan Asia       1972    36.1 13079460      740. 11537966
#>  6 Afghanistan Asia       1977    38.4 14880372      786. 13079460
#>  7 Afghanistan Asia       1982    39.9 12881816      978. 14880372
#>  8 Afghanistan Asia       1987    40.8 13867957      852. 12881816
#>  9 Afghanistan Asia       1992    41.7 16317921      649. 13867957
#> 10 Afghanistan Asia       1997    41.8 22227415      635. 16317921
#> 11 Afghanistan Asia       2002    42.1 25268405      727. 22227415
#> 12 Afghanistan Asia       2007    43.8 31889923      975. 25268405
```

However, this problem extends to all the univariate functions that are
applied in the same manner in a data.frame. For example

``` r
illu_data %>% 
  mutate(pop_diff = base::diff(pop))
#> Error: Column `pop_diff` must be length 12 (the number of rows) or one, not 11
```

The idea for `transx` is coming from the need to construct wrapper
functions.

``` r
diffx <- function(x, ...) x - dplyr::lag(x, ... )

illu_data %>% 
    mutate(pop_diff = diffx(pop))
#> # A tibble: 12 x 7
#>    country     continent  year lifeExp      pop gdpPercap pop_diff
#>    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>    <int>
#>  1 Afghanistan Asia       1952    28.8  8425333      779.       NA
#>  2 Afghanistan Asia       1957    30.3  9240934      821.   815601
#>  3 Afghanistan Asia       1962    32.0 10267083      853.  1026149
#>  4 Afghanistan Asia       1967    34.0 11537966      836.  1270883
#>  5 Afghanistan Asia       1972    36.1 13079460      740.  1541494
#>  6 Afghanistan Asia       1977    38.4 14880372      786.  1800912
#>  7 Afghanistan Asia       1982    39.9 12881816      978. -1998556
#>  8 Afghanistan Asia       1987    40.8 13867957      852.   986141
#>  9 Afghanistan Asia       1992    41.7 16317921      649.  2449964
#> 10 Afghanistan Asia       1997    41.8 22227415      635.  5909494
#> 11 Afghanistan Asia       2002    42.1 25268405      727.  3040990
#> 12 Afghanistan Asia       2007    43.8 31889923      975.  6621518
```

## Categories

### Basic Trasformation

  - \[\] leadx: Lead values

  - \[\] lagx: Lag values

  - \[\] diffx: Simple Differences

  - \[\] ldiffx: Logarithic Differences

  - \[\] rdiffx: Percentage change

  - \[\] standarise

  - \[\] normalise

  - \[\] normalise0

  - \[\] minmax

  - \[\] demean

## Power Transform

  - \[\] boc\_cox: Box-Cox
  - \[\] yeo\_johnsonYeo-Johnson

## Trend

### Deterministic Trend

  - \[\] dtrend\_lin():
  - \[\] dtrend\_quad():
  - \[\] dtrend\_poly():

### Stochastic Trend

  - \[\] st\_arima

### Cycle Trend Decomposition

filter\_\*

  - \[\] filter\_hp(): Hodrick Prescot

  - \[\] filter\_bk(): Baxter King

  - \[\] filter\_cf(): Chirstiano Fitzgerlard

  - \[\] filter\_bw(): Buttersword

  - \[\] filter\_tr(): Trigonometric

  - \[\] decomp\_bn(): Beveridge-Nelson decomposition

  - \[\] filter\_ham: Hamilton

## Outliers

out\_\*

  - \[\] out\_drop()
  - \[\] out\_trim()
  - \[\] out\_trim\_qt()
  - \[\] out\_winsorize()

## Smoothers

sm\_\*

2.  Local smoothers

<!-- end list -->

  - \[\] smooth\_holt\_winters: Holt Winters -
    <https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/HoltWinters>
  - \[\] smooth\_exp(): Expontential Smoothin
  - \[\] smooth\_ma():
  - \[\] smooth\_mm():
  - \[\] smooth\_gaussian:
  - \[\] smooth\_wiener:
  - \[\] smooth\_kalman() :
  - \[\] smooth\_nn: Nearest neighbor smoother
  - \[\] smooth\_kernel(): Kernel smoothers
  - \[\] smooth\_spline(): Smoothing splines
    <!-- - [] sm_loess(): LOESS (locally estimated scatterplot smoother)  -->

## Resampling

  - \[\] resample():
  - \[\] resample\_norm():
  - \[\] resample\_rad():
  - \[\] resample\_mammen():

## Window Operations

### Recursive

rec\_\*

  - \[\] rec\_fun
  - \[\] rec\_sample
  - \[\] rec\_sum
  - \[\] rec\_mean
  - \[\] rec\_median
  - \[\] rec\_prod
  - \[\] rec\_var
  - \[\] rec\_sd <!-- - [] rec_qt
    - [] rec_skew
    - [] rec_kurt -->
  - \[\] rec\_max
  - \[\] rec\_min

### Rolling

roll\_\*

  - \[\] roll\_fun
  - \[\] roll\_sample
  - \[\] roll\_sum
  - \[\] roll\_mean
  - \[\] roll\_median
  - \[\] roll\_prod
  - \[\] roll\_var
  - \[\] roll\_sd
  - \[\] roll\_max
  - \[\] roll\_min

### Block

blck\_\*

  - \[\] blck\_fun
  - \[\] blck\_sample
  - \[\] blck\_sum
  - \[\] blck\_mean
  - \[\] blck\_median
  - \[\] blck\_prod
  - \[\] blck\_var
  - \[\] blck\_sd
  - \[\] blck\_max
  - \[\] blck\_min
