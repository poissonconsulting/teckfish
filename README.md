
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/poissonconsulting/teckfish/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/poissonconsulting/teckfish/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/teckfish/branch/main/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/teckfish?branch=main)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

# teckfish

## Introduction

`teckfish` is an R package for Teck Fish and Aquatic Sciences Projects.
It is intended to be used by Teck staff and contractors and anyone else
who finds it useful.

The intention is to eventually migrate it to the [Teck Resources RDS
GitHub organization](https://github.com/TeckResourcesTDS).

## Installation

``` r
# install.packages("remotes")
remotes::install_github("poissonconsulting/teckfish")
```

## Demonstration

### Growing Season Degree Days

`gsdd()` takes data frame with a `date` and `temperature` column with
the mean daily water temperature in centigrade and calculates the
growing season degree days (GSDD).

``` r
library(teckfish)
gsdd(gsdd::temperature_data)
#> # A tibble: 1 × 2
#>    year  gsdd
#>   <int> <dbl>
#> 1  2019 3899.
```

`gdd` calculate the growing degree days (GDD) to a date.

``` r
gdd(gsdd::temperature_data, end_date = as.Date("1972-08-30"))
#> # A tibble: 1 × 2
#>    year   gdd
#>   <int> <dbl>
#> 1  2019 3102.
```

`gss` calculates the growing season(s) (GSS).

``` r
gss(gsdd::temperature_data)
#> # A tibble: 1 × 5
#> # Groups:   year [1]
#>    year start_dayte end_dayte   gsdd truncation
#>   <int> <date>      <date>     <dbl> <chr>     
#> 1  2019 1971-03-20  1971-11-07 3899. none
```

`gss_plots` plots the temperature time series including growing
season(s), moving average and thresholds.

``` r
gss_plot(gsdd::temperature_data)
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/teckfish/issues).

[Pull requests](https://github.com/poissonconsulting/teckfish/pulls) are
always welcome.

## Code of Conduct

Please note that the teckfish project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
