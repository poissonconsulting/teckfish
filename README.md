
<!-- README.md is generated from README.Rmd. Please edit that file -->

# teckfish

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of teckfish is to facilitate reproducible calculation of fish
growth metrics such as seasonal growing degree days.

License: MIT + file LICENSE

## Installation

``` r
remotes::install_github("poissonconsulting/teckfish")
```

    ## Using github PAT from envvar GITHUB_PAT

    ## Skipping install of 'teckfish' from a github remote, the SHA1 (380b6e7a) has not changed since last install.
    ##   Use `force = TRUE` to force installation

## Example

calculate_gsdd() takes a numerical vector and returns a sum of the
growing degree days.

``` r
library(teckfish)
day <- 1:365
x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)

calculate_gsdd(x=x, rollmean_units = 7, start_temp = 5, end_temp = 4, n_consecutive = 5)
```
