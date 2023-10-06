
<!-- README.md is generated from README.Rmd. Please edit that file -->

# teckfish

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/poissonconsulting/teckfish/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/poissonconsulting/teckfish/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/teckfish/branch/main/graph/badge.svg)](https://app.codecov.io/gh/poissonconsulting/teckfish?branch=main)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`teckfish` is an R package for Teck Fish and Aquatic Sciences Projects.

## Installation

``` r
# install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))
pak::pak("poissonconsulting/teckfish")
```

## Example

`calculate_gsdd()` takes a numerical vector of daily temperature values
and calculates the growing season degree days (GSDD).

``` r
library(teckfish)
set.seed(13)
day <- 1:365
x <- pmax(-15 * cos((2 * pi / 365) * (day - 10)) + rnorm(365, sd = .5), 0)

gsdd_cf(x)
```

    ## [1] 1692.979
