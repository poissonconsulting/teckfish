
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
# install.packages("devtools")
devtools::install_github("poissonconsulting/teckfish")
```

## Demonstration

### Growing Season Degree Days

`gsdd_cf()` takes a numerical vector of mean daily temperature values in
centigrade and calculates the growing season degree days (GSDD) based on
Coleman and Fausch’s (2007) definition.

``` r
library(teckfish)
gsdd_cf(teckfish::simulated_data$synthetic)
#> [1] 3898.806
```

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
