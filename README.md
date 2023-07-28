
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

    ## Downloading GitHub repo poissonconsulting/teckfish@HEAD

    ## 
    ## ── R CMD build ─────────────────────────────────────────────────────────────────
    ##      checking for file ‘/private/var/folders/1_/v3f3x7gj4kl88xm29qnsz3gw0000gn/T/RtmpDfYNyL/remotesb9f126bfc0e0/poissonconsulting-teckfish-41495090898720ddcc03387956bf7d6cb5b49c6e/DESCRIPTION’ ...  ✔  checking for file ‘/private/var/folders/1_/v3f3x7gj4kl88xm29qnsz3gw0000gn/T/RtmpDfYNyL/remotesb9f126bfc0e0/poissonconsulting-teckfish-41495090898720ddcc03387956bf7d6cb5b49c6e/DESCRIPTION’
    ##   ─  preparing ‘teckfish’:
    ##      checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
    ##   ─  checking for LF line-endings in source and make files and shell scripts
    ##   ─  checking for empty or unneeded directories
    ##   ─  building ‘teckfish_0.0.0.9000.tar.gz’
    ##      
    ## 

## Example

calculate_gsdd() takes a numerical vector and returns a sum of the
growing degree days.

``` r
library(teckfish)
day <- 1:365
x <- -15 * cos((2*pi / 365) * (day-10)) + rnorm(365, mean = 10, sd = .5)

calculate_gsdd(x=x, rollmean_units = 7, start_temp = 5, end_temp = 4, n_consecutive = 5)
```
