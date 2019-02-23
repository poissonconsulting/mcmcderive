
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis-CI Build
Status](https://travis-ci.org/poissonconsulting/mcmcderive.svg?branch=master)](https://travis-ci.org/poissonconsulting/mcmcderive)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/mcmcderive?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/mcmcderive)
[![Coverage
Status](https://img.shields.io/codecov/c/github/poissonconsulting/mcmcderive/master.svg)](https://codecov.io/github/poissonconsulting/mcmcderive?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

# mcmcderive

## Introduction

`mcmcderive` is an R package to generate derived parameters from Monte
Carlo Markov Chain (MCMC) samples using R code. The derived parameters
are returned as an [`mcmcr`](https://github.com/poissonconsulting/mcmcr)
object.

The `mcmcderive` package also provides the R equivalent to functions
such as

  - `pow()`
  - `phi()`
  - `log() <-`
  - `logit() <-`

that often occur in template model code.

## Demonstration

``` r
library(mcmcderive)

mcmcr::mcmcr_example
#> $alpha
#> [1] 3.718025 4.718025
#> 
#> nchains:  2 
#> niters:  400 
#> 
#> $beta
#>           [,1]     [,2]
#> [1,] 0.9716535 1.971654
#> [2,] 1.9716535 2.971654
#> 
#> nchains:  2 
#> niters:  400 
#> 
#> $sigma
#> [1] 0.7911975
#> 
#> nchains:  2 
#> niters:  400

expr <- "
  log(alpha2) <- alpha
  gamma <- sum(alpha) * sigma
"

mcmc_derive(mcmcr::mcmcr_example, expr)
#> $alpha2
#> [1]  41.18352 111.94841
#> 
#> nchains:  2 
#> niters:  400 
#> 
#> $gamma
#> [1] 6.60742
#> 
#> nchains:  2 
#> niters:  400
```

## Installation

To install the latest development version from the Poisson drat
[repository](https://github.com/poissonconsulting/drat)

``` r
# install.packages("drat")
drat::addRepo("poissonconsulting")
install.packages("mcmcderive")
```

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/mcmcderive/issues).

[Pull requests](https://github.com/poissonconsulting/mcmcderive/pulls)
are always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.
