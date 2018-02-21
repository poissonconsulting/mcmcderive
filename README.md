
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
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
Carlo Markov Chain (MCMC) samples. No more rerunning a model because you
forget to include a derived parameter\!

## Demonstration

``` r
library(mcmcderive)

mcmcr_example <- mcmcr::mcmcr_example
mcmcr_example
#> $alpha
#> [1] 3.01883 4.01883
#> nchains:  2 
#> niters:  200 
#> 
#> $beta
#>         [,1]    [,2]
#> [1,] 0.79625 1.79625
#> [2,] 1.79625 2.79625
#> nchains:  2 
#> niters:  200 
#> 
#> $sigma
#> [1] 0.7911975
#> nchains:  2 
#> niters:  200

gamma <- mcmc_derive(mcmcr_example, "gamma <- sum(alpha) * sigma")
gamma
#> $gamma
#> [1] 5.576104
#> nchains:  2 
#> niters:  200
```

## Installation

To install the latest version from GitHub

    # install.packages("devtools")
    devtools::install_github("poissonconsulting/mcmcderive")

## Citation

``` 

To cite package 'mcmcderive' in publications use:

  Joe Thorley (2018). mcmcderive: Derive MCMC Parameters. R
  package version 0.0.0.9045.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {mcmcderive: Derive MCMC Parameters},
    author = {Joe Thorley},
    year = {2018},
    note = {R package version 0.0.0.9045},
  }
```

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/mcmcderive/issues).

[Pull requests](https://github.com/poissonconsulting/mcmcderive/pulls)
are always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.
