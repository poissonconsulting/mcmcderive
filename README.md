---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



<!-- badges: start -->
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis-CI Build Status](https://travis-ci.org/poissonconsulting/mcmcderive.svg?branch=master)](https://travis-ci.org/poissonconsulting/mcmcderive)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/mcmcderive?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/mcmcderive)
[![Coverage Status](https://img.shields.io/codecov/c/github/poissonconsulting/mcmcderive/master.svg)](https://codecov.io/github/poissonconsulting/mcmcderive?branch=master)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![CRAN status](https://www.r-pkg.org/badges/version/mcmcderive)](https://cran.r-project.org/package=mcmcderive)
![CRAN downloads](http://cranlogs.r-pkg.org/badges/mcmcderive)
[![R build status](https://github.com/poissonconsulting/mcmcderive/workflows/R-CMD-check/badge.svg)](https://github.com/poissonconsulting/mcmcderive/actions)
[![Codecov test coverage](https://codecov.io/gh/poissonconsulting/mcmcderive/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/mcmcderive?branch=master)
<!-- badges: end -->

# mcmcderive

## Why `mcmcderive`?

`mcmcderive` is an R package to generate derived parameter(s) from Monte Carlo Markov Chain (MCMC) samples using R code.

This is useful because it means Bayesian models can be fitted without the inclusion of derived parameters which add unnecessary clutter and slows model fitting.
For more information on MCMC samples see Brooks et al. (2011).

## Installation

To install the latest release version from [CRAN](https://cran.r-project.org/package=mcmcderive)
```
install.packages("mcmcderive")
```

To install the latest development version from [GitHub](https://github.com/poissonconsulting/mcmcderive)
```
remotes::install_github("poissonconsulting/mcmcderive")
```

## Demonstration


```r
library(mcmcderive)
#> Error in library(mcmcderive): there is no package called 'mcmcderive'

mcmcr::mcmcr_example
#> Error in loadNamespace(name): there is no package called 'mcmcr'

expr <- "
  log(alpha2) <- alpha
  gamma <- sum(alpha) * sigma
"

mcmc_derive(mcmcr::mcmcr_example, expr, silent = TRUE)
#> Error in mcmc_derive(mcmcr::mcmcr_example, expr, silent = TRUE): could not find function "mcmc_derive"
```

### Parallel Chains

If the MCMC object has multiple chains the run time can be substantially reduced by generating the derived parameters for each chain in parallel.
In order for this to work it is necessary to:

1) Ensure plyr and doParallel are installed using `install.packages(c("plyr", "doParallel"))`.
2) Register a parallel backend using `doParallel::registerDoParallel(4)`.
3) Set `parallel = TRUE` in the call to `mcmc_derive()`.

### Extras

To facilitate the translation of model code into R code the `mcmcderive` package also provides the R equivalent to common model functions such as `pow()`, `phi()` and `log() <- `.

## Contribution

Please report any [issues](https://github.com/poissonconsulting/mcmcderive/issues).

[Pull requests](https://github.com/poissonconsulting/mcmcderive/pulls) are always welcome.

Please note that the 'mcmcderive' project is released with a [Contributor Code of Conduct](https://poissonconsulting.github.io/mcmcderive/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## References

Brooks, S., Gelman, A., Jones, G.L., and Meng, X.-L. (Editors). 2011. Handbook for Markov Chain Monte Carlo. Taylor & Francis, Boca Raton.
