
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mcmcderive <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/poissonconsulting/mcmcderive/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/poissonconsulting/mcmcderive/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/mcmcderive/graph/badge.svg)](https://app.codecov.io/gh/poissonconsulting/mcmcderive)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/license/mit/)
[![CRAN
status](https://www.r-pkg.org/badges/version/mcmcderive)](https://cran.r-project.org/package=mcmcderive)
![CRAN downloads](https://cranlogs.r-pkg.org/badges/mcmcderive)
<!-- badges: end -->

## Why `mcmcderive`?

`mcmcderive` is an R package to generate derived parameter(s) from Monte
Carlo Markov Chain (MCMC) samples using R code.

This is useful because it means Bayesian models can be fitted without
the inclusion of derived parameters which add unnecessary clutter and
slows model fitting. For more information on MCMC samples see Brooks et
al. (2011).

## Demonstration

``` r
library(mcmcderive)
#> Registered S3 method overwritten by 'mcmcr':
#>   method         from 
#>   as.mcmc.nlists nlist

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

mcmc_derive(mcmcr::mcmcr_example, expr, silent = TRUE)
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

### Parallel Chains

If the MCMC object has multiple chains the run time can be substantially
reduced by generating the derived parameters for each chain in parallel.
In order for this to work it is necessary to:

1)  Ensure plyr and doParallel are installed using
    `install.packages(c("plyr", "doParallel"))`.
2)  Register a parallel backend using
    `doParallel::registerDoParallel(4)`.
3)  Set `parallel = TRUE` in the call to `mcmc_derive()`.

### Extras

To facilitate the translation of model code into R code the `extras`
package provides the R equivalent to common model functions such as
`pow()`, `phi()` and `log() <-`.

## Installation

### Release

To install the release version from
[CRAN](https://CRAN.R-project.org/package=mcmcderive).

``` r
install.packages("mcmcderive")
```

The website for the release version is at
<https://poissonconsulting.github.io/mcmcderive/>.

### Development

To install the development version from
[GitHub](https://github.com/poissonconsulting/mcmcderive)

``` r
# install.packages("remotes")
remotes::install_github("poissonconsulting/mcmcderive")
```

or from
[r-universe](https://poissonconsulting.r-universe.dev/mcmcderive).

``` r
install.packages("mcmcderive", repos = c("https://poissonconsulting.r-universe.dev", "https://cloud.r-project.org"))
```

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/mcmcderive/issues).

[Pull requests](https://github.com/poissonconsulting/mcmcderive/pulls)
are always welcome.

## Code of Conduct

Please note that the mcmcderive project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## References

Brooks, S., Gelman, A., Jones, G.L., and Meng, X.-L. (Editors). 2011.
Handbook for Markov Chain Monte Carlo. Taylor & Francis, Boca Raton.
