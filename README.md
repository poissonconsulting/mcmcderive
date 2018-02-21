---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



 [![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis-CI Build Status](https://travis-ci.org/poissonconsulting/mcmcderive.svg?branch=master)](https://travis-ci.org/poissonconsulting/mcmcderive)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/mcmcderive?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/mcmcderive)
[![Coverage Status](https://img.shields.io/codecov/c/github/poissonconsulting/mcmcderive/master.svg)](https://codecov.io/github/poissonconsulting/mcmcderive?branch=master)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

# mcmcderive

## Introduction

`mcmcderive` is an R package to generate derived parameters from Monte Carlo Markov Chain (MCMC) samples.
No more rerunning a model because you forget to include a derived parameter!

## Demonstration


```r
library(mcmcderive)

mcmcr_example <- mcmcr:::mcmcr_example
#> Error in get(name, envir = asNamespace(pkg), inherits = FALSE): object 'mcmcr_example' not found
mcmcr_example
#> Error in eval(expr, envir, enclos): object 'mcmcr_example' not found

gamma <- mcmc_derive(mcmcr_example, "gamma <- sum(alpha) * sigma")
#> Error in mcmc_derive(mcmcr_example, "gamma <- sum(alpha) * sigma"): object 'mcmcr_example' not found
gamma
#> function (x)  .Primitive("gamma")
```

## Installation

To install the latest version from GitHub
```
# install.packages("devtools")
devtools::install_github("poissonconsulting/mcmcderive")
```

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

Please report any [issues](https://github.com/poissonconsulting/mcmcderive/issues).

[Pull requests](https://github.com/poissonconsulting/mcmcderive/pulls) are always welcome.

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md).
By participating in this project you agree to abide by its terms.
