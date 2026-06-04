# MCMC Derive

Generates an MCMC object with derived parameters from an MCMC object.

## Usage

``` r
mcmc_derive(object, ...)

# S3 method for class 'nlist'
mcmc_derive(
  object,
  expr,
  values = list(),
  monitor = ".*",
  primary = FALSE,
  silent = getOption("mcmcderive.silent", FALSE),
  ...
)

# S3 method for class 'nlists'
mcmc_derive(
  object,
  expr,
  values = list(),
  monitor = ".*",
  primary = FALSE,
  silent = getOption("mcmcderive.silent", FALSE),
  ...
)

# S3 method for class 'mcmc'
mcmc_derive(
  object,
  expr,
  values = list(),
  monitor = ".*",
  primary = FALSE,
  silent = getOption("mcmcderive.silent", FALSE),
  ...
)

# S3 method for class 'mcmc.list'
mcmc_derive(
  object,
  expr,
  values = list(),
  monitor = ".*",
  primary = FALSE,
  parallel = FALSE,
  silent = getOption("mcmcderive.silent", FALSE),
  ...
)

# S3 method for class 'mcmcr'
mcmc_derive(
  object,
  expr,
  values = list(),
  monitor = ".*",
  primary = FALSE,
  parallel = FALSE,
  silent = getOption("mcmcderive.silent", FALSE),
  ...
)

# S3 method for class 'mcmcrs'
mcmc_derive(
  object,
  expr,
  values = list(),
  monitor = ".*",
  primary = FALSE,
  parallel = FALSE,
  silent = getOption("mcmcderive.silent", FALSE),
  ...
)
```

## Arguments

- object:

  An MCMC object.

- ...:

  Unused.

- expr:

  A string of the R code defining the values of the derived parameter(s)
  with respect to the parameters in object.

- values:

  A named list of additional R objects to evaluate in the R expression.

- monitor:

  A regular expression specifying the derived parameter(s) in expr to
  monitor.

- primary:

  A flag specifying whether to include the original primary parameters
  in the new MCMC object.

- silent:

  A flag specifying whether to suppress messages and warnings.

- parallel:

  A flag specifying whether to generate the derived parameters for each
  chain in parallel.

## Value

An MCMC object with the derived parameter(s).

## Details

It's important to note that parameters in the expression that also occur
in the original object are not included in the new object unless
`primary = TRUE` in which case they are simply copied from the original
object to the new one. This applies even when the primary parameters are
redefined in values.

## Methods (by class)

- `mcmc_derive(nlist)`: Get derived parameters for an
  [`nlist::nlist-object()`](https://rdrr.io/pkg/nlist/man/nlist.html)

- `mcmc_derive(nlists)`: Get derived parameters for an
  [`nlist::nlists-object()`](https://rdrr.io/pkg/nlist/man/nlists.html)

- `mcmc_derive(mcmc)`: Get derived parameters for an
  [`coda::mcmc()`](https://rdrr.io/pkg/coda/man/mcmc.html) object

- `mcmc_derive(mcmc.list)`: Get derived parameters for an
  [`coda::mcmc.list()`](https://rdrr.io/pkg/coda/man/mcmc.list.html)
  object

- `mcmc_derive(mcmcr)`: Get derived parameters for an
  [`mcmcr::mcmcr-object()`](https://poissonconsulting.github.io/mcmcr/reference/mcmcr-object.html)

- `mcmc_derive(mcmcrs)`: Get derived parameters for an
  [`mcmcr::mcmcrs-object()`](https://poissonconsulting.github.io/mcmcr/reference/mcmcrs-object.html)

## Examples

``` r
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
#> 

expr <- "
 log(alpha2) <- alpha
 gamma <- sum(alpha) * sigma"

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
#> 
```
