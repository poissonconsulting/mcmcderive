# Changelog

## mcmcderive 0.1.2.9005

- Standardize CI via reusable workflows (tier: important)
- Guard tests against missing Suggests dependency for check-no-suggests
  CI

## mcmcderive 0.1.2.9004

- Add fledge-bump workflow
- Add fledge-tag-on-merge workflow

## mcmcderive 0.1.2.9003

- Internal changes.

## mcmcderive 0.1.2.9002

### Bug fixes

- Flatten double braces more aggressively
  ([\#21](https://github.com/poissonconsulting/mcmcderive/issues/21),
  [\#22](https://github.com/poissonconsulting/mcmcderive/issues/22)).

## mcmcderive 0.1.2.9001

- Add
  [`expression_vectorize()`](https://poissonconsulting.github.io/mcmcderive/dev/reference/expression_vectorize.md).

## mcmcderive 0.1.2.9000

- Same as previous version.

## mcmcderive 0.1.2

CRAN release: 2021-08-06

- Removed constraint that derived parameters do not have any missing
  values.

## mcmcderive 0.1.0

CRAN release: 2020-07-16

- Added
  [`mcmc_derive()`](https://poissonconsulting.github.io/mcmcderive/dev/reference/mcmc_derive.md)
  for nlist, nlists and mcmc objects
- Added `primary = FALSE` argument to
  [`mcmc_derive()`](https://poissonconsulting.github.io/mcmcderive/dev/reference/mcmc_derive.md)
  to include original parameters as is.
- Adopted Tidyverse style for error messages.
- Moved extra functions to `extras` package.
- Replaced `checkr` and `err` dependencies for `chk`.

## mcmcderive 0.0.1

CRAN release: 2019-07-02

- Optimized
  [`mcmc_derive()`](https://poissonconsulting.github.io/mcmcderive/dev/reference/mcmc_derive.md).
