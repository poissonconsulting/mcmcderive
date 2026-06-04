# Convert New Expression

Takes an expression and removes the for loop and adds `cbind` for
arrays.

## Usage

``` r
expression_vectorize(x)
```

## Arguments

- x:

  An expression

## Value

An expression

## Examples

``` r
expression_vectorize(rlang::expr(for (i in 1:nObs) {
  eCount[i] <- b0
}))
#> eCount <- b0
expression_vectorize(
  rlang::expr(
    for (i in 1:length(LogLength)) {
      eWeightLength[i] <- b0 + bDayte * Dayte[i]
    }
  )
)
#> eWeightLength <- b0 + bDayte * Dayte
expression_vectorize(
  rlang::expr(
    for (i in 1:nObs) {
      eAnnual[i] <- bAnn[Ann[i]] + bSA[Site[i], Ann[i]]
    }
  )
)
#> eAnnual <- bAnn[Ann] + bSA[cbind(Site, Ann)]
```
