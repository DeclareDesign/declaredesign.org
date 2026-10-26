# Population variance

Computes the (uncorrected) variance: the mean squared deviation from the
mean. Useful for design-based standard errors. Missing values are
dropped.

## Usage

``` r
pop.var(x)
```

## Arguments

- x:

  (required) A numeric vector.

## Value

A numeric scalar: the mean of `(x - mean(x))^2` over the non-missing
values of `x`.

## Examples

``` r
pop.var(c(1, 2, 3, 4, 5))
#> [1] 2
```
