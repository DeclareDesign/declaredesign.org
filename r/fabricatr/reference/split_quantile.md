# Split data into quantile buckets

Cuts a numeric vector you already have into `type` equally sized buckets
at its sample quantiles, so bucket `1` holds the lowest values. To draw
new bucket assignments without a variable to cut, use
[`draw_quantile`](https://declaredesign.org/r/fabricatr/reference/draw_quantile.md).

## Usage

``` r
split_quantile(x, type)
```

## Arguments

- x:

  Numeric vector.

- type:

  Number of buckets (2 = median split, 3 = terciles, etc.).

## Value

A factor with levels `1` through `type`, one per element of `x`. It is
deliberately *unordered*, as in fabricatr 1.x: an ordered factor would
make `lm` fit polynomial contrasts (`q.L`, `q.Q`, `q.C`) where the same
script under 1.x gets treatment contrasts (`q2`, `q3`, `q4`), and report
different coefficients without saying why.

## Examples

``` r
split_quantile(rnorm(100), type = 4)
#>   [1] 2 3 3 2 1 1 2 3 2 3 2 2 1 4 2 3 4 4 2 3 1 4 4 1 2 1 3 1 4 2 3 1 2 1 3 4 4
#>  [38] 4 1 4 2 1 4 1 3 1 3 3 2 1 3 4 4 1 2 3 3 1 2 3 2 1 1 1 1 1 3 4 1 2 2 2 1 4
#>  [75] 4 3 3 4 3 2 3 2 2 4 3 2 4 2 3 4 3 1 4 4 4 2 3 1 4 4
#> Levels: 1 2 3 4
```
