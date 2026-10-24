# Draw quantile bucket assignments

Draws `N` new bucket assignments, `type` equally sized buckets in random
order, by cutting `N` uniform draws with
[`split_quantile`](https://declaredesign.org/r/fabricatr/reference/split_quantile.md).
To bucket a variable you already have, use `split_quantile` on it
directly.

## Usage

``` r
draw_quantile(type, N)
```

## Arguments

- type:

  Number of buckets.

- N:

  Number of observations.

## Value

A factor of length `N` with levels `1` through `type`, unordered for the
reason given under
[`split_quantile`](https://declaredesign.org/r/fabricatr/reference/split_quantile.md).

## Examples

``` r
draw_quantile(type = 4, N = 100)
#>   [1] 4 2 1 4 2 2 3 3 2 4 3 3 2 3 1 1 3 2 3 3 3 4 1 1 3 4 1 2 3 4 3 4 1 1 3 2 4
#>  [38] 1 2 2 3 2 4 2 1 1 4 3 1 4 4 2 4 1 3 2 4 3 2 1 3 2 1 2 1 1 4 4 4 2 4 2 4 1
#>  [75] 1 1 1 4 2 4 3 2 1 2 4 3 2 2 4 2 1 3 3 3 1 3 4 1 4 3
#> Levels: 1 2 3 4
```
