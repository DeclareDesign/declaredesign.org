# Split data into quantile buckets (e.g. terciles, quartiles, quantiles, deciles).

Survey data is often presented in aggregated, depersonalized form, which
can involve binning underlying data into quantile buckets; for example,
rather than reporting underlying income, a survey might report income by
decile. `split_quantile` can automatically produce this split using any
data `x` and any number of splits \`type.

## Usage

``` r
split_quantile(x = NULL, type = NULL)
```

## Arguments

- x:

  A vector of any type that can be ordered – i.e. numeric or factor
  where factor levels are ordered.

- type:

  The number of buckets to split data into. For a median split, enter 2;
  for terciles, enter 3; for quartiles, enter 4; for quintiles, 5; for
  deciles, 10.

## Examples

``` r
# Divide this arbitrary data set in 3.
data_input <- rnorm(n = 100)
split_quantile(x = data_input, type = 3)
#>   [1] 2 3 1 3 2 1 2 3 1 1 3 3 3 1 2 1 1 2 2 1 3 2 3 2 1 3 3 1 2 1 3 3 3 3 1 1 2
#>  [38] 2 2 2 2 1 2 1 3 1 3 1 1 3 3 2 2 3 1 1 3 1 1 2 1 2 3 1 3 2 3 2 1 2 1 1 2 1
#>  [75] 2 3 1 3 2 3 1 3 2 1 3 2 3 2 2 2 2 3 2 2 1 3 3 3 1 1
#> Levels: 1 2 3
```
