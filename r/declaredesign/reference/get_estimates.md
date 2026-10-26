# Get estimates for a fixed dataset

Runs only the estimator steps of a design against a supplied data frame.

## Usage

``` r
get_estimates(
  design,
  data = draw_data(design),
  start = 1L,
  end = length(design)
)
```

## Arguments

- design:

  (required) A `design`.

- data:

  (optional) A data frame. Defaults to `draw_data(design)`, a fresh
  draw.

- start:

  (optional) Integer; first step index to consider. Defaults to `1`.

- end:

  (optional) Integer; last step index to consider. Defaults to the last
  step.

## Value

A tibble with one row per estimate from the estimator and test steps
between `start` and `end`, as
[`draw_estimates()`](https://declaredesign.org/r/declaredesign/reference/draw_estimates.md)
returns but without estimands. An empty tibble when that range holds no
estimator.

## See also

Other drawing from a design:
[`draw_data()`](https://declaredesign.org/r/declaredesign/reference/draw_data.md),
[`draw_estimands()`](https://declaredesign.org/r/declaredesign/reference/draw_estimands.md),
[`draw_estimates()`](https://declaredesign.org/r/declaredesign/reference/draw_estimates.md),
[`run_design()`](https://declaredesign.org/r/declaredesign/reference/run_design.md)

## Examples

``` r
design <- declare_model(N = 30, U = rnorm(N), Z = rep(0:1, 15), Y = U + Z) +
  declare_estimator(Y ~ Z, .method = lm, term = "Z", label = "ols")
df <- draw_data(design)
get_estimates(design, df)
#> # A tibble: 1 × 8
#>   term  estimate std.error statistic p.value conf.low conf.high estimator
#>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <chr>    
#> 1 Z        0.466     0.354      1.32   0.199   -0.259      1.19 ols      
```
