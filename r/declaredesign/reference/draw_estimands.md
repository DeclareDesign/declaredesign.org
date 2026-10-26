# Draw the realized estimands

Runs the data-generating and inquiry steps only, not the estimators.

## Usage

``` r
draw_estimands(design, ...)

draw_estimand(design, ...)
```

## Arguments

- design:

  (required) A `design`.

- ...:

  (optional) Named parameter values, as in
  [`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md).
  Each supplies one value: use
  [`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
  itself to sweep over several. A parameter named `design`, or any
  prefix of it, cannot be supplied this way and needs the
  [`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
  route.

## Value

A tibble with columns `inquiry` and `estimand`, one row per inquiry.

## See also

Other drawing from a design:
[`draw_data()`](https://declaredesign.org/r/declaredesign/reference/draw_data.md),
[`draw_estimates()`](https://declaredesign.org/r/declaredesign/reference/draw_estimates.md),
[`get_estimates()`](https://declaredesign.org/r/declaredesign/reference/get_estimates.md),
[`run_design()`](https://declaredesign.org/r/declaredesign/reference/run_design.md)

## Examples

``` r
design <- declare_model(N = 25, U = rnorm(N), Y = U) +
  declare_inquiry(mu = mean(Y))
draw_estimands(design)
#> # A tibble: 1 × 2
#>   inquiry estimand
#>   <chr>      <dbl>
#> 1 mu         0.452
```
