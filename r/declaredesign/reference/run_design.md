# Execute a design end-to-end

Runs the design once and returns one row per estimate, with the realized
estimands joined on. This is one simulation: `simulate_design(sims = 1)`
without the `sim_ID` column.

## Usage

``` r
run_design(design, ...)
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

The result of one run: a tibble with one row per estimate: the tidied
columns (`term`, `estimate`, `std.error`, `p.value`, `conf.low`,
`conf.high`, and so on), `estimator`, and, where an estimator names an
inquiry, `inquiry` and the realized `estimand`. A design with no
estimator returns its estimands instead, as
[`draw_estimands()`](https://declaredesign.org/r/declaredesign/reference/draw_estimands.md)
does.

## Details

Use
[`draw_data()`](https://declaredesign.org/r/declaredesign/reference/draw_data.md)
for the realized data frame,
[`draw_estimands()`](https://declaredesign.org/r/declaredesign/reference/draw_estimands.md)
for the inquiries alone, and
[`draw_estimates()`](https://declaredesign.org/r/declaredesign/reference/draw_estimates.md)
for the estimates alone.

## See also

Other drawing from a design:
[`draw_data()`](https://declaredesign.org/r/declaredesign/reference/draw_data.md),
[`draw_estimands()`](https://declaredesign.org/r/declaredesign/reference/draw_estimands.md),
[`draw_estimates()`](https://declaredesign.org/r/declaredesign/reference/draw_estimates.md),
[`get_estimates()`](https://declaredesign.org/r/declaredesign/reference/get_estimates.md)

## Examples

``` r
design <- declare_model(N = 30, U = rnorm(N), Y = U) +
  declare_inquiry(mu = mean(Y))
run_design(design)
#> # A tibble: 1 × 2
#>   inquiry estimand
#>   <chr>      <dbl>
#> 1 mu         0.309
```
