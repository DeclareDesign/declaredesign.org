# Draw the realized estimates

Runs the design once and returns its estimates joined to inquiries.

## Usage

``` r
draw_estimates(design, ...)
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

The estimates from one run: a tibble with one row per estimate: the
tidied columns (`term`, `estimate`, `std.error`, `p.value`, `conf.low`,
`conf.high`, and so on), `estimator`, and, where an estimator names an
inquiry, `inquiry` and the realized `estimand`. A design with no
estimator returns its estimands instead.

## See also

Other drawing from a design:
[`draw_data()`](https://declaredesign.org/r/declaredesign/reference/draw_data.md),
[`draw_estimands()`](https://declaredesign.org/r/declaredesign/reference/draw_estimands.md),
[`get_estimates()`](https://declaredesign.org/r/declaredesign/reference/get_estimates.md),
[`run_design()`](https://declaredesign.org/r/declaredesign/reference/run_design.md)

## Examples

``` r
design <- declare_model(N = 30, U = rnorm(N), Y = U) +
  declare_inquiry(mu = mean(Y)) +
  declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)",
                    inquiry = "mu", label = "ols")
draw_estimates(design)
#> # A tibble: 1 × 10
#>   term        estimate std.error statistic p.value conf.low conf.high estimator
#>   <chr>          <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <chr>    
#> 1 (Intercept)   0.0405     0.182     0.223   0.825   -0.331     0.412 ols      
#> # ℹ 2 more variables: inquiry <chr>, estimand <dbl>
```
