# Tidy a diagnosis

Returns the diagnosands in long form, one row per group per diagnosand,
with the bootstrap standard error and percentile confidence interval
alongside. This is the shape to plot from:
`ggplot(aes(estimate, diagnosand)) + geom_errorbarh(aes(xmin = conf.low, xmax = conf.high))`.

## Usage

``` r
# S3 method for class 'diagnosis'
tidy(x, conf.int = TRUE, conf.level = 0.95, ...)
```

## Arguments

- x:

  (required) A `diagnosis`.

- conf.int:

  (optional) Whether to include the confidence interval. Defaults to
  `TRUE`.

- conf.level:

  (optional) Confidence level for the interval. Defaults to `0.95`.

- ...:

  (optional) Ignored.

## Value

A tibble with one row per group per diagnosand: the grouping columns
(`design`, `inquiry`, `estimator`, `outcome`, `term`, and any redesign
parameters), `diagnosand`, `estimate`, and, when bootstrapped,
`std.error`, `conf.low`, and `conf.high`.

## Details

`std.error`, `conf.low`, and `conf.high` come from the bootstrap
replicates, so they appear only when the diagnosis was run with
`bootstrap_sims > 0`.

## Examples

``` r
design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
  declare_inquiry(ATE = 0) +
  declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                    label = "ols")
d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
tidy(d)
#> # A tibble: 7 × 6
#>   design inquiry estimator term  diagnosand    estimate
#>   <chr>  <chr>   <chr>     <chr> <chr>            <dbl>
#> 1 design ATE     ols       Z     mean_estimand   0     
#> 2 design ATE     ols       Z     mean_estimate   0.0788
#> 3 design ATE     ols       Z     bias            0.0788
#> 4 design ATE     ols       Z     sd_estimate     0.581 
#> 5 design ATE     ols       Z     rmse            0.526 
#> 6 design ATE     ols       Z     power           0.2   
#> 7 design ATE     ols       Z     coverage        0.8   
```
