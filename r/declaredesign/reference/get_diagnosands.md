# Get the diagnosands table from a diagnosis

Get the diagnosands table from a diagnosis

## Usage

``` r
get_diagnosands(diagnosis)
```

## Arguments

- diagnosis:

  (required) A `diagnosis` object.

## Value

The diagnosis's `diagnosands_df`: a tibble with one row per group
(design, inquiry, estimator, outcome, and term), holding `n_sims`, a
column per diagnosand, and, when bootstrapped, an `se(<diagnosand>)`
column for each.

## See also

Other simulation and diagnosis:
[`compare_diagnoses()`](https://declaredesign.org/r/declaredesign/reference/compare_diagnoses.md),
[`diagnose_design()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md),
[`diagnose_simulations()`](https://declaredesign.org/r/declaredesign/reference/diagnose_simulations.md),
[`get_simulations()`](https://declaredesign.org/r/declaredesign/reference/get_simulations.md),
[`simulate_design()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md)

## Examples

``` r
design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
  declare_inquiry(ATE = 0) +
  declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                    label = "ols")
d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
get_diagnosands(d)
#> # A tibble: 1 × 12
#>   design inquiry estimator term  n_sims mean_estimand mean_estimate    bias
#>   <chr>  <chr>   <chr>     <chr>  <int>         <dbl>         <dbl>   <dbl>
#> 1 design ATE     ols       Z          5             0       -0.0309 -0.0309
#> # ℹ 4 more variables: sd_estimate <dbl>, rmse <dbl>, power <dbl>,
#> #   coverage <dbl>
```
