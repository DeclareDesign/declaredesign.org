# Diagnose a precomputed simulations table

Diagnose a precomputed simulations table

## Usage

``` r
diagnose_simulations(simulations_df, diagnosands = NULL, bootstrap_sims = 100)
```

## Arguments

- simulations_df:

  (required) A tibble produced by
  [`simulate_design()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md).
  Grouping set with
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  adds to the groups diagnosed.

- diagnosands:

  (optional) A diagnosands `design_step` (e.g., from
  [`declare_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/declare_diagnosands.md)).
  `NULL` (the default) uses
  [`default_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/default_diagnosands.md).

- bootstrap_sims:

  (optional) Number of bootstrap replicates. Defaults to `100`; `0`
  skips the bootstrap.

## Value

A `diagnosis` object, as
[`diagnose_design()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md)
returns.

## See also

Other simulation and diagnosis:
[`compare_diagnoses()`](https://declaredesign.org/r/declaredesign/reference/compare_diagnoses.md),
[`diagnose_design()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md),
[`get_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/get_diagnosands.md),
[`get_simulations()`](https://declaredesign.org/r/declaredesign/reference/get_simulations.md),
[`simulate_design()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md)

## Examples

``` r
design <- declare_model(N = 40, U = rnorm(N), Z = rep(0:1, 20), Y = U + Z) +
  declare_inquiry(ATE = 1) +
  declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                    label = "ols")
sims <- simulate_design(design, sims = 5)
diagnose_simulations(sims, bootstrap_sims = 0)
#> Research design diagnosis
#>   5 simulations
#> 
#>  Design Inquiry Estimator Term N Sims Mean Estimand Mean Estimate Bias
#>  design     ATE       ols    Z      5          1.00          1.07 0.07
#>  SD Estimate RMSE Power Coverage
#>         0.32 0.29  1.00     1.00
```
