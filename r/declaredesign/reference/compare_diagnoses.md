# Compare the diagnosands of two designs

Diagnoses both designs and reports, for every diagnosand they have in
common, the value under each design and the difference between them. The
bootstrap replicates behind each diagnosis supply a standard error and a
confidence interval for the difference.

## Usage

``` r
compare_diagnoses(
  design1,
  design2,
  sims = 500,
  bootstrap_sims = 100,
  diagnosands = NULL,
  merge_by_estimator = TRUE,
  alpha = 0.05
)
```

## Arguments

- design1, design2:

  (required) A `design` or a `diagnosis`.

- sims:

  (optional) Number of simulations, used only for arguments that are
  designs. Defaults to `500`.

- bootstrap_sims:

  (optional) Number of bootstrap replicates, used only for arguments
  that are designs. Defaults to `100`. With `0`, differences are
  reported without standard errors or intervals.

- diagnosands:

  (optional) A diagnosands `design_step` applied to both designs, so the
  two sides are compared on the same footing by construction. `NULL`
  (the default) uses what
  [`diagnose_design()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md)
  would: diagnosands attached with
  [`set_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/set_diagnosands.md),
  else
  [`default_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/default_diagnosands.md).
  Used only for arguments that are designs; a diagnosis passed in keeps
  the diagnosands it was computed with.

- merge_by_estimator:

  (optional) Match estimators by label. `TRUE` by default; `FALSE`
  compares every pair of estimators within an inquiry.

- alpha:

  (optional) One minus the coverage of the reported interval. Defaults
  to `0.05`.

## Value

A `compared_diagnoses` object: a list with three elements.
`compared_diagnoses_df` is a tibble with one row per diagnosand per
matched group, holding the grouping columns, `diagnosand`, `mean_1`,
`mean_2`, `mean_difference`, `se_1`, `se_2`, `se_difference`,
`conf.low`, and `conf.high` (the last four are `NA` without bootstrap
replicates). `diagnosis1` and `diagnosis2` are the two `diagnosis`
objects compared.

## Details

Rows are matched on whichever of `inquiry`, `estimator`, and `term` both
diagnoses carry. With `merge_by_estimator = FALSE`, estimators are not
matched to each other and every pair within an inquiry is compared,
which is how to compare two estimators that carry different labels.

`mean_difference` is the difference of the two point estimates. The
bootstrap enters only through `se_difference` and the interval. The two
designs are simulated independently, so the replicates are paired
arbitrarily; the interval is a percentile interval on that difference.

Either argument may be a design or an already computed `diagnosis`, so a
diagnosis that took a long time to run can be reused. Passing two
diagnoses computed with different diagnosands compares whatever they
have in common.

## See also

Other simulation and diagnosis:
[`diagnose_design()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md),
[`diagnose_simulations()`](https://declaredesign.org/r/declaredesign/reference/diagnose_simulations.md),
[`get_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/get_diagnosands.md),
[`get_simulations()`](https://declaredesign.org/r/declaredesign/reference/get_simulations.md),
[`simulate_design()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md)

## Examples

``` r
design <- declare_parameters(n_units = 100) +
  declare_model(N = n_units, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = randomizr::complete_ra(N)) +
  declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
  declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                    label = "ols")
bigger <- redesign(design, n_units = 200)
compare_diagnoses(design, bigger, sims = 20, bootstrap_sims = 20)
#> Comparison of diagnosands, design 2 minus design 1
#> 
#> # A tibble: 7 × 13
#>   inquiry estimator term  n_units_2 diagnosand    mean_1  mean_2 mean_difference
#>   <chr>   <chr>     <chr>     <dbl> <chr>          <dbl>   <dbl>           <dbl>
#> 1 ATE     ols       Z           200 mean_estima…  0.5     0.5            0      
#> 2 ATE     ols       Z           200 mean_estima…  0.469   0.470          0.00155
#> 3 ATE     ols       Z           200 bias         -0.0311 -0.0296         0.00155
#> 4 ATE     ols       Z           200 sd_estimate   0.204   0.131         -0.0731 
#> 5 ATE     ols       Z           200 rmse          0.201   0.131         -0.0703 
#> 6 ATE     ols       Z           200 power         0.75    0.9            0.15   
#> 7 ATE     ols       Z           200 coverage      0.95    0.9           -0.0500 
#> # ℹ 5 more variables: se_1 <dbl>, se_2 <dbl>, se_difference <dbl>,
#> #   conf.low <dbl>, conf.high <dbl>
```
