# Diagnose a design

Simulates the design and computes diagnosands (bias, RMSE, power,
coverage, and so on) across simulations. If `bootstrap_sims > 0`,
bootstrap standard errors for each diagnosand are also reported.

## Usage

``` r
diagnose_design(
  ...,
  sims = NULL,
  bootstrap_sims = 100,
  diagnosands = NULL,
  progress = FALSE
)

diagnose_designs(
  ...,
  sims = NULL,
  bootstrap_sims = 100,
  diagnosands = NULL,
  progress = FALSE
)
```

## Arguments

- ...:

  (required) One or more `design` objects, or lists of them. A
  simulations data frame from
  [`simulate_design()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md)
  in first position is diagnosed directly, as
  [`diagnose_simulations()`](https://declaredesign.org/r/declaredesign/reference/diagnose_simulations.md)
  does.

- sims:

  (optional) Number of simulations. Defaults to `NULL`. When `NULL`,
  designs with step-level `draws` run in nested mode; otherwise the
  design runs `500` flat simulations. When supplied alongside step-level
  `draws`, the `draws` are ignored and a warning is emitted.

- bootstrap_sims:

  (optional) Number of bootstrap replicates for diagnosand standard
  errors. Defaults to `100`; `0` skips the bootstrap.

- diagnosands:

  (optional) A diagnosands `design_step` (e.g., from
  [`declare_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/declare_diagnosands.md)).
  `NULL` (the default) uses diagnosands attached with
  [`set_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/set_diagnosands.md),
  else
  [`default_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/default_diagnosands.md).

- progress:

  (optional) If `TRUE`, display a progress bar for this call by wrapping
  it in
  [`progressr::with_progress()`](https://progressr.futureverse.org/reference/with_progress.html).
  Defaults to `FALSE`. See
  [`simulate_design()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md).

## Value

A `diagnosis` object: a list whose main elements are `simulations_df`
(the simulations, as
[`simulate_design()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md)
returns them), `diagnosands_df` (a tibble with one row per group of
design, redesign parameters, inquiry, estimator, outcome, and term,
holding `n_sims`, a column per diagnosand, and, when bootstrapped, an
`se(<diagnosand>)` column for each), `diagnosand_names`,
`bootstrap_sims`, and `bootstrap_replicates`. When the simulation was
nested, `variance_decomposition` holds a tibble decomposing the variance
of each quantity by draw level; otherwise it is `NULL`.

## See also

Other simulation and diagnosis:
[`compare_diagnoses()`](https://declaredesign.org/r/declaredesign/reference/compare_diagnoses.md),
[`diagnose_simulations()`](https://declaredesign.org/r/declaredesign/reference/diagnose_simulations.md),
[`get_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/get_diagnosands.md),
[`get_simulations()`](https://declaredesign.org/r/declaredesign/reference/get_simulations.md),
[`simulate_design()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md)

## Examples

``` r
design <- declare_model(N = 40, U = rnorm(N), Z = rep(0:1, 20), Y = U + Z) +
  declare_inquiry(ATE = 1) +
  declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                    label = "ols")
diagnose_design(design, sims = 5, bootstrap_sims = 0)
#> Research design diagnosis
#>   5 simulations
#> 
#>  Design Inquiry Estimator Term N Sims Mean Estimand Mean Estimate  Bias
#>  design     ATE       ols    Z      5          1.00          0.89 -0.11
#>  SD Estimate RMSE Power Coverage
#>         0.37 0.35  0.60     1.00
```
