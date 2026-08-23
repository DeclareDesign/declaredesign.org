# Compare Diagnoses

Diagnose and compare designs.

## Usage

``` r
compare_diagnoses(
  design1,
  design2,
  sims = 500,
  bootstrap_sims = 100,
  merge_by_estimator = TRUE,
  alpha = 0.05
)
```

## Arguments

- design1:

  A design or a diagnosis.

- design2:

  A design or a diagnosis.

- sims:

  The number of simulations, defaulting to 1000. `sims` may also be a
  vector indicating the number of simulations for each step in a design,
  as described for
  [`simulate_design`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md).
  Used for both designs.

- bootstrap_sims:

  Number of bootstrap replicates for the diagnosands to obtain the
  standard errors of the diagnosands, defaulting to `1000`. Set to
  `FALSE` to turn off bootstrapping. Used for both designs. Must be
  greater or equal to 100.

- merge_by_estimator:

  A logical. Whether to include `estimator` in the set of columns used
  for merging. Defaults to `TRUE`.

- alpha:

  The significance level, 0.05 by default.

## Value

A list with a `data.frame` of compared diagnoses and both diagnoses.

## Details

The function `compare_diagnoses` runs a many-to-many merge matching by
`inquiry` and `term` (if present). If `merge_by_estimator` equals
`TRUE`, `estimator` is also included in the merging condition. Any
diagnosand that is not included in both designs will be dropped from the
merge.

## Examples

``` r
design_a <- 
  declare_model(N  = 100, 
                U = rnorm(N),
                Y_Z_0 = U,
                Y_Z_1 = U + rnorm(N, mean = 2, sd = 2)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N, prob = 0.5)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")

design_b <- replace_step(
  design_a, step = "assignment", 
  declare_assignment(Z = complete_ra(N, prob = 0.3)) )

comparison <- compare_diagnoses(design_a, design_b, sims = 40)
```
