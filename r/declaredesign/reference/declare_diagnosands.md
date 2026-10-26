# Declare diagnosands

Defines the summary statistics computed across simulations during
diagnosis. Each named expression is evaluated against the simulations
data frame, grouped by `inquiry`, `estimator`, `term`, and `outcome`
where present.

## Usage

``` r
declare_diagnosands(..., subset = NULL, alpha = 0.05, label = "diagnosands")
```

## Arguments

- ...:

  (optional) Named expressions defining diagnosands, each evaluated on
  the simulations table of one group and returning a single value.

- subset:

  (optional) An expression evaluated on the simulations table; only rows
  for which it is `TRUE` enter the diagnosands. `NULL` (the default)
  keeps every simulation.

- alpha:

  (optional) Significance level. Any diagnosand expression that mentions
  `alpha` sees this value. Defaults to `0.05`.

- label:

  (optional) Step label. Defaults to `"diagnosands"`.

## Value

A diagnosands `design_step`: a function that takes a simulations table
and returns a tibble with columns `diagnosand` and `value`, one row per
diagnosand. Its `dots` attribute carries the diagnosand quosures. Pass
it to
[`diagnose_design()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md)
as `diagnosands`.

## See also

Other diagnosands:
[`default_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/default_diagnosands.md),
[`format.diagnosis()`](https://declaredesign.org/r/declaredesign/reference/format.diagnosis.md),
[`select_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/select_diagnosands.md),
[`set_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/set_diagnosands.md)

## Examples

``` r
diags <- declare_diagnosands(
  bias = mean(estimate - estimand, na.rm = TRUE),
  rmse = sqrt(mean((estimate - estimand)^2, na.rm = TRUE))
)

design <-
  declare_model(N = 30, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = sample(rep(0:1, length.out = N))) +
  declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z) +
  declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE")

diagnose_design(design, sims = 5, bootstrap_sims = 0, diagnosands = diags)
#> Research design diagnosis
#>   5 simulations
#> 
#>  Design Inquiry Estimator Term N Sims  Bias RMSE
#>  design     ATE estimator    Z      5 -0.09 0.35

# power at the 10 percent level, computed on significant simulations only
declare_diagnosands(power = mean(p.value <= alpha), alpha = 0.1)
#> <design_step: diagnosands [diagnosand]>
```
