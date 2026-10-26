# Attach diagnosands to a design (deprecated)

Deprecated in 2.0. Stores `diagnosands` as an attribute on `design`,
which
[`diagnose_design()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md)
uses in place of the defaults when present. Pass the diagnosands to the
diagnosis instead: `diagnose_design(design, diagnosands = ...)`.

## Usage

``` r
set_diagnosands(design, diagnosands = default_diagnosands())
```

## Arguments

- design:

  (required) A `design`.

- diagnosands:

  (optional) A diagnosands `design_step`. Defaults to
  [`default_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/default_diagnosands.md).

## Value

`design`, with `diagnosands` stored in its `diagnosands` attribute.

## Details

Two reasons, beyond diagnosis not being a feature of a design. The
attribute does not survive
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md),
so anything attached this way is silently gone the first time a
parameter moves, and the defaults that replace it may not be computable
from the estimator's output. And a diagnosands object replaces the
default set rather than adding to it, so declaring one diagnosand drops
the other six without saying so.

## See also

Other diagnosands:
[`declare_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/declare_diagnosands.md),
[`default_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/default_diagnosands.md),
[`format.diagnosis()`](https://declaredesign.org/r/declaredesign/reference/format.diagnosis.md),
[`select_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/select_diagnosands.md)

## Examples

``` r
design <-
  declare_model(N = 30, Y = rnorm(N)) +
  declare_inquiry(mu = mean(Y)) +
  declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)", inquiry = "mu")

# instead of set_diagnosands(design, ...)
diagnose_design(
  design,
  diagnosands = declare_diagnosands(mean_estimate = mean(estimate, na.rm = TRUE)),
  sims = 5,
  bootstrap_sims = 0
)
#> Research design diagnosis
#>   5 simulations
#> 
#>  Design Inquiry Estimator        Term N Sims Mean Estimate
#>  design      mu estimator (Intercept)      5         -0.06
```
