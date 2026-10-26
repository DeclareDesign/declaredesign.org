# Default diagnosands

Returns the standard set of diagnosands. A diagnosand is a summary of a
design's behaviour across simulations, so each one is computed over the
whole set of draws rather than within any single draw.

## Usage

``` r
default_diagnosands()
```

## Value

A diagnosands `design_step`, as
[`declare_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/declare_diagnosands.md)
returns, holding the seven diagnosands listed under Details.

## Details

- `mean_estimand`:

  Average of the estimand across draws. The estimand varies from draw to
  draw whenever the model is stochastic.

- `mean_estimate`:

  Average of the estimator's estimate across draws.

- `bias`:

  Average of estimate minus estimand. Zero means the estimator lands on
  the estimand on average, not that it is close on any one draw.

- `sd_estimate`:

  Standard deviation of the estimates, which is the true sampling
  variability of the design.

- `rmse`:

  Root mean squared error, the average distance between estimate and
  estimand. It counts bias and variability together, so it is the one to
  read when an estimator trades one against the other.

- `power`:

  Share of draws in which the p-value is at or below 0.05. Read it as
  power only when the effect is real; under a true null the same number
  is the false positive rate.

- `coverage`:

  Share of draws in which the confidence interval contains the estimand.
  A 95 percent interval should give 0.95. Materially lower means the
  intervals are too narrow and the design overstates its own precision.

## See also

Other diagnosands:
[`declare_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/declare_diagnosands.md),
[`format.diagnosis()`](https://declaredesign.org/r/declaredesign/reference/format.diagnosis.md),
[`select_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/select_diagnosands.md),
[`set_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/set_diagnosands.md)

## Examples

``` r
default_diagnosands()
#> <design_step: diagnosands [diagnosand]>
```
