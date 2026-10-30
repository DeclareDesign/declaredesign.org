# Internal method that creates linear fits

Internal method that creates linear fits

## Usage

``` r
lm_robust_fit(
  y,
  X,
  weights,
  cluster,
  ci = TRUE,
  se_type,
  has_int,
  alpha = 0.05,
  return_vcov = TRUE,
  return_fit = TRUE,
  try_cholesky = FALSE,
  iv_stage = list(0),
  fe_rank = 0L,
  fe_leverage = NULL,
  femat = NULL,
  linear_hypothesis = NULL
)
```

## Arguments

- y:

  numeric outcome vector or matrix

- X:

  numeric design matrix

- weights:

  numeric weights vector

- cluster:

  numeric cluster vector

- ci:

  boolean, whether to return confidence intervals and p-values

- se_type:

  character denoting which kind of SEs to return

- has_int:

  logical, whether the model has an intercept

- alpha:

  numeric, test size for confidence intervals

- return_vcov:

  logical, whether to return the vcov matrix

- return_fit:

  logical, whether to return fitted values

- try_cholesky:

  logical. Solve by Cholesky decomposition of `X'X` rather than by the
  pivoted QR, falling back to the QR where the design is rank deficient.
  See
  [`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
  for when the fast path is safe.

- iv_stage:

  list of length one or two for 2SLS stages

- fe_rank:

  integer, degrees of freedom absorbed by fixed effects

- fe_leverage:

  numeric vector of per-observation leverage contributed by the absorbed
  fixed effects, or `NULL`. `h_ii` of the full design splits exactly
  into the demeaned-X leverage plus this term, for any number of FE
  factors, which is what makes HC2 and HC3 available under
  `fixed_effects` without building the dummy matrix.

- femat:

  optional numeric matrix of fixed-effect dummies for the estimation
  sample. Only `CR2` needs it: its adjustment is built from
  cluster-level blocks of the hat matrix, which do not decompose the way
  the diagonal does. HC2 and HC3 take `fe_leverage` instead. `NULL`
  unless the requested `se_type` requires it.

- linear_hypothesis:

  optional hypotheses, in the form
  [`lh_robust()`](https://declaredesign.org/r/estimatr/reference/lh_robust.md)
  takes, whose `CR2` Satterthwaite degrees of freedom are returned as
  `hypothesis_df`. Ignored for every other `se_type`, where a
  combination of coefficients has the same degrees of freedom as each of
  them.

## Examples

``` r
# The fitter behind lm_robust(), exported for packages that have already
# built their own design matrix. Most users want lm_robust().
set.seed(45)
X <- cbind(`(Intercept)` = 1, x = rnorm(50))
y <- X[, "x"] + rnorm(50)

lm_robust_fit(
  y = y, X = X,
  weights = NULL, cluster = NULL,
  se_type = "HC2", has_int = TRUE
)
#>               Estimate Std. Error   t value     Pr(>|t|)   CI Lower CI Upper DF
#> (Intercept) 0.02772598  0.1691670 0.1638971 8.705003e-01 -0.3124071 0.367859 48
#> x           1.14688528  0.1601907 7.1595005 4.189198e-09  0.8248003 1.468970 48
```
