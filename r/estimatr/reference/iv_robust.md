# Two-Stage Least Squares Instrumental Variables Regression

This formula estimates an instrumental variables regression using
two-stage least squares with a variety of options for robust standard
errors

## Usage

``` r
iv_robust(
  formula,
  data,
  weights,
  subset,
  clusters,
  fixed_effects,
  se_type = NULL,
  ci = TRUE,
  alpha = 0.05,
  diagnostics = FALSE,
  return_vcov = TRUE,
  try_cholesky = FALSE
)
```

## Arguments

- formula:

  an object of class formula of the regression and the instruments. For
  example, the formula `y ~ x1 + x2 | z1 + z2` specifies `x1` and `x2`
  as endogenous regressors and `z1` and `z2` as their respective
  instruments.

- data:

  A `data.frame`

- weights:

  the bare (unquoted) names of the weights variable in the supplied
  data.

- subset:

  An optional bare (unquoted) expression specifying a subset of
  observations to be used.

- clusters:

  An optional bare (unquoted) name of the variable that corresponds to
  the clusters in the data.

- fixed_effects:

  An optional right-sided formula containing the fixed effects that will
  be projected out of the data, such as `~ blockID`. Do not pass
  multiple-fixed effects with intersecting groups. Speed gains are
  greatest for variables with large numbers of groups and when using
  "HC1" or "stata" standard errors. See 'Details'.

- se_type:

  The sort of standard error sought. If `clusters` is not specified the
  options are "HC0", "HC1" (or "stata", the equivalent), "HC2"
  (default), "HC3", or "classical". If `clusters` is specified the
  options are "CR0", "CR2" (default), or "stata". Can also specify
  "none", which may speed up estimation of the coefficients.

- ci:

  logical. Whether to compute and return p-values and confidence
  intervals, TRUE by default.

- alpha:

  The significance level, 0.05 by default.

- diagnostics:

  logical. Whether to compute and return instrumental variable
  diagnostic statistics and tests.

- return_vcov:

  logical. Whether to return the variance-covariance matrix for later
  usage, TRUE by default.

- try_cholesky:

  logical. Whether to try using a Cholesky decomposition to solve least
  squares instead of a QR decomposition, FALSE by default. Using a
  Cholesky decomposition may result in speed gains, but should only be
  used if users are sure their model is full-rank (i.e., there is no
  perfect multi-collinearity)

## Value

An object of class `"iv_robust"`.

The post-estimation commands functions `summary` and
[`tidy`](https://generics.r-lib.org/reference/tidy.html) return results
in a `data.frame`. To get useful data out of the return, you can use
these data frames, you can use the resulting list directly, or you can
use the generic accessor functions `coef`, `vcov`, `confint`, and
`predict`.

An object of class `"iv_robust"` is a list containing at least the
following components:

- coefficients:

  the estimated coefficients

- std.error:

  the estimated standard errors

- statistic:

  the t-statistic

- df:

  the estimated degrees of freedom

- p.value:

  the p-values from a two-sided t-test using `coefficients`,
  `std.error`, and `df`

- conf.low:

  the lower bound of the `1 - alpha` percent confidence interval

- conf.high:

  the upper bound of the `1 - alpha` percent confidence interval

- term:

  a character vector of coefficient names

- alpha:

  the significance level specified by the user

- se_type:

  the standard error type specified by the user

- res_var:

  the residual variance

- nobs:

  the number of observations used

- k:

  the number of columns in the design matrix (includes linearly
  dependent columns!)

- rank:

  the rank of the fitted model

- vcov:

  the fitted variance covariance matrix

- r.squared:

  the \\R^2\\ of the second stage regression

- adj.r.squared:

  the \\R^2\\ of the second stage regression, but penalized for having
  more parameters, `rank`

- fstatistic:

  a vector with the value of the second stage F-statistic with the
  numerator and denominator degrees of freedom

- firststage_fstatistic:

  a vector with the value of the first stage F-statistic with the
  numerator and denominator degrees of freedom, useful for a test for
  weak instruments

- weighted:

  whether or not weights were applied

- call:

  the original function call

- fitted.values:

  the matrix of predicted means

We also return `terms` with the second stage terms and
`terms_regressors` with the first stage terms, both of which used by
`predict`. If `fixed_effects` are specified, then we return
`proj_fstatistic`, `proj_r.squared`, and `proj_adj.r.squared`, which are
model fit statistics that are computed on the projected model (after
demeaning the fixed effects).

We also return various diagnostics when `` `diagnostics` == TRUE ``.
These are stored in `diagnostic_first_stage_fstatistic`,
`diagnostic_endogeneity_test`, and `diagnostic_overid_test`. They have
the test statistic, relevant degrees of freedom, and p.value in a named
vector. See 'Details' for more. These are printed in a formatted table
when the model object is passed to
[`summary()`](https://rdrr.io/r/base/summary.html).

## Details

This function performs two-stage least squares estimation to fit
instrumental variables regression. The syntax is similar to that in
`ivreg` from the `AER` package. Regressors and instruments should be
specified in a two-part formula, such as `y ~ x1 + x2 | z1 + z2 + z3`,
where `x1` and `x2` are regressors and `z1`, `z2`, and `z3` are
instruments. Unlike `ivreg`, you must explicitly specify all exogenous
regressors on both sides of the bar.

The default variance estimators are the same as in
[`lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.md).
Without clusters, we default to `HC2` standard errors, and with clusters
we default to `CR2` standard errors. 2SLS variance estimates are
computed using the same estimators as in
[`lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.md),
however the design matrix used are the second-stage regressors, which
includes the estimated endogenous regressors, and the residuals used are
the difference between the outcome and a fit produced by the
second-stage coefficients and the first-stage (endogenous) regressors.
More notes on this can be found at [the mathematical
appendix](https://declaredesign.org/r/estimatr/articles/mathematical-notes.html).

If `fixed_effects` are specified, both the outcome, regressors, and
instruments are centered using the method of alternating projections
(Halperin 1962; Gaure 2013). Specifying fixed effects in this way will
result in large speed gains with standard error estimators that do not
need to invert the matrix of fixed effects. This means using
"classical", "HC0", "HC1", "CR0", or "stata" standard errors will be
faster than other standard error estimators. Be wary when specifying
fixed effects that may result in perfect fits for some observations or
if there are intersecting groups across multiple fixed effect variables
(e.g. if you specify both "year" and "country" fixed effects with an
unbalanced panel where one year you only have data for one country).

If `diagnostics` are requested, we compute and return three sets of
diagnostics. First, we return tests for weak instruments using
first-stage F-statistics (`diagnostic_first_stage_fstatistic`).
Specifically, the F-statistics reported compare the model regressing
each endogeneous variable on both the included exogenous variables and
the instruments to a model where each endogenous variable is regressed
only on the included exogenous variables (without the instruments). A
significant F-test for weak instruments provides evidence against the
null hypothesis that the instruments are weak. Second, we return tests
for the endogeneity of the endogenous variables, often called the
Wu-Hausman test (`diagnostic_endogeneity_test`). We implement the
regression test from Hausman (1978), which allows for robust variance
estimation. A significant endogeneity test provides evidence against the
null that all the variables are exogenous. Third, we return a test for
the correlation between the instruments and the error term
(`diagnostic_overid_test`). We implement the Wooldridge (1995) robust
score test, which is identical to Sargan's (1958) test with classical
standard errors. This test is only reported if the model is
overidentified (i.e. the number of instruments is greater than the
number of endogenous regressors), and if no weights are specified.

## References

Gaure, Simon. 2013. "OLS with multiple high dimensional category
variables." Computational Statistics & Data Analysis 66: 8-1.
[doi:10.1016/j.csda.2013.03.024](https://doi.org/10.1016/j.csda.2013.03.024)

Halperin, I. 1962. "The product of projection operators." Acta
Scientiarum Mathematicarum (Szeged) 23(1-2): 96-99.

## Examples

``` r
library(fabricatr)
dat <- fabricate(
  N = 40,
  Y = rpois(N, lambda = 4),
  Z = rbinom(N, 1, prob = 0.4),
  D  = Z * rbinom(N, 1, prob = 0.8),
  X = rnorm(N),
  G = sample(letters[1:4], N, replace = TRUE)
)

# Instrument for treatment `D` with encouragement `Z`
tidy(iv_robust(Y ~ D + X | Z + X, data = dat))
#>          term   estimate std.error statistic      p.value   conf.low conf.high
#> 1 (Intercept) 3.11050295 0.3897573 7.9806146 1.460744e-09  2.3207796 3.9002263
#> 2           D 0.56185078 0.6718722 0.8362465 4.083846e-01 -0.7994916 1.9231932
#> 3           X 0.08507783 0.3253453 0.2615001 7.951571e-01 -0.5741343 0.7442899
#>   df outcome
#> 1 37       Y
#> 2 37       Y
#> 3 37       Y

# Instrument with Stata's `ivregress 2sls , small rob` HC1 variance
tidy(iv_robust(Y ~ D | Z, data = dat, se_type = "stata"))
#>          term  estimate std.error statistic      p.value   conf.low conf.high
#> 1 (Intercept) 3.1052632 0.3732938 8.3185511 4.372968e-10  2.3495695  3.860957
#> 2           D 0.5394737 0.6707058 0.8043373 4.262058e-01 -0.8182992  1.897247
#>   df outcome
#> 1 38       Y
#> 2 38       Y

# With clusters, we use CR2 errors by default
dat$cl <- rep(letters[1:5], length.out = nrow(dat))
tidy(iv_robust(Y ~ D | Z, data = dat, clusters = cl))
#>          term  estimate std.error statistic     p.value  conf.low conf.high
#> 1 (Intercept) 3.1052632 0.5127115 6.0565501 0.004813205  1.634830  4.575696
#> 2           D 0.5394737 0.6397718 0.8432283 0.446613594 -1.237456  2.316404
#>         df outcome
#> 1 3.698715       Y
#> 2 3.996357       Y

# Again, easy to replicate Stata (again with `small` correction in Stata)
tidy(iv_robust(Y ~ D | Z, data = dat, clusters = cl, se_type = "stata"))
#>          term  estimate std.error statistic     p.value  conf.low conf.high df
#> 1 (Intercept) 3.1052632 0.5128499 6.0549161 0.003754932  1.681364  4.529163  4
#> 2           D 0.5394737 0.6422307 0.8399998 0.448183917 -1.243645  2.322592  4
#>   outcome
#> 1       Y
#> 2       Y

# We can also specify fixed effects, that will be taken as exogenous regressors
# Speed gains with fixed effects are greatests with "stata" or "HC1" std.errors
tidy(iv_robust(Y ~ D | Z, data = dat, fixed_effects = ~ G, se_type = "HC1"))
#>   term  estimate std.error statistic   p.value  conf.low conf.high df outcome
#> 1    D 0.3230081  0.673923 0.4792952 0.6347099 -1.045128  1.691144 35       Y
```
