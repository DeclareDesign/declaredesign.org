# Two-Stage Least Squares Instrumental Variables Regression

Fits a two-stage least squares instrumental variables regression and
returns heteroskedasticity-robust or cluster-robust standard errors,
with optional weak-instrument, Wu-Hausman, and overidentification
diagnostics.

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

  (required) An object of class formula with regressors and instruments,
  e.g. `y ~ x1 + x2 | z1 + z2`.

- data:

  (optional) A `data.frame`

- weights:

  (optional) The bare (unquoted) name of the weights variable

- subset:

  (optional) A bare (unquoted) expression specifying a subset

- clusters:

  (optional) A bare (unquoted) name of the cluster variable

- fixed_effects:

  (optional) A one-sided formula of fixed effects to absorb, such as
  `~ blockID`. Uses FWL demeaning (see
  [`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
  for details and SE type restrictions). Diagnostics are not available
  with `fixed_effects`.

- se_type:

  (optional) The standard error type. `"HC2"` and `"HC3"` work with
  `fixed_effects` at any number of factors: the second stage runs on
  fitted regressors, but those are demeaned by the same fixed effects,
  so the leverage decomposition
  [`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
  describes applies unchanged. `"CR2"` with `fixed_effects` expands the
  dummies, as in estimatr 1.0.6. Defaults: `"HC2"` (no clusters, with or
  without FE), `"CR2"` (clusters, no FE), `"CR0"` (clusters, with FE).

- ci:

  (optional) Logical. Whether to compute p-values and confidence
  intervals.

- alpha:

  (optional) The significance level, 0.05 by default.

- diagnostics:

  (optional) Logical. Whether to compute IV diagnostic statistics: the
  first-stage F test of the excluded instruments for each endogenous
  regressor, a regression-based Wu-Hausman test of endogeneity, and,
  when the model is overidentified, a test of the overidentifying
  restrictions. That test is Sargan's with `se_type = "classical"` and
  Wooldridge's (1995) robust score test otherwise, with the score's
  variance summed within clusters when `clusters` is given. With
  `weights`, each test is the one on the model with every row multiplied
  by the square root of its weight. The first-stage F and Wu-Hausman
  tests are Wald tests under the fit's own `se_type`, so a classical one
  is valid exactly when the classical weighted standard errors are and a
  robust one exactly when the robust ones are. The robust score test
  uses the score's HC0 or CR0 sandwich under every robust `se_type`, as
  Wooldridge (1995) and Stata define it; the HC1, HC2, HC3, CR2, and
  `"stata"` refinements correct a coefficient covariance and have no
  counterpart in a score test. The overidentification test is `NA`, with
  a warning, for a clustered fit with no more clusters than
  restrictions. All three reproduce Stata's `estat firststage`,
  `estat endogenous`, and `estat overid` on every row of the test
  suite's Stata fixture that Stata answers, except the robust score test
  after aweights under `forceweights`, where Stata computes the
  frequency-weight statistic instead.

- return_vcov:

  (optional) Logical. Whether to return the vcov matrix.

- try_cholesky:

  (optional) Logical. Whether to solve by Cholesky decomposition of
  `X'X` rather than by the default pivoted QR. `FALSE` by default, and
  worth turning on in most applied settings: about 1.4 times faster at n
  = 100,000 with two regressors, and 1.7 times faster at n = 200,000
  with 60 regressors, where it is 0.15s against 0.25s. The saving is per
  fit, so it is worth most in a simulation that fits the same design
  thousands of times.

  Rank deficiency is caught on either path. Redundant columns come back
  as `NA` exactly as they do from
  [`lm()`](https://rdrr.io/r/stats/lm.html) whichever path ran, and a
  design that is rank deficient falls back to the QR.

  Whether it is safe turns on one question, whether two regressors are
  nearly the same variable. Forming `X'X` squares the condition number,
  so the Cholesky path has about twice the rounding error of the QR, and
  only near-collinearity makes that visible. Differences of scale do
  not, because the columns are normalized before either decomposition,
  so a covariate in dollars beside one in years costs nothing. For a
  treatment indicator, a few covariates, block or cluster dummies, the
  centered interactions
  [`lm_lin()`](https://declaredesign.org/r/estimatr/reference/lm_lin.md)
  builds, or a factorial, the two paths agree to at least 10 significant
  digits, which is why
  [`difference_in_means()`](https://declaredesign.org/r/estimatr/reference/difference_in_means.md)
  sets it to `TRUE` internally. Agreement falls to about 3 digits as the
  scaled condition index reaches `1e6`, and the QR fallback takes over
  above roughly `1e8`. Nothing interpretable lives in that range: a
  design at `1e6` returns a coefficient of 4.8e4 with a standard error
  of 4.6e4 on a regressor whose true effect is zero. To check a design
  directly, scale the columns first, since the unscaled condition number
  of a design in mixed units is large for a reason that does not affect
  the fit: `kappa(sweep(X, 2, sqrt(colSums(X^2)), "/"), exact = TRUE)`.

## Value

An object of class `"iv_robust"`, a list holding the estimate table in
`coefficients`, `std.error`, `df`, `statistic`, `p.value`, `conf.low`,
`conf.high`, `term`, and `outcome`; the fit in `fitted.values`,
`residuals`, `vcov`, `nobs`, `k`, `rank`, `df.residual`, and `res_var`;
the summary statistics `r.squared`, `adj.r.squared`, `tss`, and
`fstatistic`; and `se_type`, `weighted`, `clustered`, `fes`, `alpha`,
`terms`, `xlevels`, and `call`.

`residuals` are the structural residuals, `y - X beta`, rather than the
second-stage ones. `ei.iv`, `terms_regressors`, and `formula` record the
two-stage structure. With `diagnostics = TRUE` the object also holds
`diagnostic_first_stage_fstatistic`, `diagnostic_endogeneity_test`, and
`diagnostic_overid_test`.

## Examples

``` r
set.seed(25)
n <- 200
dat <- data.frame(z = rbinom(n, 1, 0.5), cl = rep(1:20, each = 10))
dat$x <- dat$z * rbinom(n, 1, 0.7)
dat$y <- dat$x + rnorm(n)

# Endogenous regressor on the left of the bar, instrument on the right
fit <- iv_robust(y ~ x | z, data = dat)
tidy(fit)
#> # A tibble: 2 × 9
#>   term     estimate std.error statistic p.value conf.low conf.high    df outcome
#>   <chr>       <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 (Interc…    0.137    0.0957      1.43 0.153    -0.0514     0.326   198 y      
#> 2 x           0.625    0.210       2.97 0.00334   0.210      1.04    198 y      

# The same variance menu as lm_robust()
iv_robust(y ~ x | z, data = dat, se_type = "classical")
#>              Estimate Std. Error  t value    Pr(>|t|)    CI Lower  CI Upper  DF
#> (Intercept) 0.1373582 0.09435025 1.455833 0.147022480 -0.04870211 0.3234186 198
#> x           0.6251925 0.21094522 2.963767 0.003411742  0.20920489 1.0411802 198
iv_robust(y ~ x | z, data = dat, clusters = cl)
#>              Estimate Std. Error  t value   Pr(>|t|)     CI Lower  CI Upper
#> (Intercept) 0.1373582 0.06811342 2.016610 0.05907724 -0.005856096 0.2805725
#> x           0.6251925 0.17196811 3.635514 0.00176444  0.265215125 0.9851700
#>                   DF
#> (Intercept) 17.80343
#> x           18.96572

# Weak-instrument, endogeneity, and overidentification tests
summary(iv_robust(y ~ x | z, data = dat, diagnostics = TRUE))
#> 
#> Call:
#> iv_robust(formula = y ~ x | z, data = dat, diagnostics = TRUE)
#> 
#> Standard error type:  HC2 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper  DF
#> (Intercept)   0.1374    0.09573   1.435 0.152908 -0.05142   0.3261 198
#> x             0.6252    0.21047   2.970 0.003342  0.21013   1.0403 198
#> 
#> Multiple R-squared:  0.124 , Adjusted R-squared:  0.1195 
#> F-statistic: 8.823 on 1 and 198 DF,  p-value: 0.003342
#> 
#> Diagnostics:
#>                    value     Df1 Df2 p.value    
#> Weak instruments 175.375   1.000 198  <2e-16 ***
#> Wu-Hausman         2.178   1.000 197   0.142    
#> Score (robust)        NA   0.000  NA      NA    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
