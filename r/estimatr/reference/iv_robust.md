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
  first_stage = FALSE,
  return_vcov = TRUE,
  try_cholesky = FALSE,
  df_type = NULL
)
```

## Arguments

- formula:

  (required) An object of class formula with regressors and instruments,
  in either of two forms. The two-part `y ~ x1 + d | x1 + z1 + z2` lists
  the regressors left of the bar and every instrument right of it, so an
  exogenous regressor appears on both sides. The three-part
  `y ~ x1 | d | z1 + z2`, the syntax of
  [`ivreg::ivreg()`](https://zeileis.github.io/ivreg/reference/ivreg.html),
  lists the exogenous regressors, the endogenous regressors, and the
  excluded instruments once each, and fits the same model; as in
  [`ivreg::ivreg()`](https://zeileis.github.io/ivreg/reference/ivreg.html),
  `formula(fit)` returns its two-part form with the endogenous
  regressors first, `y ~ d + x1 | z1 + z2 + x1`, and the coefficients
  follow that order.

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
  for details and SE type restrictions). The diagnostics are those of
  the same model with the fixed effects as dummies among both the
  regressors and the instruments.

- se_type:

  (optional) The standard error type. `"HC2"` and `"HC3"` work with
  `fixed_effects` at any number of factors: the second stage runs on
  fitted regressors, but those are demeaned by the same fixed effects,
  so the leverage decomposition
  [`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
  describes applies unchanged. `"CR2"` and `"CR3"` with `fixed_effects`
  expand the dummies, as CR2 did in estimatr 1.0.6. Defaults: `"HC2"`
  (no clusters, with or without FE), `"CR2"` (clusters, no FE), `"CR0"`
  (clusters, with FE). `"stata"` and `"stata_nested"` count K as
  [`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
  describes.

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

- first_stage:

  (optional) Logical. Whether to return the first-stage regressions in
  `first_stage`, one
  [`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
  fit per endogenous regressor (see Value). `FALSE` by default, as
  `diagnostics` is: the fits are not needed for the estimates, and on
  100,000 rows they add about two thirds to the running time and double
  the size of the object.

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

- df_type:

  (optional) The degrees of freedom (see
  [`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md))

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
two-stage structure. With `first_stage = TRUE`, `first_stage` is a list
of
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
fits, one per endogenous regressor and named by it, each the regression
of that regressor on every instrument with the fit's `se_type`,
clusters, weights, fixed effects, and `df_type`, so
[`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`summary()`](https://rdrr.io/r/base/summary.html), and table packages
take them as they are (see the examples). Each equals the
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
call recorded in its `call`, which is `NULL` for the dummy column of an
endogenous factor. With `diagnostics = TRUE` the object also holds
`diagnostic_first_stage_fstatistic`, `diagnostic_endogeneity_test`, and
`diagnostic_overid_test`.

## Weak instruments

The confidence intervals in `conf.low` and `conf.high` are Wald
intervals, and they rely on a strong first stage. When the instruments
barely move the endogenous regressor, they can cover the true
coefficient far less often than `1 - alpha`; a small first-stage F from
`diagnostics = TRUE` is the usual warning sign.

The Anderson-Rubin (1949) test keeps its size however weak the
instruments are, and
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
computes it directly. To test that the coefficient on the endogenous
regressor `x` equals `b`, regress `y - b * x` on the excluded
instruments and the exogenous regressors, with the `se_type`,
`clusters`, `weights`, and `fixed_effects` of the IV fit, and test that
the coefficients on the excluded instruments are zero. With one
instrument that test is the instrument's own row of the fit; with
several it is the `joint_hypothesis` of
`lh_robust(..., linear_hypothesis = c("z1 = 0", "z2 = 0"))`. At `b = 0`
the regression is the reduced form, which in an experiment that
randomizes the instrument is the intent-to-treat regression.

The Anderson-Rubin confidence set is every `b` that the test does not
reject, found over a grid (see the examples). Under a weak first stage
it need not be an interval: it can be the union of two rays or the whole
real line, which says that the data rule out no value of the
coefficient. A set that reaches either end of the grid should be checked
on a wider one. With several endogenous regressors, the same regression
with `y` minus each regressor times its hypothesized value tests all of
them jointly.

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

# Exogenous regressors, endogenous regressors, and instruments, each once
dat$w <- rnorm(n)
iv_robust(y ~ w | x | z, data = dat)
#>               Estimate Std. Error    t value    Pr(>|t|)    CI Lower  CI Upper
#> (Intercept)  0.1400588 0.09772890  1.4331362 0.153403995 -0.05267029 0.3327880
#> x            0.6173731 0.21775597  2.8351604 0.005058038  0.18794112 1.0468051
#> w           -0.0138943 0.06951721 -0.1998684 0.841789638 -0.15098773 0.1231991
#>              DF
#> (Intercept) 197
#> x           197
#> w           197

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

# The first stage, one lm_robust fit per endogenous regressor
fit_fs <- iv_robust(y ~ x | z, data = dat, first_stage = TRUE)
#> The standard error is NA for (Intercept): every observation that identifies that coefficient is fitted exactly, so the variance is built from residuals that are all numerically zero. Reported as a number it would be zero or rounding error, and its t statistic would put p at 0.
tidy(fit_fs$first_stage$x)
#> # A tibble: 2 × 9
#>   term   estimate std.error statistic   p.value conf.low conf.high    df outcome
#>   <chr>     <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 (Int… -3.97e-17   NA           NA   NA          NA        NA       198 x      
#> 2 z      6.56e- 1    0.0495      13.2  4.39e-29    0.558     0.754   198 x      

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

# Anderson-Rubin confidence set: the values of b at which the regression of
# y - b * x on the instrument finds no effect of z. range() reports the set
# as one interval, so check ar_p for a set with more than one piece.
b_grid <- seq(-2, 4, by = 0.01)
ar_p <- sapply(b_grid, function(b) {
  lm_robust(I(y - b * x) ~ z, data = dat)$p.value[["z"]]
})
range(b_grid[ar_p > 0.05])
#> [1] 0.20 1.03
confint(fit)["x", ]
#>     2.5 %    97.5 % 
#> 0.2101339 1.0402512 
```
