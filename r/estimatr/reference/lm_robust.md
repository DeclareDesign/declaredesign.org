# Ordinary Least Squares with Robust Standard Errors

Fits a linear model by ordinary least squares and returns
heteroskedasticity-robust or cluster-robust standard errors, with the
small-sample corrections used in design-based work. Fixed effects can be
absorbed rather than expanded into dummy columns, at no cost in the
available standard error types.

## Usage

``` r
lm_robust(
  formula,
  data,
  weights,
  subset,
  clusters,
  fixed_effects,
  se_type = NULL,
  ci = TRUE,
  alpha = 0.05,
  return_vcov = TRUE,
  try_cholesky = FALSE
)
```

## Arguments

- formula:

  (required) An object of class formula, as in
  [`lm()`](https://rdrr.io/r/stats/lm.html)

- data:

  (optional) A `data.frame`

- weights:

  (optional) The bare (unquoted) name of the weights variable

- subset:

  (optional) A bare (unquoted) expression specifying a subset

- clusters:

  (optional) A bare (unquoted) name of the cluster variable

- fixed_effects:

  (optional) A one-sided formula of fixed effects to absorb rather than
  expand into dummy columns, such as `~ blockID` or `~ block + year`.
  Each variable is demeaned within the groups before OLS is run, so by
  the Frisch-Waugh-Lovell theorem the coefficients and residuals are the
  dummy regression's exactly.

  Absorbing costs nothing in available standard error types. `"HC2"` and
  `"HC3"` are exact at any number of factors, because the leverage of
  the full design splits into the demeaned-X leverage plus a term that
  is cheap to compute, so no dummy hat matrix is built.

  `"CR2"` is the exception: its adjustment is built from cluster-level
  blocks of the hat matrix rather than from the diagonal, and blocks do
  not split that way, so it expands the dummies and pays for the
  expansion. That is why `fixed_effects` with `clusters` defaults to
  `"CR0"`. Asking for `se_type = "CR2"` still works and still gives the
  1.0.6 number. Refused is the three together: `"CR2"` with both
  `weights` and `fixed_effects`, as in estimatr 1.0.6.

  The projection identity, the several-factor case, the exact-rank
  calculation, and the weighted CR2 and HC2 conventions are derived in
  [`vignette("mathematical-notes")`](https://declaredesign.org/r/estimatr/articles/mathematical-notes.md).

- se_type:

  (optional) The standard error type. Defaults depend on whether
  clusters and/or fixed effects are present:

  - No clusters, no FE: `"HC2"` (default), `"HC0"`, `"HC1"`, `"HC3"`,
    `"classical"`, `"stata"`, `"none"`.

  - Clusters, no FE: `"CR2"` (default), `"CR0"`, `"stata"`, `"none"`.

  - No clusters, with FE (any number of factors): `"HC2"` (default),
    `"HC0"`, `"HC1"`, `"HC3"`, `"classical"`, `"stata"`, `"none"`. The
    same menu as with no FE at all.

  - Clusters, with FE: `"CR0"` (default), `"CR2"`, `"stata"`, `"none"`.
    `"CR2"` expands the fixed effects into dummies, so it is not the
    default here; it is refused with `weights`.

  `"stata"` means two different things. With no clusters it is exactly
  `"HC1"`, and the fitted object reports `se_type = "HC1"`. With
  clusters it is **not** an alias for `"CR0"`: it is CR0 scaled by
  Stata's finite-sample factor, `(J / (J - 1)) * ((N - 1) / (N - K))` on
  the variance, and the object reports `se_type = "stata"` to keep the
  distinction visible.

- ci:

  (optional) Logical. Whether to compute p-values and confidence
  intervals.

- alpha:

  (optional) The significance level, 0.05 by default.

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

An object of class `"lm_robust"`, a list holding the estimate table in
`coefficients`, `std.error`, `df`, `statistic`, `p.value`, `conf.low`,
`conf.high`, `term`, and `outcome`; the fit in `fitted.values`,
`residuals`, `vcov`, `nobs`, `k`, `rank`, `df.residual`, and `res_var`;
the summary statistics `r.squared`, `adj.r.squared`, `tss`, and
`fstatistic`; and `se_type`, `weighted`, `clustered`, `fes`, `alpha`,
`terms`, `xlevels`, and `call`.

Absorbed fits add `fixed_effects`, `felevels` (the absorbed levels of
each factor), and the within-projection summaries `proj_r.squared`,
`proj_adj.r.squared`, `proj_tss`, and `proj_fstatistic`.

## Examples

``` r
set.seed(15)
dat <- data.frame(
  y = rpois(40, lambda = 4),
  x = rnorm(40),
  z = rbinom(40, 1, prob = 0.4),
  cl = rep(1:10, each = 4),
  bl = rep(c("A", "B", "C", "D"), each = 10),
  w = runif(40)
)

# HC2 is the default
fit <- lm_robust(y ~ x + z, data = dat)
fit
#>               Estimate Std. Error    t value     Pr(>|t|)   CI Lower  CI Upper
#> (Intercept)  4.5970879  0.4386909 10.4791036 1.261673e-12  3.7082156 5.4859602
#> x            0.1735281  0.3813973  0.4549799 6.517822e-01 -0.5992562 0.9463125
#> z           -0.9016607  0.5423749 -1.6624308 1.048766e-01 -2.0006166 0.1972952
#>             DF
#> (Intercept) 37
#> x           37
#> z           37
tidy(fit)
#> # A tibble: 3 × 9
#>   term    estimate std.error statistic  p.value conf.low conf.high    df outcome
#>   <chr>      <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 (Inter…    4.60      0.439    10.5   1.26e-12    3.71      5.49     37 y      
#> 2 x          0.174     0.381     0.455 6.52e- 1   -0.599     0.946    37 y      
#> 3 z         -0.902     0.542    -1.66  1.05e- 1   -2.00      0.197    37 y      
summary(fit)
#> 
#> Call:
#> lm_robust(formula = y ~ x + z, data = dat)
#> 
#> Standard error type:  HC2 
#> 
#> Coefficients:
#>             Estimate Std. Error t value  Pr(>|t|) CI Lower CI Upper DF
#> (Intercept)   4.5971     0.4387  10.479 1.262e-12   3.7082   5.4860 37
#> x             0.1735     0.3814   0.455 6.518e-01  -0.5993   0.9463 37
#> z            -0.9017     0.5424  -1.662 1.049e-01  -2.0006   0.1973 37
#> 
#> Multiple R-squared:  0.05748 ,   Adjusted R-squared:  0.00653 
#> F-statistic: 1.453 on 2 and 37 DF,  p-value: 0.2468
confint(fit, level = 0.8)
#>                   10 %       90 %
#> (Intercept)  4.0246601  5.1695157
#> x           -0.3241398  0.6711961
#> z           -1.6093809 -0.1939405

# Other variance estimators, including Stata's
lm_robust(y ~ x + z, data = dat, se_type = "classical")
#>               Estimate Std. Error    t value     Pr(>|t|)   CI Lower  CI Upper
#> (Intercept)  4.5970879  0.3802497 12.0896557 2.044110e-14  3.8266288 5.3675470
#> x            0.1735281  0.3380391  0.5133374 6.107671e-01 -0.5114042 0.8584604
#> z           -0.9016607  0.6184263 -1.4579922 1.532792e-01 -2.1547114 0.3513900
#>             DF
#> (Intercept) 37
#> x           37
#> z           37
lm_robust(y ~ x + z, data = dat, se_type = "stata")
#>               Estimate Std. Error    t value     Pr(>|t|)   CI Lower  CI Upper
#> (Intercept)  4.5970879  0.4398377 10.4517829 1.356971e-12  3.7058921 5.4882837
#> x            0.1735281  0.3731076  0.4650887 6.445936e-01 -0.5824596 0.9295159
#> z           -0.9016607  0.5377414 -1.6767553 1.020208e-01 -1.9912282 0.1879068
#>             DF
#> (Intercept) 37
#> x           37
#> z           37

# Clustered inference defaults to CR2
lm_robust(y ~ x + z, data = dat, clusters = cl)
#>               Estimate Std. Error    t value     Pr(>|t|)   CI Lower   CI Upper
#> (Intercept)  4.5970879  0.2881143 15.9557767 6.225249e-07  3.9211723  5.2730035
#> x            0.1735281  0.3062021  0.5667111 5.912145e-01 -0.5734551  0.9205113
#> z           -0.9016607  0.3382580 -2.6656009 3.142761e-02 -1.6972729 -0.1060485
#>                   DF
#> (Intercept) 7.285030
#> x           6.076014
#> z           7.188890
lm_robust(y ~ x + z, data = dat, clusters = cl, se_type = "stata")
#>               Estimate Std. Error    t value     Pr(>|t|)   CI Lower   CI Upper
#> (Intercept)  4.5970879  0.2912685 15.7829901 7.245925e-08  3.9381928  5.2559830
#> x            0.1735281  0.3118984  0.5563611 5.915262e-01 -0.5320351  0.8790913
#> z           -0.9016607  0.3376476 -2.6704193 2.560486e-02 -1.6654727 -0.1378487
#>             DF
#> (Intercept)  9
#> x            9
#> z            9

# Weights and subsets behave as they do in lm()
lm_robust(y ~ x + z, data = dat, weights = w, clusters = cl)
#>               Estimate Std. Error    t value     Pr(>|t|)  CI Lower    CI Upper
#> (Intercept)  4.9189683  0.3379954 14.5533567 1.472987e-05  4.070464  5.76747278
#> x            0.3033278  0.5700630  0.5320952 6.165681e-01 -1.144574  1.75122918
#> z           -0.9879729  0.3890518 -2.5394380 4.117960e-02 -1.922489 -0.05345721
#>                   DF
#> (Intercept) 5.431130
#> x           5.208788
#> z           6.499607
lm_robust(y ~ x, data = dat, subset = z == 1)
#>              Estimate Std. Error   t value     Pr(>|t|)  CI Lower  CI Upper DF
#> (Intercept)  4.026354  0.3785824 10.635346 1.833544e-07  3.201494 4.8512143 12
#> x           -0.462500  0.3599288 -1.284976 2.230493e-01 -1.246718 0.3217174 12

# Fixed effects are absorbed rather than expanded into dummies. With a
# single factor the HC2 default is exact and costs nothing extra.
lm_robust(y ~ z, data = dat, fixed_effects = ~ bl)
#>     Estimate Std. Error  t value  Pr(>|t|)  CI Lower  CI Upper DF
#> z -0.5857143  0.5623705 -1.04151 0.3047803 -1.727387 0.5559586 35
```
