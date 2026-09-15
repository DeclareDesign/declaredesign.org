# Linear Regression with Lin (2013) Covariate Adjustment

Estimates an average treatment effect with covariate adjustment
following Lin (2013): every covariate is centered, interacted with
treatment, and entered alongside it. Centering is what makes the
treatment coefficient the effect estimate, and the interactions avoid
the bias Freedman (2008) identified in ordinary covariate-adjusted
regression.

## Usage

``` r
lm_lin(
  formula,
  covariates,
  data,
  weights,
  subset,
  clusters,
  se_type = NULL,
  ci = TRUE,
  alpha = 0.05,
  return_vcov = TRUE,
  try_cholesky = FALSE
)
```

## Arguments

- formula:

  (required) An object of class formula with only the treatment on the
  RHS

- covariates:

  (required) A right-sided formula with pre-treatment covariates

- data:

  (optional) A `data.frame`

- weights:

  (optional) The bare (unquoted) name of the weights variable

- subset:

  (optional) A bare (unquoted) expression specifying a subset

- clusters:

  (optional) A bare (unquoted) name of the cluster variable

- se_type:

  (optional) The sort of standard error (see
  [`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md))

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
  centered interactions `lm_lin()` builds, or a factorial, the two paths
  agree to at least 10 significant digits, which is why
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

An object of class `"lm_robust"`, as returned by
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md),
with two additions: `scaled_center`, the covariate means used for
centering (taken after any function in the formula is evaluated), and
`treatment_levels`. The treatment row of `coefficients` is the estimate
of the average treatment effect.

## References

Lin, Winston. 2013. "Agnostic Notes on Regression Adjustments to
Experimental Data: Reexamining Freedman's Critique." The Annals of
Applied Statistics 7(1): 295-318.
[doi:10.1214/12-AOAS583](https://doi.org/10.1214/12-AOAS583) .

## Examples

``` r
set.seed(20)
dat <- data.frame(
  x  = rnorm(40, mean = 2.3),
  x2 = rpois(40, lambda = 2),
  x3 = runif(40),
  z  = rep(0:1, 20),
  cl = rep(1:20, each = 2)
)
dat$y <- rnorm(40) + dat$x + 0.35 * dat$z

# lm_robust's interface plus one argument
fit <- lm_lin(y ~ z, covariates = ~ x, data = dat)
tidy(fit)
#> # A tibble: 4 × 9
#>   term    estimate std.error statistic  p.value conf.low conf.high    df outcome
#>   <chr>      <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 (Inter…    2.40      0.200    12.0   4.01e-14    1.99      2.80     36 y      
#> 2 z          0.139     0.319     0.435 6.66e- 1   -0.509     0.787    36 y      
#> 3 x_c        1.06      0.138     7.67  4.43e- 9    0.778     1.34     36 y      
#> 4 z:x_c      0.171     0.292     0.585 5.62e- 1   -0.422     0.764    36 y      

# Several covariates
lm_lin(y ~ z, covariates = ~ x + x2, data = dat)
#>                Estimate Std. Error    t value     Pr(>|t|)   CI Lower  CI Upper
#> (Intercept)  2.42989472  0.2180992 11.1412356 6.829095e-13  1.9866638 2.8731257
#> z            0.16196478  0.3363458  0.4815425 6.332160e-01 -0.5215721 0.8455016
#> x_c          1.05791142  0.1484872  7.1245976 3.108087e-08  0.7561492 1.3596737
#> x2_c         0.08313873  0.1720040  0.4833533 6.319432e-01 -0.2664156 0.4326930
#> z:x_c        0.16420883  0.2656980  0.6180281 5.406749e-01 -0.3757545 0.7041722
#> z:x2_c      -0.22663250  0.2247718 -1.0082781 3.204432e-01 -0.6834238 0.2301588
#>             DF
#> (Intercept) 34
#> z           34
#> x_c         34
#> x2_c        34
#> z:x_c       34
#> z:x2_c      34

# Covariates are centered after any function in the formula is evaluated
fit2 <- lm_lin(y ~ z, covariates = ~ x + log(x3), data = dat)
fit2$scaled_center["log(x3)"]
#>   log(x3) 
#> -1.210077 
mean(log(dat$x3))
#> [1] -1.210077

# Clusters, and multi-valued treatments whether or not they are factors
lm_lin(y ~ z, covariates = ~ x, data = dat, clusters = cl)
#>              Estimate Std. Error    t value     Pr(>|t|)   CI Lower  CI Upper
#> (Intercept) 2.3986657  0.2001761 11.9827783 6.303561e-10  1.9776121 2.8197193
#> z           0.1389292  0.3285313  0.4228797 6.773020e-01 -0.5503936 0.8282521
#> x_c         1.0572066  0.1378425  7.6696733 3.379419e-04  0.7150297 1.3993835
#> z:x_c       0.1710721  0.2030999  0.8423052 4.177404e-01 -0.2765258 0.6186699
#>                    DF
#> (Intercept) 17.707096
#> z           18.332361
#> x_c          5.665371
#> z:x_c       10.884767
dat$z3 <- rep(1:3, length.out = 40)
lm_lin(y ~ z3, covariates = ~ x, data = dat)
#>               Estimate Std. Error    t value     Pr(>|t|)    CI Lower  CI Upper
#> (Intercept)  2.2025788  0.2346917  9.3849900 5.776318e-11  1.72562800 2.6795296
#> z32          0.8783179  0.3394842  2.5872131 1.412568e-02  0.18840303 1.5682327
#> z33         -0.1284882  0.3507253 -0.3663500 7.163738e-01 -0.84124770 0.5842713
#> x_c          0.8553097  0.2091691  4.0890816 2.506626e-04  0.43022683 1.2803925
#> z32:x_c      0.4795874  0.2749677  1.7441587 9.016727e-02 -0.07921429 1.0383891
#> z33:x_c      0.1998578  0.2784735  0.7176904 4.778507e-01 -0.36606852 0.7657841
#>             DF
#> (Intercept) 34
#> z32         34
#> z33         34
#> x_c         34
#> z32:x_c     34
#> z33:x_c     34
lm_lin(y ~ factor(z3), covariates = ~ x, data = dat)
#>                   Estimate Std. Error    t value     Pr(>|t|)    CI Lower
#> (Intercept)      2.2025788  0.2346917  9.3849900 5.776318e-11  1.72562800
#> factor(z3)2      0.8783179  0.3394842  2.5872131 1.412568e-02  0.18840303
#> factor(z3)3     -0.1284882  0.3507253 -0.3663500 7.163738e-01 -0.84124770
#> x_c              0.8553097  0.2091691  4.0890816 2.506626e-04  0.43022683
#> factor(z3)2:x_c  0.4795874  0.2749677  1.7441587 9.016727e-02 -0.07921429
#> factor(z3)3:x_c  0.1998578  0.2784735  0.7176904 4.778507e-01 -0.36606852
#>                  CI Upper DF
#> (Intercept)     2.6795296 34
#> factor(z3)2     1.5682327 34
#> factor(z3)3     0.5842713 34
#> x_c             1.2803925 34
#> factor(z3)2:x_c 1.0383891 34
#> factor(z3)3:x_c 0.7657841 34

# Dropping the intercept gives the mean outcome under each condition
lm_lin(y ~ z3 - 1, covariates = ~ x, data = dat)
#>          Estimate Std. Error   t value     Pr(>|t|)  CI Lower CI Upper DF
#> z31     2.2025788  0.2346917  9.384990 5.776318e-11 1.7256280 2.679530 34
#> z32     3.0808967  0.2452944 12.559998 2.519584e-14 2.5823986 3.579395 34
#> z33     2.0740906  0.2606301  7.957986 2.851310e-09 1.5444266 2.603755 34
#> z31:x_c 0.8553097  0.2091691  4.089082 2.506626e-04 0.4302268 1.280392 34
#> z32:x_c 1.3348970  0.1784812  7.479203 1.114629e-08 0.9721796 1.697614 34
#> z33:x_c 1.0551674  0.1838363  5.739712 1.876206e-06 0.6815671 1.428768 34
```
