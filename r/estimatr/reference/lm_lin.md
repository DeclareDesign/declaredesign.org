# Linear regression with the Lin (2013) covariate adjustment

This function is a wrapper for
[`lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
that is useful for estimating treatment effects with pre-treatment
covariate data. This implements the method described by Lin (2013).

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

  an object of class formula, as in
  [`lm`](https://rdrr.io/r/stats/lm.html), such as `Y ~ Z` with only one
  variable on the right-hand side, the treatment

- covariates:

  a right-sided formula with pre-treatment covariates on the right hand
  side, such as ` ~ x1 + x2 + x3`.

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

- se_type:

  The sort of standard error sought. If `clusters` is not specified the
  options are "HC0", "HC1" (or "stata", the equivalent), "HC2"
  (default), "HC3", or "classical". If `clusters` is specified the
  options are "CR0", "CR2" (default), or "stata" are permissible.

- ci:

  logical. Whether to compute and return p-values and confidence
  intervals, TRUE by default.

- alpha:

  The significance level, 0.05 by default.

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

An object of class `"lm_robust"`.

The post-estimation commands functions `summary` and
[`tidy`](https://generics.r-lib.org/reference/tidy.html) return results
in a `data.frame`. To get useful data out of the return, you can use
these data frames, you can use the resulting list directly, or you can
use the generic accessor functions `coef`, `vcov`, `confint`, and
`predict`. Marginal effects and uncertainty about them can be gotten by
passing this object to
[`margins`](https://rdrr.io/pkg/margins/man/margins.html) from the
margins.

Users who want to print the results in TeX of HTML can use the
[`extract`](https://rdrr.io/pkg/texreg/man/extract.html) function and
the texreg package.

An object of class `"lm_robust"` is a list containing at least the
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

- N:

  the number of observations used

- k:

  the number of columns in the design matrix (includes linearly
  dependent columns!)

- rank:

  the rank of the fitted model

- vcov:

  the fitted variance covariance matrix

- r.squared:

  The \\R^2\\, \$\$R^2 = 1 - Sum(e\[i\]^2) / Sum((y\[i\] - y^\*)^2),\$\$
  where \\y^\*\\ is the mean of \\y\[i\]\\ if there is an intercept and
  zero otherwise, and \\e\[i\]\\ is the ith residual.

- adj.r.squared:

  The \\R^2\\ but penalized for having more parameters, `rank`

- weighted:

  whether or not weights were applied

- call:

  the original function call

- fitted.values:

  the matrix of predicted means

We also return `terms` and `contrasts`, used by `predict`, and
`scaled_center` (the means of each of the covariates used for centering
them).

## Details

This function is simply a wrapper for
[`lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
and implements the Lin estimator (see the reference below). This method
pre-processes the data by taking the covariates specified in the
`` `covariates` `` argument, centering them by subtracting from each
covariate its mean, and interacting them with the treatment. If the
treatment has multiple values, a series of dummies for each value is
created and each of those is interacted with the demeaned covariates.
More details can be found in the [Getting Started
vignette](https://declaredesign.org/r/estimatr/articles/getting-started.html)
and the [mathematical
notes](https://declaredesign.org/r/estimatr/articles/mathematical-notes.html).

## References

Freedman, David A. 2008. "On Regression Adjustments in Experiments with
Several Treatments." The Annals of Applied Statistics. JSTOR, 176-96.
[doi:10.1214/07-AOAS143](https://doi.org/10.1214/07-AOAS143) .

Lin, Winston. 2013. "Agnostic Notes on Regression Adjustments to
Experimental Data: Reexamining Freedman's Critique." The Annals of
Applied Statistics 7 (1). Institute of Mathematical Statistics: 295-318.
[doi:10.1214/12-AOAS583](https://doi.org/10.1214/12-AOAS583) .

## See also

[`lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)

## Examples

``` r
library(fabricatr)
library(randomizr)
dat <- fabricate(
  N = 40,
  x = rnorm(N, mean = 2.3),
  x2 = rpois(N, lambda = 2),
  x3 = runif(N),
  y0 = rnorm(N) + x,
  y1 = rnorm(N) + x + 0.35
)

dat$z <- complete_ra(N = nrow(dat))
dat$y <- ifelse(dat$z == 1, dat$y1, dat$y0)

# Same specification as lm_robust() with one additional argument
lmlin_out <- lm_lin(y ~ z, covariates = ~ x, data = dat)
tidy(lmlin_out)
#>          term   estimate std.error  statistic      p.value   conf.low conf.high
#> 1 (Intercept)  2.1695249 0.2630931  8.2462256 8.203489e-10  1.6359474 2.7031024
#> 2           z  0.3176985 0.3660547  0.8678989 3.911985e-01 -0.4246948 1.0600918
#> 3         x_c  1.1158804 0.3607171  3.0935061 3.811699e-03  0.3843123 1.8474485
#> 4       z:x_c -0.3759682 0.4416031 -0.8513712 4.001911e-01 -1.2715807 0.5196444
#>   df outcome
#> 1 36       y
#> 2 36       y
#> 3 36       y
#> 4 36       y

# Works with multiple pre-treatment covariates
lm_lin(y ~ z, covariates = ~ x + x2, data = dat)
#>               Estimate Std. Error    t value     Pr(>|t|)   CI Lower  CI Upper
#> (Intercept)  2.2078434  0.2527477  8.7353654 3.301533e-10  1.6941983 2.7214886
#> z            0.1305558  0.3299517  0.3956816 6.948120e-01 -0.5399867 0.8010984
#> x_c          1.1201793  0.3663516  3.0576619 4.326312e-03  0.3756633 1.8646952
#> x2_c         0.1779870  0.2543577  0.6997510 4.888432e-01 -0.3389300 0.6949040
#> z:x_c       -0.4265414  0.4143615 -1.0293942 3.105622e-01 -1.2686254 0.4155426
#> z:x2_c       0.4007569  0.3116628  1.2858668 2.071784e-01 -0.2326182 1.0341320
#>             DF
#> (Intercept) 34
#> z           34
#> x_c         34
#> x2_c        34
#> z:x_c       34
#> z:x2_c      34

# Also centers data AFTER evaluating any functions in formula
lmlin_out2 <- lm_lin(y ~ z, covariates = ~ x + log(x3), data = dat)
lmlin_out2$scaled_center["log(x3)"]
#>    log(x3) 
#> -0.9082314 
mean(log(dat$x3))
#> [1] -0.9082314

# Works easily with clusters
dat$clusterID <- rep(1:20, each = 2)
dat$z_clust <- cluster_ra(clusters = dat$clusterID)

lm_lin(y ~ z_clust, covariates = ~ x, data = dat, clusters = clusterID)
#>               Estimate Std. Error   t value     Pr(>|t|)    CI Lower
#> (Intercept)  1.9823963  0.2215822  8.946552 1.653459e-05  1.47357969
#> z_clust      0.6202697  0.3403026  1.822701 8.654839e-02 -0.09945087
#> x_c          1.2962678  0.2584531  5.015486 5.403446e-03  0.60917533
#> z_clust:x_c -0.7906409  0.3215639 -2.458736 3.552230e-02 -1.51492528
#>                CI Upper        DF
#> (Intercept)  2.49121299  8.199197
#> z_clust      1.33999031 16.475542
#> x_c          1.98336024  4.502141
#> z_clust:x_c -0.06635648  9.263654

# Works with multi-valued treatments
dat$z_multi <- sample(1:3, size = nrow(dat), replace = TRUE)
lm_lin(y ~ z_multi, covariates = ~ x, data = dat)
#>                Estimate Std. Error    t value     Pr(>|t|)   CI Lower
#> (Intercept)   2.3242674  0.3105887  7.4834257 1.101187e-08  1.6930753
#> z_multi2      0.3326992  0.5377691  0.6186656 5.402597e-01 -0.7601790
#> z_multi3     -0.1635568  0.3589169 -0.4556956 6.515061e-01 -0.8929637
#> x_c           1.3790405  0.2723045  5.0643318 1.417632e-05  0.8256511
#> z_multi2:x_c -0.2493044  0.6072650 -0.4105365 6.839882e-01 -1.4834154
#> z_multi3:x_c -1.1250641  0.2865315 -3.9264934 3.995635e-04 -1.7073662
#>                CI Upper DF
#> (Intercept)   2.9554596 34
#> z_multi2      1.4255775 34
#> z_multi3      0.5658500 34
#> x_c           1.9324298 34
#> z_multi2:x_c  0.9848065 34
#> z_multi3:x_c -0.5427620 34

# Stratified estimator with blocks
dat$blockID <- rep(1:5, each = 8)
dat$z_block <- block_ra(blocks = dat$blockID)

lm_lin(y ~ z_block, ~ factor(blockID), data = dat)
#>                                 Estimate Std. Error     t value     Pr(>|t|)
#> (Intercept)                   2.23463020  0.2833025  7.88778950 8.385351e-09
#> z_block                       0.33866318  0.4582224  0.73908033 4.656020e-01
#> (factor(blockID)2)_c          0.58760354  1.0945574  0.53684122 5.953366e-01
#> (factor(blockID)3)_c         -0.05860625  0.7991543 -0.07333534 9.420261e-01
#> (factor(blockID)4)_c         -0.33820266  1.0002727 -0.33811045 7.376347e-01
#> (factor(blockID)5)_c         -1.04976411  0.8191669 -1.28150218 2.098342e-01
#> z_block:(factor(blockID)2)_c  0.55049472  1.4188758  0.38797950 7.007706e-01
#> z_block:(factor(blockID)3)_c  0.85255003  1.5329347  0.55615548 5.822294e-01
#> z_block:(factor(blockID)4)_c  2.03911494  1.6820765  1.21226056 2.348737e-01
#> z_block:(factor(blockID)5)_c  1.18597842  1.3124738  0.90362061 3.733957e-01
#>                                CI Lower  CI Upper DF
#> (Intercept)                   1.6560494 2.8132110 30
#> z_block                      -0.5971519 1.2744782 30
#> (factor(blockID)2)_c         -1.6477809 2.8229880 30
#> (factor(blockID)3)_c         -1.6906970 1.5734845 30
#> (factor(blockID)4)_c         -2.3810321 1.7046268 30
#> (factor(blockID)5)_c         -2.7227260 0.6231978 30
#> z_block:(factor(blockID)2)_c -2.3472362 3.4482257 30
#> z_block:(factor(blockID)3)_c -2.2781202 3.9832203 30
#> z_block:(factor(blockID)4)_c -1.3961435 5.4743733 30
#> z_block:(factor(blockID)5)_c -1.4944508 3.8664076 30

if (FALSE) { # \dontrun{
  # Can also use 'margins' package if you have it installed to get
  # marginal effects
  library(margins)
  lmlout <- lm_lin(y ~ z_block, ~ x, data = dat)
  summary(margins(lmlout))

  # Can output results using 'texreg'
  library(texreg)
  texregobj <- extract(lmlout)
} # }
```
