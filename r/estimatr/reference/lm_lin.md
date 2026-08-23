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

We also return `terms`, `contrasts`, and `treatment_levels`, used by
`predict`, and `scaled_center` (the means of each of the covariates used
for centering them).

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
#> 1 (Intercept)  2.0074002 0.2012088  9.9767032 6.611620e-12  1.5993299 2.4154705
#> 2           z  0.7263528 0.3037565  2.3912337 2.214176e-02  0.1103061 1.3423996
#> 3         x_c  1.0673003 0.2721532  3.9216889 3.786550e-04  0.5153480 1.6192527
#> 4       z:x_c -0.2815330 0.3235071 -0.8702529 3.899283e-01 -0.9376358 0.3745698
#>   df outcome
#> 1 36       y
#> 2 36       y
#> 3 36       y
#> 4 36       y

# Works with multiple pre-treatment covariates
lm_lin(y ~ z, covariates = ~ x + x2, data = dat)
#>               Estimate Std. Error    t value     Pr(>|t|)    CI Lower  CI Upper
#> (Intercept)  2.0331904  0.2035346  9.9894110 1.198631e-11  1.61955838 2.4468224
#> z            0.6874490  0.3118326  2.2045450 3.435353e-02  0.05372891 1.3211692
#> x_c          1.1154732  0.2539604  4.3923109 1.038122e-04  0.59936348 1.6315829
#> x2_c         0.2456971  0.2805576  0.8757456 3.873136e-01 -0.32446459 0.8158588
#> z:x_c       -0.3294245  0.3067964 -1.0737561 2.904938e-01 -0.95290983 0.2940608
#> z:x2_c      -0.1146326  0.3618812 -0.3167687 7.533560e-01 -0.85006364 0.6207984
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
#> -0.7815509 
mean(log(dat$x3))
#> [1] -0.7815509

# Works easily with clusters
dat$clusterID <- rep(1:20, each = 2)
dat$z_clust <- cluster_ra(clusters = dat$clusterID)

lm_lin(y ~ z_clust, covariates = ~ x, data = dat, clusters = clusterID)
#>              Estimate Std. Error   t value     Pr(>|t|)     CI Lower  CI Upper
#> (Intercept) 2.1869918  0.2599259 8.4139039 6.338923e-05  1.573233032 2.8007505
#> z_clust     0.1547201  0.3441864 0.4495242 6.598847e-01 -0.582978576 0.8924188
#> x_c         0.6678927  0.2632028 2.5375595 5.052778e-02 -0.002227515 1.3380129
#> z_clust:x_c 0.5497024  0.3559260 1.5444288 1.576286e-01 -0.258197236 1.3576021
#>                    DF
#> (Intercept)  7.049192
#> z_clust     14.103467
#> x_c          5.165385
#> z_clust:x_c  8.804119

# Works with multi-valued treatments, whether treatment is specified as a
# factor or not
dat$z_multi <- sample(1:3, size = nrow(dat), replace = TRUE)

lm_lin(y ~ z_multi, covariates = ~ x, data = dat)
#>                  Estimate Std. Error      t value     Pr(>|t|)   CI Lower
#> (Intercept)   2.087111175  0.2397513  8.705318218 3.583451e-10  1.5998780
#> z_multi2      0.632076161  0.3949708  1.600311246 1.187819e-01 -0.1706010
#> z_multi3      0.225302890  0.4595353  0.490284219 6.270820e-01 -0.7085851
#> x_c           0.971855697  0.3265662  2.975983428 5.346867e-03  0.3081933
#> z_multi2:x_c -0.164498171  0.4184305 -0.393131454 6.966767e-01 -1.0148512
#> z_multi3:x_c  0.007310125  0.8138866  0.008981748 9.928862e-01 -1.6467064
#>               CI Upper DF
#> (Intercept)  2.5743444 34
#> z_multi2     1.4347533 34
#> z_multi3     1.1591909 34
#> x_c          1.6355181 34
#> z_multi2:x_c 0.6858548 34
#> z_multi3:x_c 1.6613267 34
lm_lin(y ~ factor(z_multi), covariates = ~ x, data = dat)
#>                          Estimate Std. Error      t value     Pr(>|t|)
#> (Intercept)           2.087111175  0.2397513  8.705318218 3.583451e-10
#> factor(z_multi)2      0.632076161  0.3949708  1.600311246 1.187819e-01
#> factor(z_multi)3      0.225302890  0.4595353  0.490284219 6.270820e-01
#> x_c                   0.971855697  0.3265662  2.975983428 5.346867e-03
#> factor(z_multi)2:x_c -0.164498171  0.4184305 -0.393131454 6.966767e-01
#> factor(z_multi)3:x_c  0.007310125  0.8138866  0.008981748 9.928862e-01
#>                        CI Lower  CI Upper DF
#> (Intercept)           1.5998780 2.5743444 34
#> factor(z_multi)2     -0.1706010 1.4347533 34
#> factor(z_multi)3     -0.7085851 1.1591909 34
#> x_c                   0.3081933 1.6355181 34
#> factor(z_multi)2:x_c -1.0148512 0.6858548 34
#> factor(z_multi)3:x_c -1.6467064 1.6613267 34

# Stratified estimator with blocks
dat$blockID <- rep(1:5, each = 8)
dat$z_block <- block_ra(blocks = dat$blockID)

lm_lin(y ~ z_block, ~ factor(blockID), data = dat)
#>                                Estimate Std. Error    t value     Pr(>|t|)
#> (Intercept)                   2.0704132  0.3451408  5.9987493 1.399160e-06
#> z_block                       0.5931938  0.4501499  1.3177696 1.975558e-01
#> (factor(blockID)2)_c          1.1858143  1.1686199  1.0147134 3.183567e-01
#> (factor(blockID)3)_c          1.4533957  1.1962207  1.2149896 2.338464e-01
#> (factor(blockID)4)_c          0.3103012  1.2793724  0.2425417 8.100110e-01
#> (factor(blockID)5)_c          0.6828532  1.1941371  0.5718382 5.716919e-01
#> z_block:(factor(blockID)2)_c -1.5162764  1.4447190 -1.0495303 3.023158e-01
#> z_block:(factor(blockID)3)_c -1.2130004  1.4862041 -0.8161735 4.208346e-01
#> z_block:(factor(blockID)4)_c -0.4459309  1.5448894 -0.2886491 7.748351e-01
#> z_block:(factor(blockID)5)_c -1.9553208  1.3328098 -1.4670666 1.527638e-01
#>                                CI Lower CI Upper DF
#> (Intercept)                   1.3655416 2.775285 30
#> z_block                      -0.3261349 1.512523 30
#> (factor(blockID)2)_c         -1.2008259 3.572455 30
#> (factor(blockID)3)_c         -0.9896129 3.896404 30
#> (factor(blockID)4)_c         -2.3025258 2.923128 30
#> (factor(blockID)5)_c         -1.7559001 3.121606 30
#> z_block:(factor(blockID)2)_c -4.4667862 1.434233 30
#> z_block:(factor(blockID)3)_c -4.2482341 1.822233 30
#> z_block:(factor(blockID)4)_c -3.6010159 2.709154 30
#> z_block:(factor(blockID)5)_c -4.6772816 0.766640 30

# Fitting the model without an intercept provides estimates of mean outcomes
# under each respective treatment condition
lm_lin(y ~ z_multi - 1, covariates = ~ x, data = dat)
#>               Estimate Std. Error  t value     Pr(>|t|)   CI Lower CI Upper DF
#> z_multi1     2.0871112  0.2397513 8.705318 3.583451e-10  1.5998780 2.574344 34
#> z_multi2     2.7191873  0.3138809 8.663118 4.021285e-10  2.0813046 3.357070 34
#> z_multi3     2.3124141  0.3920357 5.898478 1.166573e-06  1.5157017 3.109126 34
#> z_multi1:x_c 0.9718557  0.3265662 2.975983 5.346867e-03  0.3081933 1.635518 34
#> z_multi2:x_c 0.8073575  0.2616076 3.086139 4.016042e-03  0.2757069 1.339008 34
#> z_multi3:x_c 0.9791658  0.7454971 1.313440 1.978295e-01 -0.5358665 2.494198 34

# Predictions are the same in equivalent models with and without an intercept
lmlin_out3 <- lm_lin(y ~ z_multi - 1, covariates = ~ x, data = dat)
lmlin_out4 <- lm_lin(y ~ z_multi, covariates = ~ x, data = dat)

predict(lmlin_out3, newdata = dat, se.fit = TRUE, interval = "confidence")
#> $fit
#>             fit          lwr      upr
#>  [1,] 2.3148733  1.520493578 3.109253
#>  [2,] 1.0553421  0.336509857 1.774174
#>  [3,] 2.3741742  1.587682857 3.160665
#>  [4,] 3.3882470  2.849157073 3.927337
#>  [5,] 3.2186249  2.110544772 4.326705
#>  [6,] 2.4173422  1.819230739 3.015454
#>  [7,] 1.1887540 -0.271571522 2.649079
#>  [8,] 2.2164078  1.320712787 3.112103
#>  [9,] 3.0791046  2.127778894 4.030430
#> [10,] 2.8346069  2.025135448 3.644078
#> [11,] 2.0824386  1.028973626 3.135903
#> [12,] 0.5038127 -1.383740271 2.391366
#> [13,] 2.7328432  2.099881853 3.365805
#> [14,] 2.6019025  1.916848013 3.286957
#> [15,] 2.3141288  1.519044088 3.109214
#> [16,] 2.8107096  2.204023457 3.417396
#> [17,] 3.0320489  2.480507665 3.583590
#> [18,] 1.6589095  1.176417507 2.141402
#> [19,] 3.5519654  1.991451481 5.112479
#> [20,] 2.7298727  2.082183557 3.377562
#> [21,] 0.8366025 -0.002143485 1.675348
#> [22,] 3.7990467  3.157496850 4.440597
#> [23,] 3.4486446  2.032989808 4.864299
#> [24,] 3.0761846  2.141215856 4.011153
#> [25,] 0.4284628 -0.653805468 1.510731
#> [26,] 1.7118342  0.160006724 3.263662
#> [27,] 2.6619264  1.946085077 3.377768
#> [28,] 2.0867902  1.599626207 2.573954
#> [29,] 2.6100413  1.980831565 3.239251
#> [30,] 4.6693927  3.596362589 5.742423
#> [31,] 1.2489511  0.625547577 1.872355
#> [32,] 0.2001062 -3.609984931 4.010197
#> [33,] 2.9045913  2.325118737 3.484064
#> [34,] 2.1284849  1.216214248 3.040755
#> [35,] 3.0437360  2.494185009 3.593287
#> [36,] 1.2547779  0.634037327 1.875519
#> [37,] 4.1940974  2.535496751 5.852698
#> [38,] 2.5837464  1.892600750 3.274892
#> [39,] 1.5803974  1.079890138 2.080905
#> [40,] 2.1064820  1.082631063 3.130333
#> 
#> $se.fit
#>         1         2         3         4         5         6         7         8 
#> 0.3908879 0.3537135 0.3870062 0.2652682 0.5452494 0.2943108 0.7185776 0.4407417 
#>         9        10        11        12        13        14        15        16 
#> 0.4681158 0.3983140 0.5183751 0.9288021 0.3114593 0.3370926 0.3912348 0.2985301 
#>        17        18        19        20        21        22        23        24 
#> 0.2713951 0.2374183 0.7678771 0.3187063 0.4127190 0.3156854 0.6965967 0.4600670 
#>        25        26        27        28        29        30        31        32 
#> 0.5325483 0.7636027 0.3522417 0.2397172 0.3096132 0.5280025 0.3067562 1.8748193 
#>        33        34        35        36        37        38        39        40 
#> 0.2851392 0.4488981 0.2704158 0.3054458 0.8161423 0.3400898 0.2462830 0.5038030 
#> 
predict(lmlin_out4, newdata = dat, se.fit = TRUE, interval = "confidence")
#> $fit
#>             fit          lwr      upr
#>  [1,] 2.3148733  1.520493578 3.109253
#>  [2,] 1.0553421  0.336509857 1.774174
#>  [3,] 2.3741742  1.587682857 3.160665
#>  [4,] 3.3882470  2.849157073 3.927337
#>  [5,] 3.2186249  2.110544772 4.326705
#>  [6,] 2.4173422  1.819230739 3.015454
#>  [7,] 1.1887540 -0.271571522 2.649079
#>  [8,] 2.2164078  1.320712787 3.112103
#>  [9,] 3.0791046  2.127778894 4.030430
#> [10,] 2.8346069  2.025135448 3.644078
#> [11,] 2.0824386  1.028973626 3.135903
#> [12,] 0.5038127 -1.383740271 2.391366
#> [13,] 2.7328432  2.099881853 3.365805
#> [14,] 2.6019025  1.916848013 3.286957
#> [15,] 2.3141288  1.519044088 3.109214
#> [16,] 2.8107096  2.204023457 3.417396
#> [17,] 3.0320489  2.480507665 3.583590
#> [18,] 1.6589095  1.176417507 2.141402
#> [19,] 3.5519654  1.991451481 5.112479
#> [20,] 2.7298727  2.082183557 3.377562
#> [21,] 0.8366025 -0.002143485 1.675348
#> [22,] 3.7990467  3.157496850 4.440597
#> [23,] 3.4486446  2.032989808 4.864299
#> [24,] 3.0761846  2.141215856 4.011153
#> [25,] 0.4284628 -0.653805468 1.510731
#> [26,] 1.7118342  0.160006724 3.263662
#> [27,] 2.6619264  1.946085077 3.377768
#> [28,] 2.0867902  1.599626207 2.573954
#> [29,] 2.6100413  1.980831565 3.239251
#> [30,] 4.6693927  3.596362589 5.742423
#> [31,] 1.2489511  0.625547577 1.872355
#> [32,] 0.2001062 -3.609984931 4.010197
#> [33,] 2.9045913  2.325118737 3.484064
#> [34,] 2.1284849  1.216214248 3.040755
#> [35,] 3.0437360  2.494185009 3.593287
#> [36,] 1.2547779  0.634037327 1.875519
#> [37,] 4.1940974  2.535496751 5.852698
#> [38,] 2.5837464  1.892600750 3.274892
#> [39,] 1.5803974  1.079890138 2.080905
#> [40,] 2.1064820  1.082631063 3.130333
#> 
#> $se.fit
#>         1         2         3         4         5         6         7         8 
#> 0.3908879 0.3537135 0.3870062 0.2652682 0.5452494 0.2943108 0.7185776 0.4407417 
#>         9        10        11        12        13        14        15        16 
#> 0.4681158 0.3983140 0.5183751 0.9288021 0.3114593 0.3370926 0.3912348 0.2985301 
#>        17        18        19        20        21        22        23        24 
#> 0.2713951 0.2374183 0.7678771 0.3187063 0.4127190 0.3156854 0.6965967 0.4600670 
#>        25        26        27        28        29        30        31        32 
#> 0.5325483 0.7636027 0.3522417 0.2397172 0.3096132 0.5280025 0.3067562 1.8748193 
#>        33        34        35        36        37        38        39        40 
#> 0.2851392 0.4488981 0.2704158 0.3054458 0.8161423 0.3400898 0.2462830 0.5038030 
#> 

if (FALSE) { # \dontrun{
  # Can also use 'margins' package if you have it installed to get
  # marginal effects
  library(margins)
  # Instruct 'margins' to treat z as a factor
  lmlout <- lm_lin(y ~ factor(z_block), ~ x, data = dat)
  summary(margins(lmlout))

  # Can output results using 'texreg'
  library(texreg)
  texregobj <- extract(lmlout)
} # }
```
