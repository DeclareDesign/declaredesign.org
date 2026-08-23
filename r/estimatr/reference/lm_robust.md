# Ordinary Least Squares with Robust Standard Errors

This formula fits a linear model, provides a variety of options for
robust standard errors, and conducts coefficient tests

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

  an object of class formula, as in
  [`lm`](https://rdrr.io/r/stats/lm.html)

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
margins, or to `emmeans` in the emmeans package.

Users who want to print the results in TeX of HTML can use the
[`extract`](https://rdrr.io/pkg/texreg/man/extract.html) function and
the texreg package.

If users specify a multivariate linear regression model (multiple
outcomes), then some of the below components will be of higher dimension
to accommodate the additional models.

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

- fstatistic:

  a vector with the value of the F-statistic with the numerator and
  denominator degrees of freedom

- weighted:

  whether or not weights were applied

- call:

  the original function call

- fitted.values:

  the matrix of predicted means

We also return `terms` and `contrasts`, used by `predict`. If
`fixed_effects` are specified, then we return `proj_fstatistic`,
`proj_r.squared`, and `proj_adj.r.squared`, which are model fit
statistics that are computed on the projected model (after demeaning the
fixed effects).

## Details

This function performs linear regression and provides a variety of
standard errors. It takes a formula and data much in the same was as
[`lm`](https://rdrr.io/r/stats/lm.html) does, and all auxiliary
variables, such as clusters and weights, can be passed either as quoted
names of columns, as bare column names, or as a self-contained vector.
Examples of usage can be seen below and in the [Getting Started
vignette](https://declaredesign.org/r/estimatr/articles/getting-started.html).

The mathematical notes in [this
vignette](https://declaredesign.org/r/estimatr/articles/mathematical-notes.html)
specify the exact estimators used by this function. The default variance
estimators have been chosen largely in accordance with the procedures in
[this
manual](https://github.com/acoppock/Green-Lab-SOP/blob/master/Green_Lab_SOP.pdf).
The default for the case without clusters is the HC2 estimator and the
default with clusters is the analogous CR2 estimator. Users can easily
replicate Stata standard errors in the clustered or non-clustered case
by setting `` `se_type` = "stata" ``.

The function estimates the coefficients and standard errors in C++,
using the `RcppEigen` package. By default, we estimate the coefficients
using Column-Pivoting QR decomposition from the Eigen C++ library,
although users could get faster solutions by setting
`` `try_cholesky` = TRUE `` to use a Cholesky decomposition instead.
This will likely result in quicker solutions, but the algorithm does not
reliably detect when there are linear dependencies in the model and may
fail silently if they exist.

If `` `fixed_effects` `` are specified, both the outcome and design
matrix are centered using the method of alternating projections
(Halperin 1962; Gaure 2013). Specifying fixed effects in this way will
result in large speed gains with standard error estimators that do not
need to invert the matrix of fixed effects. This means using
"classical", "HC0", "HC1", "CR0", or "stata" standard errors will be
faster than other standard error estimators. Be wary when specifying
fixed effects that may result in perfect fits for some observations or
if there are intersecting groups across multiple fixed effect variables
(e.g. if you specify both "year" and "country" fixed effects with an
unbalanced panel where one year you only have data for one country).

As with `` `lm()` ``, multivariate regression (multiple outcomes) will
only admit observations into the estimation that have no missingness on
any outcome.

## References

Abadie, Alberto, Susan Athey, Guido W Imbens, and Jeffrey Wooldridge.
2017. "A Class of Unbiased Estimators of the Average Treatment Effect in
Randomized Experiments." arXiv Pre-Print.
<https://arxiv.org/abs/1710.02926v2>.

Bell, Robert M, and Daniel F McCaffrey. 2002. "Bias Reduction in
Standard Errors for Linear Regression with Multi-Stage Samples." Survey
Methodology 28 (2): 169-82.

Gaure, Simon. 2013. "OLS with multiple high dimensional category
variables." Computational Statistics & Data Analysis 66: 8-1.
[doi:10.1016/j.csda.2013.03.024](https://doi.org/10.1016/j.csda.2013.03.024)

Halperin, I. 1962. "The product of projection operators." Acta
Scientiarum Mathematicarum (Szeged) 23(1-2): 96-99.

MacKinnon, James, and Halbert White. 1985. "Some
Heteroskedasticity-Consistent Covariance Matrix Estimators with Improved
Finite Sample Properties." Journal of Econometrics 29 (3): 305-25.
[doi:10.1016/0304-4076(85)90158-7](https://doi.org/10.1016/0304-4076%2885%2990158-7)
.

Pustejovsky, James E, and Elizabeth Tipton. 2016. "Small Sample Methods
for Cluster-Robust Variance Estimation and Hypothesis Testing in Fixed
Effects Models." Journal of Business & Economic Statistics. Taylor &
Francis.
[doi:10.1080/07350015.2016.1247004](https://doi.org/10.1080/07350015.2016.1247004)
.

Samii, Cyrus, and Peter M Aronow. 2012. "On Equivalencies Between
Design-Based and Regression-Based Variance Estimators for Randomized
Experiments." Statistics and Probability Letters 82 (2).
[doi:10.1016/j.spl.2011.10.024](https://doi.org/10.1016/j.spl.2011.10.024)
.

## Examples

``` r
set.seed(15)
library(fabricatr)
dat <- fabricate(
  N = 40,
  y = rpois(N, lambda = 4),
  x = rnorm(N),
  z = rbinom(N, 1, prob = 0.4)
)

# Default variance estimator is HC2 robust standard errors
lmro <- lm_robust(y ~ x + z, data = dat)

# Can tidy() the data in to a data.frame
tidy(lmro)
#>          term   estimate std.error  statistic      p.value   conf.low conf.high
#> 1 (Intercept)  4.5970879 0.4386909 10.4791036 1.261673e-12  3.7082156 5.4859602
#> 2           x  0.1735281 0.3813973  0.4549799 6.517822e-01 -0.5992562 0.9463125
#> 3           z -0.9016607 0.5423749 -1.6624308 1.048766e-01 -2.0006166 0.1972952
#>   df outcome
#> 1 37       y
#> 2 37       y
#> 3 37       y
# Can use summary() to get more statistics
summary(lmro)
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
# Can also get coefficients three ways
lmro$coefficients
#> (Intercept)           x           z 
#>   4.5970879   0.1735281  -0.9016607 
coef(lmro)
#> (Intercept)           x           z 
#>   4.5970879   0.1735281  -0.9016607 
tidy(lmro)$estimate
#> [1]  4.5970879  0.1735281 -0.9016607
# Can also get confidence intervals from object or with new 1 - `alpha`
lmro$conf.low
#> (Intercept)           x           z 
#>   3.7082156  -0.5992562  -2.0006166 
confint(lmro, level = 0.8)
#>                   10 %       90 %
#> (Intercept)  4.0246601  5.1695157
#> x           -0.3241398  0.6711961
#> z           -1.6093809 -0.1939405

# Can recover classical standard errors
lmclassic <- lm_robust(y ~ x + z, data = dat, se_type = "classical")
tidy(lmclassic)
#>          term   estimate std.error  statistic      p.value   conf.low conf.high
#> 1 (Intercept)  4.5970879 0.3802497 12.0896557 2.044110e-14  3.8266288 5.3675470
#> 2           x  0.1735281 0.3380391  0.5133374 6.107671e-01 -0.5114042 0.8584604
#> 3           z -0.9016607 0.6184263 -1.4579922 1.532792e-01 -2.1547114 0.3513900
#>   df outcome
#> 1 37       y
#> 2 37       y
#> 3 37       y

# Can easily match Stata's robust standard errors
lmstata <- lm_robust(y ~ x + z, data = dat, se_type = "stata")
tidy(lmstata)
#>          term   estimate std.error  statistic      p.value   conf.low conf.high
#> 1 (Intercept)  4.5970879 0.4398377 10.4517829 1.356971e-12  3.7058921 5.4882837
#> 2           x  0.1735281 0.3731076  0.4650887 6.445936e-01 -0.5824596 0.9295159
#> 3           z -0.9016607 0.5377414 -1.6767553 1.020208e-01 -1.9912282 0.1879068
#>   df outcome
#> 1 37       y
#> 2 37       y
#> 3 37       y

# Easy to specify clusters for cluster-robust inference
dat$clusterID <- sample(1:10, size = 40, replace = TRUE)

lmclust <- lm_robust(y ~ x + z, data = dat, clusters = clusterID)
tidy(lmclust)
#>          term   estimate std.error  statistic      p.value   conf.low
#> 1 (Intercept)  4.5970879 0.3915230 11.7415521 7.616489e-05  3.5922426
#> 2           x  0.1735281 0.3018399  0.5749012 5.896636e-01 -0.5966033
#> 3           z -0.9016607 0.4089681 -2.2047214 6.507872e-02 -1.8780217
#>    conf.high       df outcome
#> 1 5.60193322 5.026547       y
#> 2 0.94365951 5.127624       y
#> 3 0.07470026 6.685305       y

# Can also match Stata's clustered standard errors
lm_robust(
  y ~ x + z,
  data = dat,
  clusters = clusterID,
  se_type = "stata"
)
#>               Estimate Std. Error    t value     Pr(>|t|)   CI Lower   CI Upper
#> (Intercept)  4.5970879  0.3942711 11.6597139 2.668545e-06  3.6878972 5.50627862
#> x            0.1735281  0.3017859  0.5750041 5.810951e-01 -0.5223914 0.86944760
#> z           -0.9016607  0.4204547 -2.1444897 6.432503e-02 -1.8712309 0.06790948
#>             DF
#> (Intercept)  8
#> x            8
#> z            8

# Works just as LM does with functions in the formula
dat$blockID <- rep(c("A", "B", "C", "D"), each = 10)

lm_robust(y ~ x + z + factor(blockID), data = dat)
#>                    Estimate Std. Error    t value     Pr(>|t|)   CI Lower
#> (Intercept)       5.0761464  0.7420501  6.8407062 7.125209e-08  3.5681192
#> x                 0.1905284  0.3731326  0.5106186 6.129177e-01 -0.5677682
#> z                -0.6517671  0.5811381 -1.1215355 2.699187e-01 -1.8327818
#> factor(blockID)B -0.1538306  0.9367502 -0.1642173 8.705325e-01 -2.0575361
#> factor(blockID)C -1.0519650  0.9520788 -1.1049138 2.769549e-01 -2.9868220
#> factor(blockID)D -1.0871302  0.8479584 -1.2820561 2.084963e-01 -2.8103891
#>                   CI Upper DF
#> (Intercept)      6.5841735 34
#> x                0.9488250 34
#> z                0.5292477 34
#> factor(blockID)B 1.7498750 34
#> factor(blockID)C 0.8828920 34
#> factor(blockID)D 0.6361286 34

# Weights are also easily specified
dat$w <- runif(40)

lm_robust(
  y ~ x + z,
  data = dat,
  weights = w,
  clusters = clusterID
)
#>               Estimate Std. Error   t value     Pr(>|t|)   CI Lower  CI Upper
#> (Intercept)  4.5913785  0.4910753  9.349642 0.0003289342  3.3038538 5.8789032
#> x            0.4028538  0.2133290  1.888416 0.1475001559 -0.2444348 1.0501424
#> z           -1.0782068  0.5341716 -2.018465 0.1086801430 -2.5205693 0.3641556
#>                   DF
#> (Intercept) 4.693802
#> x           3.279816
#> z           4.306669

# Subsetting works just as in `lm()`
lm_robust(y ~ x, data = dat, subset = z == 1)
#>              Estimate Std. Error   t value     Pr(>|t|)  CI Lower  CI Upper DF
#> (Intercept)  4.026354  0.3785824 10.635346 1.833544e-07  3.201494 4.8512143 12
#> x           -0.462500  0.3599288 -1.284976 2.230493e-01 -1.246718 0.3217174 12

# One can also choose to set the significance level for different CIs
lm_robust(y ~ x + z, data = dat, alpha = 0.1)
#>               Estimate Std. Error    t value     Pr(>|t|)   CI Lower   CI Upper
#> (Intercept)  4.5970879  0.4386909 10.4791036 1.261673e-12  3.8569752 5.33720059
#> x            0.1735281  0.3813973  0.4549799 6.517822e-01 -0.4699248 0.81698110
#> z           -0.9016607  0.5423749 -1.6624308 1.048766e-01 -1.8166979 0.01337647
#>             DF
#> (Intercept) 37
#> x           37
#> z           37

# We can also specify fixed effects
# Speed gains with fixed effects are greatest with "stata" or "HC1" std.errors
tidy(lm_robust(y ~ z, data = dat, fixed_effects = ~ blockID, se_type = "HC1"))
#>   term   estimate std.error statistic   p.value  conf.low conf.high df outcome
#> 1    z -0.5857143 0.5551326 -1.055089 0.2986138 -1.712693 0.5412649 35       y

if (FALSE) { # \dontrun{
  # Can also use 'margins' or 'emmeans' package if you have them installed
  # to get marginal effects
  library(margins)
  lmrout <- lm_robust(y ~ x + z, data = dat)
  summary(margins(lmrout))

  # Can output results using 'texreg'
  library(texreg)
  texreg(lmrout)

  # Using emmeans to obtain covariate-adjusted means
  library(emmeans)
  fiber.rlm <- lm_robust(strength ~ diameter + machine, data = fiber)
  emmeans(fiber.rlm, "machine")
} # }
```
