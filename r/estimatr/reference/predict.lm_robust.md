# Predict method for `lm_robust` object

Produces predicted values, obtained by evaluating the regression
function in the frame `newdata` for fits from
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
and
[`lm_lin()`](https://declaredesign.org/r/estimatr/reference/lm_lin.md).
If `se.fit` is `TRUE`, standard errors of the predictions are
calculated. Setting `interval` adds confidence or prediction (tolerance)
intervals at the level set by `alpha`, sometimes called narrow and wide
intervals respectively.

## Usage

``` r
# S3 method for class 'lm_robust'
predict(
  object,
  newdata,
  se.fit = FALSE,
  interval = c("none", "confidence", "prediction"),
  alpha = 0.05,
  na.action = na.pass,
  pred.var = NULL,
  weights,
  ...
)
```

## Arguments

- object:

  An object of class `"lm_robust"`.

- newdata:

  A data frame in which to look for the variables to predict from. If
  omitted, the fitted values are returned.

- se.fit:

  Logical. Whether to return standard errors. `FALSE` by default.

- interval:

  Type of interval calculation, which can be abbreviated. `"none"` by
  default.

- alpha:

  Numeric. The test size for confidence intervals.

- na.action:

  Function determining what to do with missing values in `newdata`. The
  default is to predict `NA`.

- pred.var:

  The variance(s) to assume for future observations when building
  prediction intervals.

- weights:

  Variance weights for prediction, either a numeric vector or the bare
  (unquoted) name of the weights variable in `newdata`.

- ...:

  (optional) Ignored.

## Value

A numeric vector of predictions, or a data frame with the predictions
and their standard errors and interval bounds when `se.fit` or
`interval` is set.

## Details

Called without `newdata`, the method returns the in-sample fitted
values, and neither `se.fit` nor `interval` is available.

The equation used for the standard error of a prediction given a row of
data \\x\\ is:

\\\sqrt(x \Sigma x')\\,

where \\\Sigma\\ is the estimated variance-covariance matrix from
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md).

The prediction intervals are for a single observation at each case in
`newdata` with error variance(s) `pred.var`. The default is to assume
that future observations have the same error variance as those used for
fitting, which is taken from the fitted
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
object. If `weights` is supplied, the inverse of those weights scales
the variance. If the fit was weighted, the default is to assume constant
prediction variance, with a warning.

## See also

[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md),
[`lm_lin()`](https://declaredesign.org/r/estimatr/reference/lm_lin.md)

## Examples

``` r

# Set seed
set.seed(42)

# Simulate data
n <- 10
dat <- data.frame(y = rnorm(n), x = rnorm(n))

# Fit lm
lm_out <- lm_robust(y ~ x, data = dat)
# In-sample fitted values
predict(lm_out)
#>          1          2          3          4          5          6          7 
#> 0.26523246 0.07663413 0.78269588 0.56945196 0.54150778 0.39373130 0.57050162 
#>          8          9         10 
#> 1.02619980 0.98470859 0.26230416 
# Get predicted fits
fits <- predict(lm_out, newdata = dat)
# With standard errors and confidence intervals
fits <- predict(lm_out, newdata = dat, se.fit = TRUE, interval = "confidence")

# Use new data as well
new_dat <- data.frame(x = runif(n, 5, 8))
predict(lm_out, newdata = new_dat)
#>          1          2          3          4          5          6          7 
#> -0.6633382 -0.6957332 -0.4661705 -1.0056478 -0.6934163 -0.9964482 -0.9562101 
#>          8          9         10 
#> -0.8134173 -1.0041648 -0.8012341 

# You can also supply custom variance weights for prediction intervals
new_dat$w <- runif(n)
predict(lm_out, newdata = new_dat, weights = w, interval = "prediction")
#> $fit
#>              fit        lwr      upr
#>  [1,] -0.6633382  -5.162176 3.835499
#>  [2,] -0.6957332  -5.207346 3.815879
#>  [3,] -0.4661705  -4.414089 3.481748
#>  [4,] -1.0056478  -5.520872 3.509577
#>  [5,] -0.6934163 -10.799452 9.412619
#>  [6,] -0.9964482  -5.514685 3.521789
#>  [7,] -0.9562101  -5.438922 3.526502
#>  [8,] -0.8134173  -6.558348 4.931514
#>  [9,] -1.0041648  -6.438104 4.429774
#> [10,] -0.8012341  -5.137823 3.535355
#> 

# Works for 'lm_lin' models as well
dat$z <- sample(1:3, size = nrow(dat), replace = TRUE)
lmlin_out1 <- lm_lin(y ~ z, covariates = ~ x, data = dat)
#> Warning: 1 of 5 variance estimates came out negative, so those standard errors are NaN. The design is close to singular, and the sandwich estimator loses the difference between two nearly equal quantities to rounding. Drop covariates, or use `se_type = "classical"`.
#> Warning: 3 observations have a computed leverage at or near 1, which happens when the design is close to saturated and the observation is fitted exactly or nearly so. `se_type = "HC2"` divides by (1 - leverage). An observation at or above leverage 1 is dropped from the variance rather than divided by a negative number, and one just below it contributes a term the small divisor inflates. Use `se_type = "HC1"` or `"classical"`, or drop covariates, to use every observation.
#> Warning: Some coefficients are collinear with other regressors and were dropped, and are returned as NA: z3:x_c.
predict(lmlin_out1, newdata = dat, interval = "prediction")
#> $fit
#>                fit        lwr      upr
#>  [1,]  1.370958447  0.3615444 2.380372
#>  [2,] -0.186937884 -1.4647357 1.090860
#>  [3,]  0.239186411 -1.0086955 1.487068
#>  [4,]  0.110488863 -0.9949579 1.215936
#>  [5,]  0.093623919 -1.0022995 1.189547
#>  [6,]  0.004437459 -1.0812175 1.090092
#>  [7,]  1.511521997  0.5021080 2.520936
#>  [8,]  0.386146556 -1.1366369 1.908930
#>  [9,]  2.018423714  1.0090097 3.027838
#> [10,] -0.074881809 -1.2079017 1.058138
#> 
```
