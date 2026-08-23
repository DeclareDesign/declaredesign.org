# Predict method for `lm_robust` object

Predict method for `lm_robust` object

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

  an object of class 'lm_robust'

- newdata:

  a data frame in which to look for variables with which to predict

- se.fit:

  logical. Whether standard errors are required, default = FALSE

- interval:

  type of interval calculation. Can be abbreviated, default = none

- alpha:

  numeric denoting the test size for confidence intervals

- na.action:

  function determining what should be done with missing values in
  newdata. The default is to predict NA.

- pred.var:

  the variance(s) for future observations to be assumed for prediction
  intervals.

- weights:

  variance weights for prediction. This can be a numeric vector or a
  bare (unquoted) name of the weights variable in the supplied newdata.

- ...:

  other arguments, unused

## Details

Produces predicted values, obtained by evaluating the regression
function in the frame `newdata` for fits from `lm_robust` and `lm_lin`.
If the logical se.fit is TRUE, standard errors of the predictions are
calculated. Setting intervals specifies computation of confidence or
prediction (tolerance) intervals at the specified level, sometimes
referred to as narrow vs. wide intervals.

The equation used for the standard error of a prediction given a row of
data \\x\\ is:

\\\sqrt(x \Sigma x')\\,

where \\\Sigma\\ is the estimated variance-covariance matrix from
`lm_robust`.

The prediction intervals are for a single observation at each case in
`newdata` with error variance(s) `pred.var`. The the default is to
assume that future observations have the same error variance as those
used for fitting, which is gotten from the fit
[`lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
object. If weights is supplied, the inverse of this is used as a scale
factor. If the fit was weighted, the default is to assume constant
prediction variance, with a warning.

## Examples

``` r

# Set seed
set.seed(42)

# Simulate data
n <- 10
dat <- data.frame(y = rnorm(n), x = rnorm(n))

# Fit lm
lm_out <- lm_robust(y ~ x, data = dat)
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
predict(lmlin_out1, newdata = dat, interval = "prediction")
#> $fit
#>                fit lwr upr
#>  [1,]  1.370958447 NaN NaN
#>  [2,] -0.186937884 NaN NaN
#>  [3,]  0.239186411 NaN NaN
#>  [4,]  0.110488863 NaN NaN
#>  [5,]  0.093623919 NaN NaN
#>  [6,]  0.004437459 NaN NaN
#>  [7,]  1.511521997 NaN NaN
#>  [8,]  0.386146556 NaN NaN
#>  [9,]  2.018423714 NaN NaN
#> [10,] -0.074881809 NaN NaN
#> 

# Predictions from Lin models are equivalent with and without an intercept
# and for multi-level treatments entered as numeric or factor variables
lmlin_out2 <- lm_lin(y ~ z - 1, covariates = ~ x, data = dat)
lmlin_out3 <- lm_lin(y ~ factor(z), covariates = ~ x, data = dat)
lmlin_out4 <- lm_lin(y ~ factor(z) - 1, covariates = ~ x, data = dat)

predict(lmlin_out2, newdata = dat, interval = "prediction")
#> $fit
#>                fit lwr upr
#>  [1,]  1.370958447 NaN NaN
#>  [2,] -0.186937884 NaN NaN
#>  [3,]  0.239186411 NaN NaN
#>  [4,]  0.110488863 NaN NaN
#>  [5,]  0.093623919 NaN NaN
#>  [6,]  0.004437459 NaN NaN
#>  [7,]  1.511521997 NaN NaN
#>  [8,]  0.386146556 NaN NaN
#>  [9,]  2.018423714 NaN NaN
#> [10,] -0.074881809 NaN NaN
#> 
predict(lmlin_out3, newdata = dat, interval = "prediction")
#> $fit
#>                fit lwr upr
#>  [1,]  1.370958447 NaN NaN
#>  [2,] -0.186937884 NaN NaN
#>  [3,]  0.239186411 NaN NaN
#>  [4,]  0.110488863 NaN NaN
#>  [5,]  0.093623919 NaN NaN
#>  [6,]  0.004437459 NaN NaN
#>  [7,]  1.511521997 NaN NaN
#>  [8,]  0.386146556 NaN NaN
#>  [9,]  2.018423714 NaN NaN
#> [10,] -0.074881809 NaN NaN
#> 
predict(lmlin_out4, newdata = dat, interval = "prediction")
#> $fit
#>                fit lwr upr
#>  [1,]  1.370958447 NaN NaN
#>  [2,] -0.186937884 NaN NaN
#>  [3,]  0.239186411 NaN NaN
#>  [4,]  0.110488863 NaN NaN
#>  [5,]  0.093623919 NaN NaN
#>  [6,]  0.004437459 NaN NaN
#>  [7,]  1.511521997 NaN NaN
#>  [8,]  0.386146556 NaN NaN
#>  [9,]  2.018423714 NaN NaN
#> [10,] -0.074881809 NaN NaN
#> 

# In Lin models, predict will stop with an error message if new
# treatment levels are supplied in the new data
new_dat$z <- sample(0:3, size = nrow(new_dat), replace = TRUE)
# predict(lmlin_out, newdata = new_dat)

```
