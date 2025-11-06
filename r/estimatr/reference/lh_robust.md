# Linear Hypothesis for Ordinary Least Squares with Robust Standard Errors

This function fits a linear model with robust standard errors and
performs linear hypothesis test.

## Usage

``` r
lh_robust(..., data, linear_hypothesis)
```

## Arguments

- ...:

  Other arguments to be passed to
  [`lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)

- data:

  A `data.frame`

- linear_hypothesis:

  A character string or a matrix specifying combination, to be passed to
  the hypothesis.matrix argument of car::linearHypothesis See
  [`linearHypothesis`](https://rdrr.io/pkg/car/man/linearHypothesis.html)
  for more details.

## Value

An object of class `"lh_robust"` containing the two following
components:

- lm_robust:

  an object as returned by `lm_robust`.

- lh:

  A data frame with most of its columns pulled from `linearHypothesis`'
  output.

The only analyis directly performed by `lh_robust` is a `t-test` for the
null hypothesis of no effects of the linear combination of coefficients
as specified by the user. All other output components are either
extracted from `linearHypothesis` or `lm_robust`.

The original output returned by `linearHypothesis` is added as an
attribute under the `"linear_hypothesis"` attribute.

## Details

This function is a wrapper for
[`lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
and for
[`linearHypothesis`](https://rdrr.io/pkg/car/man/linearHypothesis.html).
It first runs `lm_robust` and next passes `"lm_robust"` object as an
argument to `linearHypothesis`.

## Examples

``` r
library(fabricatr)
dat <- fabricate(
  N = 40,
  y = rpois(N, lambda = 4),
  x = rnorm(N),
  z = rbinom(N, 1, prob = 0.4),
  clusterID = sample(1:4, 40, replace = TRUE)
)

# Default variance estimator is HC2 robust standard errors
lhro <- lh_robust(y ~ x + z, data = dat, linear_hypothesis = "z + 2x = 0")
#> Loading required namespace: car

# The linear hypothesis argument can be specified equivalently as:
lh_robust(y ~ x + z, data = dat, linear_hypothesis = "z = 2x")
#> $lm_robust
#>                 Estimate Std. Error      t value     Pr(>|t|)   CI Lower
#> (Intercept)  4.333058710  0.4694105  9.230851151 3.880146e-11  3.3819426
#> x            0.002081762  0.3186740  0.006532578 9.948229e-01 -0.6436131
#> z           -0.457422662  0.7431683 -0.615503477 5.419900e-01 -1.9632246
#>              CI Upper DF
#> (Intercept) 5.2841748 37
#> x           0.6477766 37
#> z           1.0483793 37
#> 
#> $lh
#>        Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> z = 2x  -0.4616     0.8228  -0.561   0.5782   -2.129    1.205 37
#> 
lh_robust(y ~ x + z, data = dat, linear_hypothesis = "2*x +1*z")
#> $lm_robust
#>                 Estimate Std. Error      t value     Pr(>|t|)   CI Lower
#> (Intercept)  4.333058710  0.4694105  9.230851151 3.880146e-11  3.3819426
#> x            0.002081762  0.3186740  0.006532578 9.948229e-01 -0.6436131
#> z           -0.457422662  0.7431683 -0.615503477 5.419900e-01 -1.9632246
#>              CI Upper DF
#> (Intercept) 5.2841748 37
#> x           0.6477766 37
#> z           1.0483793 37
#> 
#> $lh
#>          Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> 2*x +1*z  -0.4533      1.114  -0.407   0.6863    -2.71    1.803 37
#> 
lh_robust(y ~ x + z, data = dat, linear_hypothesis = "z + 2x = 0")
#> $lm_robust
#>                 Estimate Std. Error      t value     Pr(>|t|)   CI Lower
#> (Intercept)  4.333058710  0.4694105  9.230851151 3.880146e-11  3.3819426
#> x            0.002081762  0.3186740  0.006532578 9.948229e-01 -0.6436131
#> z           -0.457422662  0.7431683 -0.615503477 5.419900e-01 -1.9632246
#>              CI Upper DF
#> (Intercept) 5.2841748 37
#> x           0.6477766 37
#> z           1.0483793 37
#> 
#> $lh
#>            Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> z + 2x = 0  -0.4533      1.114  -0.407   0.6863    -2.71    1.803 37
#> 

# Also recovers other sorts of standard erorrs just as specified in \code{\link{lm_robust}}
lh_robust(y ~ x + z, data = dat, linear_hypothesis = "z + 2x = 0", se_type = "classical")
#> $lm_robust
#>                 Estimate Std. Error      t value     Pr(>|t|)   CI Lower
#> (Intercept)  4.333058710  0.4592151  9.435792886 2.180476e-11  3.4026005
#> x            0.002081762  0.3753277  0.005546519 9.956043e-01 -0.7584045
#> z           -0.457422662  0.7402872 -0.617898901 5.404269e-01 -1.9573871
#>             CI Upper DF
#> (Intercept) 5.263517 37
#> x           0.762568 37
#> z           1.042542 37
#> 
#> $lh
#>            Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> z + 2x = 0  -0.4533      1.165  -0.389   0.6995   -2.814    1.908 37
#> 
lh_robust(y ~ x + z, data = dat, linear_hypothesis = "z + 2x = 0", se_type =  "HC1")
#> $lm_robust
#>                 Estimate Std. Error      t value     Pr(>|t|)   CI Lower
#> (Intercept)  4.333058710  0.4735142  9.150852036 4.866117e-11  3.3736277
#> x            0.002081762  0.3106024  0.006702339 9.946884e-01 -0.6272584
#> z           -0.457422662  0.7405962 -0.617641142 5.405950e-01 -1.9580130
#>             CI Upper DF
#> (Intercept) 5.292490 37
#> x           0.631422 37
#> z           1.043168 37
#> 
#> $lh
#>            Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> z + 2x = 0  -0.4533      1.091 -0.4155   0.6802   -2.664    1.757 37
#> 

#  Can tidy() main output and subcomponents in to a data.frame
lhro <- lh_robust(y ~ x + z, data = dat, linear_hypothesis = "z + 2x = 0")
tidy(lhro )
#>          term     estimate std.error    statistic      p.value   conf.low
#> 1 (Intercept)  4.333058710 0.4694105  9.230851151 3.880146e-11  3.3819426
#> 2           x  0.002081762 0.3186740  0.006532578 9.948229e-01 -0.6436131
#> 3           z -0.457422662 0.7431683 -0.615503477 5.419900e-01 -1.9632246
#> 4  z + 2x = 0 -0.453259137 1.1135908 -0.407024841 6.863349e-01 -2.7096085
#>   conf.high df outcome
#> 1 5.2841748 37       y
#> 2 0.6477766 37       y
#> 3 1.0483793 37       y
#> 4 1.8030902 37       y
tidy(lhro$lm_robust)
#>          term     estimate std.error    statistic      p.value   conf.low
#> 1 (Intercept)  4.333058710 0.4694105  9.230851151 3.880146e-11  3.3819426
#> 2           x  0.002081762 0.3186740  0.006532578 9.948229e-01 -0.6436131
#> 3           z -0.457422662 0.7431683 -0.615503477 5.419900e-01 -1.9632246
#>   conf.high df outcome
#> 1 5.2841748 37       y
#> 2 0.6477766 37       y
#> 3 1.0483793 37       y
tidy(lhro$lh)
#>         term   estimate std.error  statistic   p.value  conf.low conf.high df
#> 1 z + 2x = 0 -0.4532591  1.113591 -0.4070248 0.6863349 -2.709609   1.80309 37
#>   outcome
#> 1       y

# Can use summary() to get more statistics on the main output and subcomponents.
summary(lhro)
#> $lm_robust
#> 
#> Call:
#> lh_robust(y ~ x + z, data = dat, linear_hypothesis = "z + 2x = 0")
#> 
#> Standard error type:  HC2 
#> 
#> Coefficients:
#>              Estimate Std. Error   t value  Pr(>|t|) CI Lower CI Upper DF
#> (Intercept)  4.333059     0.4694  9.230851 3.880e-11   3.3819   5.2842 37
#> x            0.002082     0.3187  0.006533 9.948e-01  -0.6436   0.6478 37
#> z           -0.457423     0.7432 -0.615503 5.420e-01  -1.9632   1.0484 37
#> 
#> Multiple R-squared:  0.01078 ,   Adjusted R-squared:  -0.04269 
#> F-statistic: 0.2091 on 2 and 37 DF,  p-value: 0.8123
#> 
#> $lh
#>            Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> z + 2x = 0  -0.4533      1.114  -0.407   0.6863    -2.71    1.803 37
#> 
summary(lhro$lm_robust)
#> 
#> Call:
#> lh_robust(y ~ x + z, data = dat, linear_hypothesis = "z + 2x = 0")
#> 
#> Standard error type:  HC2 
#> 
#> Coefficients:
#>              Estimate Std. Error   t value  Pr(>|t|) CI Lower CI Upper DF
#> (Intercept)  4.333059     0.4694  9.230851 3.880e-11   3.3819   5.2842 37
#> x            0.002082     0.3187  0.006533 9.948e-01  -0.6436   0.6478 37
#> z           -0.457423     0.7432 -0.615503 5.420e-01  -1.9632   1.0484 37
#> 
#> Multiple R-squared:  0.01078 ,   Adjusted R-squared:  -0.04269 
#> F-statistic: 0.2091 on 2 and 37 DF,  p-value: 0.8123
summary(lhro$lh)
#>            Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> z + 2x = 0  -0.4533      1.114  -0.407   0.6863    -2.71    1.803 37
```
