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

  A length 1 character string or a matrix specifying combination, to be
  passed to the hypothesis.matrix argument of car::linearHypothesis.
  Joint hypotheses are currently not handled by lh_robust. See
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

The only analysis directly performed by `lh_robust` is a `t-test` for
the null hypothesis of no effects of the linear combination of
coefficients as specified by the user. All other output components are
either extracted from `linearHypothesis` or `lm_robust`. Note that the
estimate returned is the value of the LHS of an equation of the form
f(X) = 0. Hyptheses "x - z = 1", "x +1= z + 2" and "x-z-1=0" will all
return the value for "x-y-1"

The original output returned by `linearHypothesis` is added as an
attribute under the `"linear_hypothesis"` attribute.

## Details

This function is a wrapper for
[`lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
and for
[`linearHypothesis`](https://rdrr.io/pkg/car/man/linearHypothesis.html).
It first runs `lm_robust` and next passes `"lm_robust"` object as an
argument to `linearHypothesis`. Currently CR2 standard errors are not
handled by lh_robust.

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
#>               Estimate Std. Error   t value     Pr(>|t|)   CI Lower  CI Upper
#> (Intercept)  4.4094685  0.4557957  9.674221 1.122813e-11  3.4859387 5.3329984
#> x            0.3619881  0.3264857  1.108741 2.746970e-01 -0.2995348 1.0235109
#> z           -0.9191800  0.6702807 -1.371336 1.785304e-01 -2.2772976 0.4389376
#>             DF
#> (Intercept) 37
#> x           37
#> z           37
#> 
#> $lh
#>        Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> z = 2x   -1.643     0.8839  -1.859  0.07101   -3.434   0.1479 37
#> 
lh_robust(y ~ x + z, data = dat, linear_hypothesis = "2*x +1*z")
#> $lm_robust
#>               Estimate Std. Error   t value     Pr(>|t|)   CI Lower  CI Upper
#> (Intercept)  4.4094685  0.4557957  9.674221 1.122813e-11  3.4859387 5.3329984
#> x            0.3619881  0.3264857  1.108741 2.746970e-01 -0.2995348 1.0235109
#> z           -0.9191800  0.6702807 -1.371336 1.785304e-01 -2.2772976 0.4389376
#>             DF
#> (Intercept) 37
#> x           37
#> z           37
#> 
#> $lh
#>          Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> 2*x +1*z  -0.1952     0.9849 -0.1982    0.844   -2.191      1.8 37
#> 
lh_robust(y ~ x + z, data = dat, linear_hypothesis = "z + 2x = 0")
#> $lm_robust
#>               Estimate Std. Error   t value     Pr(>|t|)   CI Lower  CI Upper
#> (Intercept)  4.4094685  0.4557957  9.674221 1.122813e-11  3.4859387 5.3329984
#> x            0.3619881  0.3264857  1.108741 2.746970e-01 -0.2995348 1.0235109
#> z           -0.9191800  0.6702807 -1.371336 1.785304e-01 -2.2772976 0.4389376
#>             DF
#> (Intercept) 37
#> x           37
#> z           37
#> 
#> $lh
#>            Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> z + 2x = 0  -0.1952     0.9849 -0.1982    0.844   -2.191      1.8 37
#> 

# Also recovers other sorts of standard erorrs just as specified in \code{\link{lm_robust}}
lh_robust(y ~ x + z, data = dat, linear_hypothesis = "z + 2x = 0", se_type = "classical")
#> $lm_robust
#>               Estimate Std. Error    t value     Pr(>|t|)   CI Lower  CI Upper
#> (Intercept)  4.4094685  0.4426777  9.9608997 5.104664e-12  3.5125182 5.3064188
#> x            0.3619881  0.3623107  0.9991094 3.242313e-01 -0.3721232 1.0960993
#> z           -0.9191800  0.6992058 -1.3146057 1.967333e-01 -2.3359056 0.4975456
#>             DF
#> (Intercept) 37
#> x           37
#> z           37
#> 
#> $lh
#>            Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> z + 2x = 0  -0.1952     0.9746 -0.2003   0.8424    -2.17     1.78 37
#> 
lh_robust(y ~ x + z, data = dat, linear_hypothesis = "z + 2x = 0", se_type =  "HC1")
#> $lm_robust
#>               Estimate Std. Error   t value     Pr(>|t|)   CI Lower  CI Upper
#> (Intercept)  4.4094685  0.4575107  9.637957 1.241489e-11  3.4824638 5.3364733
#> x            0.3619881  0.3218063  1.124863 2.678914e-01 -0.2900534 1.0140295
#> z           -0.9191800  0.6715469 -1.368750 1.793307e-01 -2.2798633 0.4415034
#>             DF
#> (Intercept) 37
#> x           37
#> z           37
#> 
#> $lh
#>            Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> z + 2x = 0  -0.1952     0.9848 -0.1982    0.844   -2.191      1.8 37
#> 

#  Can tidy() main output and subcomponents in to a data.frame
lhro <- lh_robust(y ~ x + z, data = dat, linear_hypothesis = "z + 2x = 0")
tidy(lhro )
#>          term   estimate std.error statistic      p.value   conf.low conf.high
#> 1 (Intercept)  4.4094685 0.4557957  9.674221 1.122813e-11  3.4859387 5.3329984
#> 2           x  0.3619881 0.3264857  1.108741 2.746970e-01 -0.2995348 1.0235109
#> 3           z -0.9191800 0.6702807 -1.371336 1.785304e-01 -2.2772976 0.4389376
#> 4  z + 2x = 0 -0.1952039 0.9848583 -0.198205 8.439697e-01 -2.1907164 1.8003086
#>   df outcome
#> 1 37       y
#> 2 37       y
#> 3 37       y
#> 4 37       y
tidy(lhro$lm_robust)
#>          term   estimate std.error statistic      p.value   conf.low conf.high
#> 1 (Intercept)  4.4094685 0.4557957  9.674221 1.122813e-11  3.4859387 5.3329984
#> 2           x  0.3619881 0.3264857  1.108741 2.746970e-01 -0.2995348 1.0235109
#> 3           z -0.9191800 0.6702807 -1.371336 1.785304e-01 -2.2772976 0.4389376
#>   df outcome
#> 1 37       y
#> 2 37       y
#> 3 37       y
tidy(lhro$lh)
#>         term   estimate std.error statistic   p.value  conf.low conf.high df
#> 1 z + 2x = 0 -0.1952039 0.9848583 -0.198205 0.8439697 -2.190716  1.800309 37
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
#>             Estimate Std. Error t value  Pr(>|t|) CI Lower CI Upper DF
#> (Intercept)   4.4095     0.4558   9.674 1.123e-11   3.4859   5.3330 37
#> x             0.3620     0.3265   1.109 2.747e-01  -0.2995   1.0235 37
#> z            -0.9192     0.6703  -1.371 1.785e-01  -2.2773   0.4389 37
#> 
#> Multiple R-squared:  0.06496 ,   Adjusted R-squared:  0.01442 
#> F-statistic: 1.739 on 2 and 37 DF,  p-value: 0.1898
#> 
#> $lh
#>            Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> z + 2x = 0  -0.1952     0.9849 -0.1982    0.844   -2.191      1.8 37
#> 
summary(lhro$lm_robust)
#> 
#> Call:
#> lh_robust(y ~ x + z, data = dat, linear_hypothesis = "z + 2x = 0")
#> 
#> Standard error type:  HC2 
#> 
#> Coefficients:
#>             Estimate Std. Error t value  Pr(>|t|) CI Lower CI Upper DF
#> (Intercept)   4.4095     0.4558   9.674 1.123e-11   3.4859   5.3330 37
#> x             0.3620     0.3265   1.109 2.747e-01  -0.2995   1.0235 37
#> z            -0.9192     0.6703  -1.371 1.785e-01  -2.2773   0.4389 37
#> 
#> Multiple R-squared:  0.06496 ,   Adjusted R-squared:  0.01442 
#> F-statistic: 1.739 on 2 and 37 DF,  p-value: 0.1898
summary(lhro$lh)
#>            Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> z + 2x = 0  -0.1952     0.9849 -0.1982    0.844   -2.191      1.8 37
```
