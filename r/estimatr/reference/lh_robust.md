# Linear Hypothesis Test for OLS with Robust Standard Errors

Tests a linear combination of coefficients, or several of them jointly,
from a model fitted by
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md).
The robust variance and the degrees of freedom of the fit are carried
through, so a clustered fit is tested on its cluster-adjusted degrees of
freedom rather than on the residual ones.

## Usage

``` r
lh_robust(..., data, linear_hypothesis)
```

## Arguments

- ...:

  (optional) Other arguments passed to
  [`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)

- data:

  (optional) A `data.frame`

- linear_hypothesis:

  (required) A character string or matrix specifying the hypothesis,
  passed to
  [`car::linearHypothesis`](https://rdrr.io/pkg/car/man/linearHypothesis.html)

## Value

An object of class `"lh_robust"` with three components: `lm_robust`, the
underlying fit; `lh`, one row per hypothesis holding `coefficients`,
`std.error`, `statistic`, `p.value`, `alpha`, `conf.low`, `conf.high`,
`df`, `term`, and `outcome`; and `joint_hypothesis`, the Wald F test of
all of them at once, as `value`, `numdf`, `dendf`, and `p.value`. Under
`se_type = "CR2"` each hypothesis's `df` is its own Satterthwaite
approximation, as
[`clubSandwich::linear_contrast()`](http://jepusto.github.io/clubSandwich/reference/linear_contrast.md)
computes it, and `dendf` is the smallest of them.

## Examples

``` r
set.seed(35)
dat <- data.frame(x = rnorm(100), z = rbinom(100, 1, 0.5),
                  cl = rep(1:10, each = 10))
dat$y <- dat$x + 0.5 * dat$z + rnorm(100)

# One linear combination of coefficients
fit <- lh_robust(y ~ x + z, data = dat, linear_hypothesis = "z + 2*x = 0")
#> Loading required namespace: car
fit
#> $lm_robust
#>              Estimate Std. Error    t value     Pr(>|t|)    CI Lower  CI Upper
#> (Intercept) 0.2722647  0.1637490  1.6626952 9.960162e-02 -0.05273177 0.5972612
#> x           1.0700945  0.0869757 12.3033728 1.614593e-21  0.89747180 1.2427172
#> z           0.1388867  0.1977131  0.7024662 4.840708e-01 -0.25351895 0.5312924
#>             DF
#> (Intercept) 97
#> x           97
#> z           97
#> 
#> $lh
#>             Estimate Std. Error t value  Pr(>|t|) CI Lower CI Upper DF
#> z + 2*x = 0    2.279     0.2573   8.857 3.934e-14    1.768     2.79 97
#> 
#> $joint_hypothesis
#>        value        numdf        dendf      p.value 
#> 7.844270e+01 1.000000e+00 9.700000e+01 3.933918e-14 
#> 
tidy(fit)
#> # A tibble: 4 × 9
#>   term    estimate std.error statistic  p.value conf.low conf.high    df outcome
#>   <chr>      <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 (Inter…    0.272    0.164      1.66  9.96e- 2  -0.0527     0.597    97 y      
#> 2 x          1.07     0.0870    12.3   1.61e-21   0.897      1.24     97 y      
#> 3 z          0.139    0.198      0.702 4.84e- 1  -0.254      0.531    97 y      
#> 4 z + 2*…    2.28     0.257      8.86  3.93e-14   1.77       2.79     97 y      

# Degrees of freedom follow the fit, so a clustered model tests against the
# cluster-adjusted df rather than the residual df
lh_robust(y ~ x + z, data = dat, clusters = cl,
          linear_hypothesis = "z + 2*x = 0")
#> $lm_robust
#>              Estimate Std. Error  t value     Pr(>|t|)   CI Lower  CI Upper
#> (Intercept) 0.2722647  0.1085927 2.507210 3.412609e-02  0.0255870 0.5189425
#> x           1.0700945  0.1127933 9.487217 1.503296e-05  0.8087622 1.3314268
#> z           0.1388867  0.1107408 1.254161 2.414684e-01 -0.1117398 0.3895132
#>                   DF
#> (Intercept) 8.761312
#> x           7.789178
#> z           8.973340
#> 
#> $lh
#>             Estimate Std. Error t value  Pr(>|t|) CI Lower CI Upper    DF
#> z + 2*x = 0    2.279     0.2497   9.128 1.347e-05    1.707    2.852 8.267
#> 
#> $joint_hypothesis
#>        value        numdf        dendf      p.value 
#> 8.332170e+01 1.000000e+00 8.266568e+00 1.346860e-05 
#> 

# Several restrictions at once give one joint Wald test as well
joint <- lh_robust(y ~ x + z, data = dat, linear_hypothesis = c("x = 0", "z = 0"))
joint$joint_hypothesis
#>        value        numdf        dendf      p.value 
#> 7.648561e+01 2.000000e+00 9.700000e+01 1.150176e-20 
```
