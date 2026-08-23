# Declare a design

Declare a design

## Usage

``` r
# S3 method for class 'dd'
lhs + rhs
```

## Arguments

- lhs:

  A step in a research design, beginning with a function that defines
  the model. Steps are evaluated sequentially. With the exception of the
  first step, all steps must be functions that take a `data.frame` as an
  argument and return a `data.frame`. Steps are declared using the
  `declare_` functions, i.e.,
  [`declare_model`](https://declaredesign.org/r/declaredesign/reference/declare_model.md),
  [`declare_inquiry`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md),
  [`declare_sampling`](https://declaredesign.org/r/declaredesign/reference/declare_sampling.md),
  [`declare_assignment`](https://declaredesign.org/r/declaredesign/reference/declare_assignment.md),
  [`declare_measurement`](https://declaredesign.org/r/declaredesign/reference/declare_measurement.md),
  [`declare_estimator`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md),
  and
  [`declare_test`](https://declaredesign.org/r/declaredesign/reference/declare_test.md).

- rhs:

  A second step in a research design

## Value

a design

## Examples

``` r
design <-
  declare_model(
    N = 500, 
    U = rnorm(N),
    potential_outcomes(Y ~ Z + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(S = complete_rs(N, n = 250)) +
  declare_assignment(Z = complete_ra(N, m = 25)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE") 

dat <- draw_data(design)
head(dat)
#>    ID          U      Y_Z_0      Y_Z_1 S Z          Y
#> 1 001  1.6662720  1.6662720  2.6662720 1 0  1.6662720
#> 2 002  1.9213911  1.9213911  2.9213911 1 0  1.9213911
#> 3 003 -2.0267236 -2.0267236 -1.0267236 1 0 -2.0267236
#> 4 004 -1.6604176 -1.6604176 -0.6604176 1 0 -1.6604176
#> 5 006  1.8281118  1.8281118  2.8281118 1 0  1.8281118
#> 6 007 -0.2794789 -0.2794789  0.7205211 1 0 -0.2794789

run_design(design)
#>   inquiry estimand estimator term estimate std.error statistic      p.value
#> 1     ATE        1 estimator    Z 1.063682 0.2007292  5.299087 2.570504e-07
#>    conf.low conf.high  df outcome
#> 1 0.6683302  1.459033 248       Y

# You may wish to have a design with only one step:

design <- declare_model(N = 500, noise = rnorm(N)) + NULL

dat <- draw_data(design)
head(dat)
#>    ID      noise
#> 1 001 -0.4286493
#> 2 002 -0.1947424
#> 3 003 -1.6725099
#> 4 004  0.4079182
#> 5 005  0.3395556
#> 6 006 -0.8388726
```
