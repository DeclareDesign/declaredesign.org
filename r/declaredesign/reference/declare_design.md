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
#>    ID          U      Y_Z_0     Y_Z_1 S Z          Y
#> 1 003  0.8567442  0.8567442 1.8567442 1 0  0.8567442
#> 2 004 -0.6856990 -0.6856990 0.3143010 1 0 -0.6856990
#> 3 005  0.3752808  0.3752808 1.3752808 1 0  0.3752808
#> 4 006 -0.3716699 -0.3716699 0.6283301 1 0 -0.3716699
#> 5 007 -0.4176228 -0.4176228 0.5823772 1 0 -0.4176228
#> 6 009  0.3705969  0.3705969 1.3705969 1 0  0.3705969

run_design(design)
#>   inquiry estimand estimator term  estimate std.error statistic      p.value
#> 1     ATE        1 estimator    Z 0.7431553 0.2061222  3.605411 0.0003767002
#>   conf.low conf.high  df outcome
#> 1 0.337182  1.149129 248       Y

# You may wish to have a design with only one step:

design <- declare_model(N = 500, noise = rnorm(N)) + NULL

dat <- draw_data(design)
head(dat)
#>    ID      noise
#> 1 001  1.7380093
#> 2 002 -0.2159268
#> 3 003 -1.1699856
#> 4 004  0.5661610
#> 5 005 -0.8782113
#> 6 006  1.6686682
```
