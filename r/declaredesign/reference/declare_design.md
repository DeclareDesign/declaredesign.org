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
#> 1 001  1.4162072  1.4162072 2.4162072 1 0  1.4162072
#> 2 003  0.6533846  0.6533846 1.6533846 1 0  0.6533846
#> 3 004  0.5013482  0.5013482 1.5013482 1 0  0.5013482
#> 4 005 -0.4871751 -0.4871751 0.5128249 1 0 -0.4871751
#> 5 007 -0.7751089 -0.7751089 0.2248911 1 0 -0.7751089
#> 6 008  1.7711269  1.7711269 2.7711269 1 1  2.7711269

run_design(design)
#>   inquiry estimand estimator term estimate std.error statistic      p.value
#> 1     ATE        1 estimator    Z 0.908514 0.2492241   3.64537 0.0003252696
#>    conf.low conf.high  df outcome
#> 1 0.4176483   1.39938 248       Y

# You may wish to have a design with only one step:

design <- declare_model(N = 500, noise = rnorm(N)) + NULL

dat <- draw_data(design)
head(dat)
#>    ID       noise
#> 1 001 -0.19430706
#> 2 002 -0.07246097
#> 3 003  1.84071996
#> 4 004 -0.03911534
#> 5 005  0.55880346
#> 6 006  1.08627093
```
