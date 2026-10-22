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
#> # A tibble: 6 × 7
#>   ID         U  Y_Z_0  Y_Z_1     S     Z      Y
#>   <chr>  <dbl>  <dbl>  <dbl> <dbl> <int>  <dbl>
#> 1 003   -1.80  -1.80  -0.796     1     0 -1.80 
#> 2 004    1.72   1.72   2.72      1     0  1.72 
#> 3 006   -0.500 -0.500  0.500     1     0 -0.500
#> 4 007    2.22   2.22   3.22      1     0  2.22 
#> 5 008    0.853  0.853  1.85      1     0  0.853
#> 6 010    0.166  0.166  1.17      1     0  0.166

run_design(design)
#>   inquiry estimand estimator term estimate std.error statistic      p.value
#> 1     ATE        1 estimator    Z 1.249662 0.2243794  5.569417 6.635727e-08
#>    conf.low conf.high  df outcome
#> 1 0.8077302  1.691595 248       Y

# You may wish to have a design with only one step:

design <- declare_model(N = 500, noise = rnorm(N)) + NULL

dat <- draw_data(design)
head(dat)
#> # A tibble: 6 × 2
#>   ID      noise
#>   <chr>   <dbl>
#> 1 001    0.423 
#> 2 002   -1.99  
#> 3 003   -1.08  
#> 4 004    0.580 
#> 5 005   -1.49  
#> 6 006    0.0381
```
