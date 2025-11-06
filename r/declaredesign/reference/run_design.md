# Run a design one time

Run a design one time

## Usage

``` r
run_design(design)
```

## Arguments

- design:

  a DeclareDesign object

## Examples

``` r
# Two-arm randomized experiment
design <-
  declare_model(
    N = 500,
    gender = rbinom(N, 1, 0.5),
    X = rep(c(0, 1), each = N / 2),
    U = rnorm(N, sd = 0.25),
    potential_outcomes(Y ~ 0.2 * Z + X + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(S = complete_rs(N = N, n = 200)) +
  declare_assignment(Z = complete_ra(N = N, m = 100)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")

# Use run_design to run a design object
run_design(design)
#>   inquiry estimand estimator term  estimate  std.error statistic      p.value
#> 1     ATE      0.2 estimator    Z 0.3210013 0.07783232  4.124267 5.471242e-05
#>    conf.low conf.high  df outcome
#> 1 0.1675146  0.474488 198       Y
```
