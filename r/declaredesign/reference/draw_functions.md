# Draw data, estimates, and inquiries from a design

Draw data, estimates, and inquiries from a design

## Usage

``` r
draw_data(design, data = NULL, start = 1, end = length(design))

draw_estimand(...)

draw_estimands(...)

draw_estimates(...)
```

## Arguments

- design:

  A design object, typically created using the + operator

- data:

  A data.frame object with sufficient information to get the data,
  estimates, inquiries, an assignment vector, or a sample.

- start:

  (Defaults to 1) a scalar indicating which step in the design to begin
  with. By default all data steps are drawn, from step 1 to the last
  step of the design.

- end:

  (Defaults to `length(design)`) a scalar indicating which step in the
  design to finish drawing data by.

- ...:

  A design or set of designs typically created using the + operator

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

# Use draw_data to create a dataset using a design
dat <- draw_data(design)

# Use end argument to draw data up to a certain design component
dat_no_sampling <- draw_data(design, end = 3)

# Use draw_estimands to extract value of inquiry
draw_estimands(design)
#>   inquiry estimand
#> 1     ATE      0.2

# Use draw_estimates to extract value of estimator
draw_estimates(design)
#>   estimator term estimate  std.error statistic      p.value  conf.low conf.high
#> 1 estimator    Z 0.334225 0.07695986  4.342849 2.243858e-05 0.1824589 0.4859912
#>    df outcome inquiry
#> 1 198       Y     ATE
```
