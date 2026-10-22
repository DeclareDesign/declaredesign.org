# Declare measurement procedure

Add a step to a design that explicitly indicates what variables are
measured by a design. It is always possible to include measured
variables directly in your declare_model calls or using declare_step.
However, putting latent variables in declare_model and
variables-as-measured in declare_measurement helps communicate which
parts of your research design are in the model and which parts are in
the data strategy.

## Usage

``` r
declare_measurement(..., handler = measurement_handler, label = NULL)

measurement_handler(data, ...)

potential_outcomes_handler(data, ...)
```

## Arguments

- ...:

  arguments to be captured, and later passed to the handler

- handler:

  a tidy-in, tidy-out function

- label:

  a string describing the step

- data:

  A data.frame.

## Value

A function that returns a data.frame.

## Examples

``` r

# declare_measurement a in use a
## Two-arm randomized experiment
design <-
  declare_model(
    N = 500,
    X = rep(c(0, 1), each = N / 2),
    U = rnorm(N, sd = 0.25),
    potential_outcomes(Y ~ 0.2 * Z + X + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(S = complete_rs(N = N, n = 200)) +
  declare_assignment(Z = complete_ra(N = N, m = 100)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")
  
run_design(design)
#>   inquiry estimand estimator term   estimate  std.error statistic  p.value
#> 1     ATE      0.2 estimator    Z 0.07833315 0.07858214 0.9968314 0.320063
#>      conf.low conf.high  df outcome
#> 1 -0.07663221 0.2332985 198       Y

# Reveal potential outcomes according to treatment assignment
design <-
  declare_model(N = 100,
                potential_outcomes(Y ~ rbinom(
                  N, size = 1, prob = 0.1 * Z + 0.5
                ))) +
  declare_assignment(Z = complete_ra(N, m = 50)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z))
  
head(draw_data(design))
#> # A tibble: 6 × 5
#>   ID    Y_Z_0 Y_Z_1     Z     Y
#>   <chr> <int> <int> <int> <int>
#> 1 001       0     0     0     0
#> 2 002       0     1     0     0
#> 3 003       0     1     0     0
#> 4 004       0     1     1     1
#> 5 005       0     0     1     0
#> 6 006       1     0     1     0

# Generate observed measurement from a latent value
design <- 
  declare_model(N = 100, latent = runif(N)) +
  declare_measurement(observed = rbinom(N, 1, prob = latent))
  
head(draw_data(design))
#> # A tibble: 6 × 3
#>   ID    latent observed
#>   <chr>  <dbl>    <int>
#> 1 001   0.0544        0
#> 2 002   0.688         0
#> 3 003   0.832         0
#> 4 004   0.195         0
#> 5 005   0.368         0
#> 6 006   0.731         0

if(require("psych")) {

  # Index creation
  
  library(psych)
  
  design <-
    declare_model(
      N = 500,
      X = rep(c(0, 1), each = N / 2),
      Y_1 = 0.2 * X + rnorm(N, sd = 0.25),
      Y_2 = 0.3 * X + 0.5 * rnorm(N, sd = 0.50),
      Y_3 = 0.1 * X + 0.4 * rnorm(N, sd = 0.75)) +
    declare_measurement(
      index = fa(
        r = cbind(Y_1, Y_2, Y_3),
        nfactors = 1,
        rotate = "varimax"
      )$scores
    )
  
  draw_data(design)
}
#> Loading required package: psych
#> # A tibble: 500 × 6
#>    ID        X     Y_1     Y_2     Y_3   index
#>    <chr> <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 001       0 -0.143  -0.319  -0.129  -0.989 
#>  2 002       0 -0.0594 -0.340  -0.318  -0.987 
#>  3 003       0  0.459   0.0110  0.0558  0.173 
#>  4 004       0 -0.106   0.0494 -0.109  -0.383 
#>  5 005       0  0.174   0.183   0.488   0.261 
#>  6 006       0  0.105  -0.606  -0.372  -1.24  
#>  7 007       0  0.0860  0.0587  0.202  -0.0925
#>  8 008       0  0.0388 -0.271  -0.367  -0.795 
#>  9 009       0 -0.257  -0.0293  0.526  -0.489 
#> 10 010       0 -0.119  -0.422  -0.315  -1.17  
#> # ℹ 490 more rows
```
