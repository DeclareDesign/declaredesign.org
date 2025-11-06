# Explore your design diagnosis

Explore your design diagnosis

## Usage

``` r
get_diagnosands(diagnosis)

get_simulations(diagnosis)
```

## Arguments

- diagnosis:

  A design diagnosis created by
  [`diagnose_design`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md).

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

if (FALSE) { # \dontrun{
# Diagnose design using default diagnosands
diagnosis <- diagnose_design(design)
diagnosis

# Use get_diagnosands to explore diagnosands:
get_diagnosands(diagnosis)

# Use get_simulations to explore simulations
get_simulations(diagnosis)

# Exploring user-defined diagnosis your own diagnosands
my_diagnosands <-
  declare_diagnosands(median_bias = median(estimate - estimand),
                      absolute_error = mean(abs(estimate - estimand)))

diagnosis <- diagnose_design(design, diagnosands = my_diagnosands)
diagnosis

tidy(diagnosis)

reshape_diagnosis(diagnosis)

get_diagnosands(diagnosis)

get_simulations(diagnosis)

} # }
```
