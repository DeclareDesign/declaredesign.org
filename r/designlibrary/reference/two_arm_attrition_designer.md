# Create a two-arm design with attrition

Routes to
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
with id `"two_arm_attrition"`:
`make_design("two_arm_attrition", N = N, a_R = a_R, ...)`.

## Usage

``` r
two_arm_attrition_designer(
  N = 100,
  a_R = 0,
  b_R = 1,
  a_Y = 0,
  b_Y = 1,
  rho = 0,
  args_to_fix = NULL
)
```

## Arguments

- N:

  Sample size.

- a_R:

  Constant in the equation relating treatment to reporting.

- b_R:

  Slope relating treatment to reporting.

- a_Y:

  Constant in the equation relating treatment to the outcome.

- b_Y:

  Slope relating treatment to the outcome.

- rho:

  Correlation between reporting and outcome shocks (0 to 1).

- args_to_fix:

  Ignored. Present for compatibility with DesignLibrary 0.1.

## Value

A design object.

## See also

[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)

## Examples

``` r
two_arm_attrition_designer(N = 40, b_R = 0.5)
#> Research design with 8 steps
#> 
#> Step 1 (parameters): declare_parameters(N = 100, a_R = 0, b_R = 1, a_Y = 0, b_Y = 1, rho = 0)
#> Step 2 (model): declare_model(N = N, u_R = rnorm(N), u_Y = rnorm(N, mean = rho * u_R, sd = sqrt(1 - rho^2)), potential_outcomes(R ~ (a_R + b_R * Z > u_R)), potential_outcomes(Y ~ (a_Y + b_Y * Z > u_Y)))
#> Step 3 (assignment): declare_assignment(Z = complete_ra(N, prob = 0.5))
#> Step 4 (measurement): declare_measurement(R = reveal_outcomes(R ~ Z), Y = reveal_outcomes(Y ~ Z), Y_obs = ifelse(R, Y, NA))
#> Step 5 (inquiry): declare_inquiry(`ATE on R` = mean(R_Z_1 - R_Z_0), `ATE on Y` = mean(Y_Z_1 - Y_Z_0), `ATE on Y (Given R)` = mean((Y_Z_1 - Y_Z_0)[R == 1]))
#> Step 6 (estimator): declare_estimator(R ~ Z, term = "Z", inquiry = "ATE on R", label = "DIM on R")
#> Step 7 (estimator): declare_estimator(Y_obs ~ Z, term = "Z", inquiry = c("ATE on Y", "ATE on Y (Given R)"), label = "DIM on Y_obs")
#> Step 8 (estimator): declare_estimator(Y ~ Z, term = "Z", inquiry = c("ATE on Y", "ATE on Y (Given R)"), label = "DIM on Y")
#> 
#> Parameters and objects the design refers to:
#>  name value   kind declared steps
#>   a_R     0 scalar     TRUE  1, 2
#>   a_Y     0 scalar     TRUE  1, 2
#>   b_R   0.5 scalar     TRUE  1, 2
#>   b_Y     1 scalar     TRUE  1, 2
#>     N    40 scalar     TRUE  1, 2
#>   rho     0 scalar     TRUE  1, 2
```
