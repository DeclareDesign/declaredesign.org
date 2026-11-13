# Create a pretest-posttest design

Routes to
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
with id `"pretest_posttest"`:
`make_design("pretest_posttest", N = N, ate = ate, ...)`.

## Usage

``` r
pretest_posttest_designer(
  N = 100,
  ate = 0.25,
  sd_1 = 1,
  sd_2 = 1,
  rho = 0.5,
  attrition_rate = 0.1,
  args_to_fix = NULL
)
```

## Arguments

- N:

  Sample size.

- ate:

  Average treatment effect.

- sd_1:

  Standard deviation of the pretest shock.

- sd_2:

  Standard deviation of the posttest shock.

- rho:

  Correlation between pretest and posttest shocks (-1 to 1).

- attrition_rate:

  Share of units not observed at the posttest.

- args_to_fix:

  Ignored. Present for compatibility with DesignLibrary 0.1.

## Value

A design object.

## See also

[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)

## Examples

``` r
pretest_posttest_designer(N = 40, attrition_rate = 0.2)
#> Research design with 8 steps
#> 
#> Step 1 (parameters): declare_parameters(N = 100, ate = 0.25, sd_1 = 1, sd_2 = 1, rho = 0.5, attrition_rate = 0.1)
#> Step 2 (model): declare_model(N = N, u_t1 = rnorm(N) * sd_1, u_t2 = rnorm(N, rho * scale(u_t1), sqrt(1 - rho^2)) * sd_2, Y_t1 = u_t1, potential_outcomes(Y_t2 ~ u_t2 + ate * Z))
#> Step 3 (inquiry): declare_inquiry(ATE = mean(Y_t2_Z_1 - Y_t2_Z_0))
#> Step 4 (assignment): declare_assignment(Z = complete_ra(N))
#> Step 5 (measurement): declare_measurement(Y_t2 = reveal_outcomes(Y_t2 ~ Z), R = complete_ra(N, prob = 1 - attrition_rate), difference = Y_t2 - Y_t1)
#> Step 6 (estimator): declare_estimator(difference ~ Z, .method = lm_robust, inquiry = "ATE", subset = R == 1, label = "Change score")
#> Step 7 (estimator): declare_estimator(Y_t2 ~ Z + Y_t1, .method = lm_robust, term = "Z", inquiry = "ATE", subset = R == 1, label = "Condition on pretest")
#> Step 8 (estimator): declare_estimator(Y_t2 ~ Z, .method = lm_robust, inquiry = "ATE", label = "Posttest only")
#> 
#> Parameters and objects the design refers to:
#>            name value   kind declared steps
#>             ate  0.25 scalar     TRUE  1, 2
#>  attrition_rate   0.2 scalar     TRUE  1, 5
#>               N    40 scalar     TRUE  1, 2
#>             rho   0.5 scalar     TRUE  1, 2
#>            sd_1     1 scalar     TRUE  1, 2
#>            sd_2     1 scalar     TRUE  1, 2
```
