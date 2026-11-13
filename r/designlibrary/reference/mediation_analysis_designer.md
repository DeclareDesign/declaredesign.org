# Create a mediation analysis design

Routes to
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
with id `"mediation_analysis"`:
`make_design("mediation_analysis", N = N, a = a, ...)`.

## Usage

``` r
mediation_analysis_designer(
  N = 200,
  a = 1,
  b = 0.4,
  c = 0,
  d = 0.5,
  rho = 0,
  args_to_fix = NULL
)
```

## Arguments

- N:

  Sample size.

- a:

  Effect of treatment on the latent index for the mediator.

- b:

  Effect of the mediator on the outcome when Z = 0.

- c:

  Interaction of the mediator and treatment in the outcome.

- d:

  Direct effect of treatment on the outcome when M = 0.

- rho:

  Correlation between mediator and outcome shocks (-1 to 1).

- args_to_fix:

  Ignored. Present for compatibility with DesignLibrary 0.1.

## Value

A design object.

## See also

[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)

## Examples

``` r
mediation_analysis_designer(N = 40, a = 0.5, d = 0.2)
#> Research design with 9 steps
#> 
#> Step 1 (parameters): declare_parameters(N = 200, a = 1, b = 0.4, c = 0, d = 0.5, rho = 0)
#> Step 2 (model): declare_model(N = N, e1 = rnorm(N), e2 = rnorm(n = N, mean = rho * e1, sd = sqrt(1 - rho^2)), potential_outcomes(M ~ 1 * (a * Z + e1 > 0)), potential_outcomes(Y ~ d * Z + b * M + c * M * Z + e2, conditions = list(M = 0:1, Z = 0:1)), Y_nat0_Z_0 = b * M_Z_0 + e2, Y_nat0_Z_1 = d + b * M_Z_0 + c * M_Z_0 + e2, Y_nat1_Z_0 = b * M_Z_1 + e2, Y_nat1_Z_1 = d + b * M_Z_1 + c * M_Z_1 + e2)
#> Step 3 (inquiry): declare_inquiry(FirstStage = mean(M_Z_1 - M_Z_0), Mediator_Effect_0 = mean(Y_M_1_Z_0 - Y_M_0_Z_0), Mediator_Effect_1 = mean(Y_M_1_Z_1 - Y_M_0_Z_1), Controlled_Direct_0 = mean(Y_M_0_Z_1 - Y_M_0_Z_0), Controlled_Direct_1 = mean(Y_M_1_Z_1 - Y_M_1_Z_0), Natural_Direct_0 = mean(Y_nat0_Z_1 - Y_nat0_Z_0), Natural_Direct_1 = mean(Y_nat1_Z_1 - Y_nat1_Z_0))
#> Step 4 (assignment): declare_assignment(Z = complete_ra(N, prob = 0.5))
#> Step 5 (measurement): declare_measurement(M = reveal_outcomes(M ~ Z), Y = reveal_outcomes(Y ~ M + Z), Y_nat0 = reveal_outcomes(Y_nat0 ~ Z), Y_nat1 = reveal_outcomes(Y_nat1 ~ Z), Not_M = 1 - M)
#> Step 6 (estimator): declare_estimator(M ~ Z, .method = lm_robust, inquiry = "FirstStage", label = "Stage 1")
#> Step 7 (estimator): declare_estimator(Y ~ Z * M, .method = lm_robust, term = "M", inquiry = "Mediator_Effect_0", label = "Stage 2")
#> Step 8 (estimator): declare_estimator(Y ~ Z * M, .method = lm_robust, term = "Z", inquiry = c("Controlled_Direct_0", "Natural_Direct_0"), label = "Direct_0")
#> Step 9 (estimator): declare_estimator(Y ~ Z * Not_M, .method = lm_robust, term = "Z", inquiry = c("Controlled_Direct_1", "Natural_Direct_1"), label = "Direct_1")
#> 
#> Parameters and objects the design refers to:
#>  name value   kind declared      steps
#>     a   0.5 scalar     TRUE       1, 2
#>     b   0.4 scalar     TRUE       1, 2
#>     c     0 scalar     TRUE 1, 2, 8, 9
#>     d   0.2 scalar     TRUE       1, 2
#>     N    40 scalar     TRUE       1, 2
#>   rho     0 scalar     TRUE       1, 2
```
