# Create a two-by-two factorial design

Routes to
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
with id `"two_by_two"`:
`make_design("two_by_two", N = N, outcome_means = outcome_means, ...)`.

## Usage

``` r
two_by_two_designer(
  N = 100,
  prob_A = 0.5,
  prob_B = 0.5,
  weight_A = 0.5,
  weight_B = 0.5,
  outcome_means = c(0, 0, 0, 0),
  mean_A0B0 = NULL,
  mean_A0B1 = NULL,
  mean_A1B0 = NULL,
  mean_A1B1 = NULL,
  sd_i = 1,
  outcome_sds = c(0, 0, 0, 0),
  args_to_fix = NULL
)
```

## Arguments

- N:

  Sample size.

- prob_A:

  Probability of assignment to A = 1.

- prob_B:

  Probability of assignment to B = 1.

- weight_A:

  Weight on A = 1 when defining the average effect of B.

- weight_B:

  Weight on B = 1 when defining the average effect of A.

- outcome_means:

  Average outcome in each cell, length 4.

- mean_A0B0, mean_A0B1, mean_A1B0, mean_A1B1:

  Optional cell-mean overrides.

- sd_i:

  Standard deviation of the individual-level shock.

- outcome_sds:

  Extra standard deviation in each cell, length 4.

- args_to_fix:

  Ignored. Present for compatibility with DesignLibrary 0.1.

## Value

A design object.

## Details

Cell means are `outcome_means` in order AB = 00, 01, 10, 11. Scalar
`mean_A0B0` / `mean_A0B1` / `mean_A1B0` / `mean_A1B1` override those
entries when supplied.

## See also

[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)

## Examples

``` r
two_by_two_designer(N = 40, outcome_means = c(0, 0.5, 0.5, 1))
#> Research design with 7 steps
#> 
#> Step 1 (parameters): declare_parameters(N = 100, prob_A = 0.5, prob_B = 0.5, weight_A = 0.5, weight_B = 0.5, outcome_means = c(0, 0, 0, 0), sd_i = 1, outcome_sds = c(0, 0, 0, 0))
#> Step 2 (model): declare_model(N = N, u = rnorm(N, sd = sd_i), potential_outcomes(Y ~ outcome_means[1 + B + 2 * A] + u + rnorm(N, sd = outcome_sds[1 + B + 2 * A]), conditions = list(A = c(0, 1), B = c(0, 1))))
#> Step 3 (inquiry): declare_inquiry(ate_A = weight_B * mean(Y_A_1_B_1 - Y_A_0_B_1) + (1 - weight_B) * mean(Y_A_1_B_0 - Y_A_0_B_0), ate_B = weight_A * mean(Y_A_1_B_1 - Y_A_1_B_0) + (1 - weight_A) * mean(Y_A_0_B_1 - Y_A_0_B_0), interaction = mean((Y_A_1_B_1 - Y_A_1_B_0) - (Y_A_0_B_1 - Y_A_0_B_0)))
#> Step 4 (assignment): declare_assignment(A = complete_ra(N, prob = prob_A), B = block_ra(blocks = A, prob = prob_B))
#> Step 5 (measurement): declare_measurement(Y = reveal_outcomes(Y ~ A + B))
#> Step 6 (estimator): declare_estimator(Y ~ A + B, .method = lm_robust, term = c("A", "B"), inquiry = c("ate_A", "ate_B"), label = "No_Interaction")
#> Step 7 (estimator): declare_estimator(Y ~ A + B + A:B, .method = lm_robust, term = "A:B", inquiry = "interaction", label = "Interaction")
#> 
#> Parameters and objects the design refers to:
#>           name             value   kind declared steps
#>              N                40 scalar     TRUE  1, 2
#>  outcome_means c(0, 0.5, 0.5, 1) vector     TRUE  1, 2
#>    outcome_sds     c(0, 0, 0, 0) vector     TRUE  1, 2
#>         prob_A               0.5 scalar     TRUE  1, 4
#>         prob_B               0.5 scalar     TRUE  1, 4
#>           sd_i                 1 scalar     TRUE  1, 2
#>       weight_A               0.5 scalar     TRUE  1, 3
#>       weight_B               0.5 scalar     TRUE  1, 3
```
