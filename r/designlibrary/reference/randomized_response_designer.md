# Create a randomized response design

Routes to
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
with id `"randomized_response"`:
`make_design("randomized_response", N = N, ...)`.

## Usage

``` r
randomized_response_designer(
  N = 1000,
  prob_forced_yes = 0.6,
  prevalence_rate = 0.1,
  withholding_rate = 0.5,
  args_to_fix = NULL
)
```

## Arguments

- N:

  Sample size.

- prob_forced_yes:

  Probability a respondent is forced to answer yes.

- prevalence_rate:

  Share of units holding the sensitive trait.

- withholding_rate:

  Share of trait holders who deny it under direct questioning.

- args_to_fix:

  Ignored. Present for compatibility with DesignLibrary 0.1.

## Value

A design object.

## See also

[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)

## Examples

``` r
randomized_response_designer(N = 40, prevalence_rate = 0.2)
#> Research design with 7 steps
#> 
#> Step 1 (parameters): declare_parameters(N = 1000, prob_forced_yes = 0.6, prevalence_rate = 0.1, withholding_rate = 0.5)
#> Step 2 (model): declare_model(N = N, sensitive_trait = draw_binary(prob = prevalence_rate, N = N), withholder = draw_binary(prob = sensitive_trait * withholding_rate, N = N), direct_answer = sensitive_trait - withholder, Y_Z_Yes = 1, Y_Z_Truth = sensitive_trait)
#> Step 3 (inquiry): declare_inquiry(true_rate = mean(sensitive_trait))
#> Step 4 (assignment): declare_assignment(Z = complete_ra(N, prob = prob_forced_yes, conditions = c("Truth", "Yes")))
#> Step 5 (measurement): declare_measurement(Y = reveal_outcomes(Y ~ Z))
#> Step 6 (estimator): declare_estimator(handler = label_estimator(function(data, prob_forced_yes) { data.frame(estimate = (mean(data$Y) - prob_forced_yes)/(1 - prob_forced_yes)) }), prob_forced_yes = prob_forced_yes, inquiry = "true_rate", label = "Forced Randomized Response")
#> Step 7 (estimator): declare_estimator(handler = label_estimator(function(data) { data.frame(estimate = mean(data$direct_answer)) }), inquiry = "true_rate", label = "Direct Question")
#> 
#> Parameters and objects the design refers to:
#>              name value   kind declared   steps
#>                 N    40 scalar     TRUE    1, 2
#>   prevalence_rate   0.2 scalar     TRUE    1, 2
#>   prob_forced_yes   0.6 scalar     TRUE 1, 4, 6
#>  withholding_rate   0.5 scalar     TRUE    1, 2
```
