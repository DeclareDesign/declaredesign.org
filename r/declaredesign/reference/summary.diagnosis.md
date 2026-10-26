# Summarize a diagnosis

The same report
[`print.diagnosis()`](https://declaredesign.org/r/declaredesign/reference/print.diagnosis.md)
gives.

## Usage

``` r
# S3 method for class 'diagnosis'
summary(object, digits = 2, ...)
```

## Arguments

- object:

  (required) A `diagnosis`.

- digits:

  (optional) Number of decimal places. Defaults to `2`.

- ...:

  (optional) Ignored.

## Value

The `data.frame` of formatted strings that
[`format.diagnosis()`](https://declaredesign.org/r/declaredesign/reference/format.diagnosis.md)
returns, invisibly.

## Examples

``` r
design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
  declare_inquiry(ATE = 0) +
  declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                    label = "ols")
summary(diagnose_design(design, sims = 5, bootstrap_sims = 0))
#> Research design diagnosis
#>   5 simulations
#> 
#>  Design Inquiry Estimator Term N Sims Mean Estimand Mean Estimate  Bias
#>  design     ATE       ols    Z      5          0.00         -0.09 -0.09
#>  SD Estimate RMSE Power Coverage
#>         0.09 0.12  0.00     1.00
```
