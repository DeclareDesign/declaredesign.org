# Print a diagnosis

Prints the table
[`format.diagnosis()`](https://declaredesign.org/r/declaredesign/reference/format.diagnosis.md)
builds: one row of diagnosand estimates per group, with bootstrap
standard errors in parentheses on the row beneath.

## Usage

``` r
# S3 method for class 'diagnosis'
print(x, digits = 2, ...)
```

## Arguments

- x:

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
d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
print(d)
#> Research design diagnosis
#>   5 simulations
#> 
#>  Design Inquiry Estimator Term N Sims Mean Estimand Mean Estimate  Bias
#>  design     ATE       ols    Z      5          0.00         -0.15 -0.15
#>  SD Estimate RMSE Power Coverage
#>         0.66 0.61  0.20     0.80
```
