# Declare test

Declares an test which generates a test statistic and associated
inferential statistics.

Use of `declare_test` is identical to use of
[`declare_estimator`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md).
Use `declare_test` for hypothesis testing with no specific inquiry in
mind; use `declare_estimator` for hypothesis testing when you can link
each estimate to an inquiry. For example, `declare_test` could be used
for a K-S test of distributional equality and `declare_estimator` for a
difference-in-means estimate of an average treatment effect.

See
[`declare_estimator`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md)
help for an explanation of how to use `method_handler`, which is used
identically in both `declare_estimator` and `declare_test`. The main
difference between `declare_estimator` and `declare_test` is that
`declare_test` does not link with an explicit inquiry.

## Usage

``` r
declare_test(..., handler = label_test(method_handler), label = "test")

label_test(fn)
```

## Arguments

- ...:

  arguments to be captured, and later passed to the handler

- handler:

  a tidy-in, tidy-out function

- label:

  a string describing the step

- fn:

  A function that takes a data.frame as an argument and returns a
  data.frame with test statistics as columns.

## Value

A function that accepts a data.frame as an argument and returns a
data.frame containing the value of the test statistic and other
inferential statistics.

## Details

`label_test` takes a data-in-data out function to `fn`, and returns a
data-in-data-out function that first runs the provided test function
`fn` and then appends a label for the test.

## See also

See
[`declare_estimator`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md)
for documentation of the `method_handler` function.

## Examples

``` r
# Balance test F test

balance_test_design <-
  declare_model(
    N = 100, 
    cov1 = rnorm(N), 
    cov2 = rnorm(N), 
    cov3 = rnorm(N)
  ) +
  declare_assignment(Z = complete_ra(N, prob = 0.2)) +
  declare_test(Z ~ cov1 + cov2 + cov3, .method = lm_robust, .summary = glance)
  
if (FALSE) { # \dontrun{
diagnosis <- diagnose_design(
  design = balance_test_design,
  diagnosands = declare_diagnosands(
  false_positive_rate = mean(p.value <= 0.05))
)
} # }

# K-S test of distributional equality

ks_test <- function(data) {
  test <- with(data, ks.test(x = Y[Z == 1], y = Y[Z == 0]))
  data.frame(statistic = test$statistic, p.value = test$p.value)
}

distributional_equality_design <-
  declare_model(
    N = 100, 
    Y_Z_1 = rnorm(N), 
    Y_Z_0 = rnorm(N, sd = 1.5)
  ) + 
  declare_assignment(Z = complete_ra(N, prob = 0.5)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
  declare_test(handler = label_test(ks_test), label = "ks-test")
  
if (FALSE) { # \dontrun{
diagnosis <- diagnose_design(
  design = distributional_equality_design,
  diagnosands = declare_diagnosands(power = mean(p.value <= 0.05))
) 
} # }

# Thanks to Jake Bowers for this example

if(require("coin")) {

  library(coin) 
  
  our_ttest <- function(data) {
    res <- coin::oneway_test(
      outcome ~ factor(Xclus),
      data = data,
      distribution = "asymptotic"
    )
    data.frame(p.value = pvalue(res)[[1]])
  }
  
  ttest_design <- 
    declare_model(
      N = 100, 
      Xclus = rbinom(n = N, size = 1, prob = 0.2), 
      outcome = 3 + rnorm(N)) +
    declare_test(handler = label_test(our_ttest), label = "t-test")
    
  if (FALSE) { # \dontrun{
  diagnosis <- diagnose_design(
    design = ttest_design,
    diagnosands = declare_diagnosands(
      false_positive_rate = mean(p.value <= 0.05))
  )
  } # }
  
}
#> Loading required package: coin
#> Loading required package: survival
#> 
#> Attaching package: ‘survival’
#> The following object is masked from ‘package:future’:
#> 
#>     cluster
```
