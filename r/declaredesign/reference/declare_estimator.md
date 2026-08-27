# Declare estimator

Declares an estimator which generates estimates and associated
statistics.

Use of `declare_test` is identical to use of `declare_estimator`. Use
`declare_test` for hypothesis testing with no specific inquiry in mind;
use `declare_estimator` for hypothesis testing when you can link each
estimate to an inquiry. For example, `declare_test` could be used for a
K-S test of distributional equality and `declare_estimator` for a
difference-in-means estimate of an average treatment effect.

## Usage

``` r
declare_estimator(
  ...,
  handler = label_estimator(method_handler),
  label = "estimator"
)

declare_estimators(
  ...,
  handler = label_estimator(method_handler),
  label = "estimator"
)

label_estimator(fn)

method_handler(
  data,
  ...,
  .method = estimatr::lm_robust,
  .summary = tidy_try,
  model,
  model_summary,
  term = FALSE
)
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
  data.frame with the estimates, summary statistics (i.e., standard
  error, p-value, and confidence interval), and a term column for
  labeling coefficient estimates.

- data:

  a data.frame

- .method:

  A method function, e.g. lm or glm. By default, the method is the
  [`estimatr::lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.html)
  function from the estimatr package, which fits OLS regression and
  calculates robust and cluster-robust standard errors.

- .summary:

  A method-in data-out function to extract coefficient estimates or
  method summary statistics, such as
  [`broom::tidy`](https://generics.r-lib.org/reference/tidy.html) or
  [`broom::glance`](https://generics.r-lib.org/reference/glance.html).
  By default, the `DeclareDesign` method summary function
  [`tidy_try`](https://declaredesign.org/r/declaredesign/reference/tidy_try.md)
  is used, which first attempts to use the available tidy method for the
  method object sent to `method`, then if not attempts to summarize
  coefficients using the `coef(summary())` and `confint` methods. If
  these do not exist for the method object, it fails.

- model:

  Deprecated argument. Use `.method` instead.

- model_summary:

  Deprecated argument. Use `.summary` instead.

- term:

  Symbols or literal character vector of term that represent quantities
  of interest, i.e. Z. If FALSE, return the first non-intercept term; if
  TRUE return all term. To escape non-standard-evaluation use `!!`.

## Value

A function that accepts a data.frame as an argument and returns a
data.frame containing the value of the estimator and associated
statistics.

## Details

`declare_estimator` is designed to handle two main ways of generating
parameter estimates from data.

In `declare_estimator`, you can optionally provide the name of an
inquiry or an object created by
[`declare_inquiry`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md)
to connect your estimate(s) to inquiry(s).

The first is through `label_estimator(method_handler)`, which is the
default value of the `handler` argument. Users can use standard method
functions like lm, glm, or iv_robust. The methods are summarized using
the function passed to the `summary` argument. This will usually be a
"tidier" like
[`broom::tidy`](https://generics.r-lib.org/reference/tidy.html). The
default `summary` function is `tidy_try`, which applies a tidy method if
available, and if not, tries to make one on the fly.

An example of this approach is:

`declare_estimator(Y ~ Z + X, .method = lm_robust, .summary = tidy, term = "Z", inquiry = "ATE")`

The second approach is using a custom data-in, data-out function,
usually first passed to `label_estimator`. The reason to pass the custom
function to `label_estimator` first is to enable clean labeling and
linking to inquiries.

An example of this approach is:

` my_fun <- function(data){ with(data, median(Y[Z == 1]) - median(Y[Z == 0])) } `

` declare_estimator(handler = label_estimator(my_fun), inquiry = "ATE") `

`label_estimator` takes a data-in-data out function to `fn`, and returns
a data-in-data-out function that first runs the provided estimation
function `fn` and then appends a label for the estimator and, if an
inquiry is provided, a label for the inquiry.

## See also

\[estimatr::lm_robust()\], \[broom::glance()\]

## Examples

``` r

# Setup for examples
design <-
  declare_model(
    N = 500,
    gender = rbinom(N, 1, 0.5),
    U = rnorm(N, sd = 0.25),
    potential_outcomes(Y ~ rbinom(
      N, 1, prob = pnorm(0.2 * Z + 0.2 * gender + 0.1 * Z * gender + U)
    ))
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(S = complete_rs(N = N, n = 200)) +
  declare_assignment(Z = complete_ra(N = N, m = 100)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z))

run_design(design)
#>   inquiry estimand
#> 1     ATE    0.108

# default estimator is lm_robust with tidy summary
design_0 <-
  design +
  declare_estimator(Y ~ Z, inquiry = "ATE")

run_design(design_0)
#>   inquiry estimand estimator term estimate  std.error statistic   p.value
#> 1     ATE    0.104 estimator    Z     0.09 0.07034547    1.2794 0.2022534
#>      conf.low conf.high  df outcome
#> 1 -0.04872249 0.2287225 198       Y

# Linear regression using lm_robust and tidy summary
design_1 <-
  design +
  declare_estimator(
    formula = Y ~ Z,
    .method = lm_robust,
    .summary = tidy,
    term = "Z",
    inquiry = "ATE",
    label = "lm_no_controls"
  )

run_design(design_1)
#>   inquiry estimand term      estimator estimate  std.error statistic   p.value
#> 1     ATE    0.096    Z lm_no_controls     0.08 0.06939857  1.152761 0.2503981
#>      conf.low conf.high  df outcome
#> 1 -0.05685519 0.2168552 198       Y

# Use glance summary function to view model fit statistics
design_2 <-
  design +
  declare_estimator(.method = lm_robust,
                    formula = Y ~ Z,
                    .summary = glance)

run_design(design_2)
#>   inquiry estimand estimator r.squared adj.r.squared statistic     p.value
#> 1     ATE    0.038 estimator 0.0410509    0.03620773  8.476027 0.004011144
#>   df.residual nobs se_type
#> 1         198  200     HC2

# Custom answer strategies
# A custom estimator should take data as an argument and return a data.frame
# with columns such as "estimate", "std.error", "p.value", "conf.low", "conf.high"
my_estimator <- function(data) {
  data.frame(estimate = mean(data$Y))
}

# Add a custom estimator to the design, wrapping it in `label_estimator()`
# in order to pass label and inquiry arguments

design_3 <-
  design +
  declare_inquiry(Y_bar = mean(Y)) +
  declare_estimator(handler = label_estimator(my_estimator),
                    label = "mean",
                    inquiry = "Y_bar")

run_design(design_3)
#>   inquiry estimand estimator estimate
#> 1   Y_bar    0.570      mean     0.57
#> 2     ATE    0.054      <NA>       NA

# Use `term` to select particular coefficients
design_4 <-
  design +
  declare_inquiry(difference_in_cates = mean(Y_Z_1[gender == 1] - Y_Z_0[gender == 1]) -
                    mean(Y_Z_1[gender == 0] - Y_Z_0[gender == 0])) +
  declare_estimator(Y ~ Z * gender,
                    term = "Z:gender",
                    inquiry = "difference_in_cates",
                    .method = lm_robust)

run_design(design_4)
#>               inquiry     estimand     term estimator    estimate std.error
#> 1 difference_in_cates 0.0008003201 Z:gender estimator -0.08878265 0.1340375
#> 2                 ATE 0.0500000000     <NA>      <NA>          NA        NA
#>    statistic   p.value   conf.low conf.high  df outcome
#> 1 -0.6623717 0.5085111 -0.3531235 0.1755582 196       Y
#> 2         NA        NA         NA        NA  NA    <NA>

if(require("broom")) {

  # Use glm from base R
  design_5 <-
    design +
    declare_estimator(Y ~ Z + gender,
                      family = "gaussian",
                      inquiry = "ATE",
                      .method = glm)

  run_design(design_5)

  # If we use logit, we'll need to estimate the average marginal effect with
  # marginaleffects::avg_slopes. We wrap this up in a function we'll pass to
  # .summary.

  if(require("marginaleffects")) {

    library(marginaleffects) # for predictions
    library(broom) # for tidy

    tidy_avg_slopes <- function(x) {
      tidy(avg_slopes(x))
    }

    design_6 <-
      design +
      declare_estimator(
        Y ~ Z + gender,
        .method = glm,
        family = binomial("logit"),
        .summary = tidy_avg_slopes,
        term = "Z"
      )

    run_design(design_6)

    # Multiple estimators for one inquiry

    design_7 <-
      design +
      declare_estimator(Y ~ Z,
                        .method = lm_robust,
                        inquiry = "ATE",
                        label = "OLS") +
      declare_estimator(
        Y ~ Z + gender,
        .method = glm,
        family = binomial("logit"),
        .summary = tidy_avg_slopes,
        inquiry = "ATE",
        term = "Z",
        label = "logit"
      )

    run_design(design_7)

  }

}
#> Loading required package: broom
#> Loading required package: marginaleffects
#> Warning: there is no package called ‘marginaleffects’
```
