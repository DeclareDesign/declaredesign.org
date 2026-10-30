# Declare an estimator

Declares the answer strategy: a function that produces an estimate of an
inquiry from the observed data. The estimate is tidied to one row per
term and joined to its estimand by the `inquiry` label during diagnosis.

## Usage

``` r
declare_estimator(
  ...,
  .method = NULL,
  .summary = tidy_try,
  inquiry = NULL,
  term = NULL,
  label = "estimator",
  handler = NULL,
  draws = 1L
)
```

## Arguments

- ...:

  (optional) Arguments forwarded, as written, to `.method` (or to
  `handler`), which evaluates them itself with the data as `data`.
  Typically the formula appears first (e.g., `Y ~ Z`); without a
  `handler`, these must include whatever `.method` needs. A legacy
  `model =` argument is read as `.method` with a deprecation warning
  rather than forwarded.

- .method:

  (optional) The model-fitting function. Defaults to
  [`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html).

- .summary:

  (optional) Function used to tidy `.method`'s output. Defaults to
  [`tidy_try()`](https://declaredesign.org/r/declaredesign/reference/tidy_try.md).

- inquiry:

  (optional) The label of the inquiry this estimator targets, as a
  string (`inquiry = "ATE"`), or a character vector of labels. Passing
  the inquiry step object is an error. The label joins the estimate to
  its estimand in every table of estimates and in the diagnosis.

- term:

  (optional) Which model terms to report. A character vector keeps
  exactly those terms, in that order, and errors if any is absent from
  the tidied output; `TRUE` keeps every term including the intercept.
  When left unset (or `FALSE`), only the first non-intercept term is
  reported, so `declare_estimator(Y ~ Z + X)` returns the `Z` row alone;
  a `handler`'s table is kept whole unless `term` names terms. Evaluated
  on every draw, so it may name a design parameter.

- label:

  (optional) Step label. Defaults to `"estimator"`. When two estimator
  or test steps in one design share a label, `+` relabels them from
  their formulas, with a message.

- handler:

  (optional) A function. When supplied, the estimator bypasses `.method`
  and `.summary` and instead calls `handler(data, ...)` with the dots as
  written, not evaluated; it must return a tidy table.

- draws:

  (optional) Number of nested draws for this step. Defaults to `1`. When
  `> 1`, the step is re-executed `draws` times for each upstream draw
  during nested simulation.

## Value

A `design_step` (class `c("design_step", "dd", "function")`): a function
that takes a data frame and returns a tibble of estimates, one row per
reported term, with `estimator` and, when `inquiry` is set, `inquiry`
columns. A draw on which `.method` errors returns a row with
`error = TRUE` and the message rather than stopping the run. Add it to a
design with `+`.

## See also

Other design declarations:
[`declare_assignment()`](https://declaredesign.org/r/declaredesign/reference/declare_assignment.md),
[`declare_inquiry()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md),
[`declare_measurement()`](https://declaredesign.org/r/declaredesign/reference/declare_measurement.md),
[`declare_model()`](https://declaredesign.org/r/declaredesign/reference/declare_model.md),
[`declare_notes()`](https://declaredesign.org/r/declaredesign/reference/declare_notes.md),
[`declare_parameters()`](https://declaredesign.org/r/declaredesign/reference/declare_parameters.md),
[`declare_sampling()`](https://declaredesign.org/r/declaredesign/reference/declare_sampling.md),
[`declare_step()`](https://declaredesign.org/r/declaredesign/reference/declare_step.md),
[`declare_test()`](https://declaredesign.org/r/declaredesign/reference/declare_test.md)

## Examples

``` r
design <- declare_model(N = 50, U = rnorm(N), Y = U) +
  declare_inquiry(mu = mean(Y)) +
  declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)",
                    inquiry = "mu", label = "ols")
draw_estimates(design)
#> # A tibble: 1 × 10
#>   term        estimate std.error statistic p.value conf.low conf.high estimator
#>   <chr>          <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <chr>    
#> 1 (Intercept)  -0.0953     0.129    -0.741   0.462   -0.354     0.163 ols      
#> # ℹ 2 more variables: inquiry <chr>, estimand <dbl>
```
