# Declare a hypothesis test

Like
[`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md)
but the result table does not gain an `inquiry` column; intended for
tests not tied to an estimand.

## Usage

``` r
declare_test(
  ...,
  .method = NULL,
  .summary = tidy_try,
  term = NULL,
  label = "test",
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
  [`estimatr::lm_robust()`](https://rdrr.io/pkg/estimatr/man/lm_robust.html),
  as in
  [`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md).

- .summary:

  (optional) Function used to tidy `.method`'s output. Defaults to
  [`tidy_try()`](https://declaredesign.org/r/declaredesign/reference/tidy_try.md).

- term:

  (optional) Which model terms to report. A character vector keeps
  exactly those terms, in that order, and errors if any is absent from
  the tidied output; `TRUE` keeps every term including the intercept.
  When left unset (or `FALSE`), only the first non-intercept term is
  reported, so `declare_estimator(Y ~ Z + X)` returns the `Z` row alone;
  a `handler`'s table is kept whole unless `term` names terms. Evaluated
  on every draw, so it may name a design parameter.

- label:

  (optional) Step label. Defaults to `"test"`. When two estimator or
  test steps in one design share a label, `+` relabels them from their
  formulas, with a message.

- handler:

  (optional) A function. When supplied, the test bypasses `.method` and
  `.summary` and instead calls `handler(data, ...)` with the dots as
  written, not evaluated; it must return a tidy table.

- draws:

  (optional) Number of nested draws for this step. Defaults to `1`. When
  `> 1`, the step is re-executed `draws` times for each upstream draw
  during nested simulation.

## Value

A `design_step`, as
[`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md)
returns, whose estimates carry no `inquiry` column.

## See also

Other design declarations:
[`declare_assignment()`](https://declaredesign.org/r/declaredesign/reference/declare_assignment.md),
[`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md),
[`declare_inquiry()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md),
[`declare_measurement()`](https://declaredesign.org/r/declaredesign/reference/declare_measurement.md),
[`declare_model()`](https://declaredesign.org/r/declaredesign/reference/declare_model.md),
[`declare_notes()`](https://declaredesign.org/r/declaredesign/reference/declare_notes.md),
[`declare_parameters()`](https://declaredesign.org/r/declaredesign/reference/declare_parameters.md),
[`declare_sampling()`](https://declaredesign.org/r/declaredesign/reference/declare_sampling.md),
[`declare_step()`](https://declaredesign.org/r/declaredesign/reference/declare_step.md)

## Examples

``` r
design <- declare_model(N = 30, Z = rep(0:1, 15), Y = Z + rnorm(N)) +
  declare_test(Y ~ Z, term = "Z", label = "diff")
draw_estimates(design)
#> # A tibble: 1 × 10
#>   term  estimate std.error statistic p.value conf.low conf.high    df outcome
#>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 Z         1.09     0.389      2.79 0.00940    0.288      1.88    28 Y      
#> # ℹ 1 more variable: estimator <chr>
```
