# Wrap a custom function as a labeled estimator

`label_estimator()` returns a function suitable for use as `.method` in
[`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md)
or
[`declare_test()`](https://declaredesign.org/r/declaredesign/reference/declare_test.md):
it fits the model, tidies the fit, tags the result with `estimator` and
`inquiry` columns, and keeps the terms asked for. `label_test()` is the
same function under a second name, kept so that 1.x scripts run; a test
is an estimator whose tidied output carries a p-value, and nothing in
the wrapper depends on which name was called. 1.x's `fn =` argument is
`.method =` in both.

## Usage

``` r
label_estimator(
  .method,
  label = NULL,
  inquiry = NULL,
  term = NULL,
  .summary = tidy_try
)

label_test(
  .method,
  label = NULL,
  inquiry = NULL,
  term = NULL,
  .summary = tidy_try
)
```

## Arguments

- .method:

  (required) A function that fits a model from `data` and `...`.

- label:

  (optional) Estimator label, written to the `estimator` column. `NULL`
  (the default) adds no column.

- inquiry:

  (optional) Inquiry label (or list of labels); the first is written to
  the `inquiry` column. `NULL` (the default) adds no column.

- term:

  (optional) Character vector of terms to keep. `NULL` (the default)
  keeps every term.

- .summary:

  (optional) Function for tidying the model fit. Defaults to
  [`tidy_try()`](https://declaredesign.org/r/declaredesign/reference/tidy_try.md).

## Value

A function of `data` and `...` that calls `.method(data = data, ...)`,
tidies the fit with `.summary`, and returns the resulting tibble with
the `estimator` and `inquiry` columns set and its rows restricted to
`term`.

## Examples

``` r
my_est <- label_estimator(
  function(data, ...) lm(Y ~ Z, data = data),
  label = "lm", inquiry = "ATE", term = "Z"
)
df <- data.frame(Y = rnorm(20), Z = rep(0:1, 10))
my_est(df)
#> # A tibble: 1 × 9
#>   term  estimate std.error statistic p.value conf.low conf.high estimator
#>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <chr>    
#> 1 Z        0.291     0.576     0.505   0.619   -0.920      1.50 lm       
#> # ℹ 1 more variable: inquiry <chr>
```
