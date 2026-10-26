# Deprecated alias for `label_estimator()`

`model_handler()` and `tidy_estimator()` are retained for compatibility
but will be removed in a future version. Use
[`label_estimator()`](https://declaredesign.org/r/declaredesign/reference/label_estimator.md)
instead.

## Usage

``` r
model_handler(...)

tidy_estimator(...)
```

## Arguments

- ...:

  (required) Forwarded to
  [`label_estimator()`](https://declaredesign.org/r/declaredesign/reference/label_estimator.md),
  which needs at least `.method`.

## Value

A function, as
[`label_estimator()`](https://declaredesign.org/r/declaredesign/reference/label_estimator.md)
returns.
