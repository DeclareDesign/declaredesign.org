# Tidy a model fit, with a fallback

Tries [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
with confidence intervals; if that fails (because the model class has no
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) method),
assembles a minimal tidy table from the coefficient summary.

## Usage

``` r
tidy_try(fit, ...)
```

## Arguments

- fit:

  (required) A fitted model. A data frame is returned as a tibble
  unchanged.

- ...:

  (optional) Passed to
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html).

## Value

A tibble with one row per term and columns including `term`, `estimate`,
`std.error`, `statistic`, `p.value`, `conf.low`, and `conf.high`. When
neither
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) nor
`coef(summary(fit))` works, a tibble with those columns and no rows.

## Examples

``` r
fit <- lm(mpg ~ wt, data = mtcars)
tidy_try(fit)
#> # A tibble: 2 × 7
#>   term        estimate std.error statistic  p.value conf.low conf.high
#>   <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#> 1 (Intercept)    37.3      1.88      19.9  8.24e-19    33.5      41.1 
#> 2 wt             -5.34     0.559     -9.56 1.29e-10    -6.49     -4.20
```
