# Capture variables as quosures

Convenience alias for
[`rlang::quos()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
used inside design declarations to pass through bare expressions.

## Usage

``` r
vars(...)
```

## Arguments

- ...:

  (optional) Expressions, captured unevaluated.

## Value

A list of quosures of class `quosures`, one per expression.

## Examples

``` r
qs <- vars(x, y, z)
length(qs)
#> [1] 3
```
