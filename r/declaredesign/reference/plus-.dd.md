# Combine design steps into a design

The `+` operator concatenates `design_step` and `design` objects into a
single `design`. `design + NULL` is a no-op that returns the design
unchanged, which makes conditional step addition convenient.

## Usage

``` r
# S3 method for class 'dd'
e1 + e2
```

## Arguments

- e1:

  (required) A `design_step`, a `design`, or `NULL`.

- e2:

  (required) A `design_step`, a `design`, or `NULL`.

## Value

A `design` (class `c("design", "dd")`): a list of the steps of `e1`
followed by those of `e2`, named by their labels.

## Examples

``` r
d <- declare_model(N = 50, U = rnorm(N), Y = U) +
  declare_inquiry(mu = mean(Y))
length(d)
#> [1] 2
```
