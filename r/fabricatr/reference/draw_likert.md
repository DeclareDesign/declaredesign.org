# Recode a latent variable into a Likert response

A convenience wrapper around `draw_ordered` that constructs equally
spaced breaks between `min` and `max`.

## Usage

``` r
draw_likert(
  x,
  min = NULL,
  max = NULL,
  bins = NULL,
  breaks = NULL,
  labels = NULL
)
```

## Arguments

- x:

  Latent numeric variable.

- min, max:

  Range the latent variable is cut on, rather than a filter applied to
  it: a value outside `[min, max]` joins the outermost bin. For 1.0.2's
  behaviour, where such a value became `NA`, call
  `draw_ordered(x, breaks = seq(min, max, length.out = bins + 1), strict = TRUE)`.

- bins:

  Number of Likert categories.

- breaks:

  Manual interior cut-points, read as
  [`draw_ordered()`](https://declaredesign.org/r/fabricatr/reference/draw_ordered.md)
  reads them, so `length(breaks) + 1` categories. An alternative to
  `min`, `max`, and `bins`. 1.0.2 read the same vector as the full set
  of bin boundaries; the `strict = TRUE` call above returns that answer.

- labels:

  Optional category labels.

## Value

An integer vector of category indices, one per element of `x`, or an
ordered factor with those `labels` when they are supplied.

## Examples

``` r
fabricate(N = 100, x = rnorm(N),
          likert = draw_likert(x, min = -3, max = 3, bins = 5))
#> # A tibble: 100 × 3
#>    ID         x likert
#>    <chr>  <dbl>  <int>
#>  1 001    0.347      3
#>  2 002   -0.918      2
#>  3 003    1.34       4
#>  4 004   -1.08       2
#>  5 005    1.07       4
#>  6 006    0.529      3
#>  7 007   -1.66       2
#>  8 008    0.678      4
#>  9 009   -1.07       2
#> 10 010   -0.341      3
#> # ℹ 90 more rows
```
