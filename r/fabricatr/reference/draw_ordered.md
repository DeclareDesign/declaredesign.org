# Draw ordered categorical outcomes from a latent variable

Cuts a continuous latent variable at the supplied `breaks` to produce
ordered discrete categories. A natural companion to latent-variable
specifications of survey responses.

## Usage

``` r
draw_ordered(
  x = latent,
  breaks,
  labels = NULL,
  N = length(x),
  strict = FALSE,
  latent = NULL,
  link = "identity",
  break_labels = NULL
)
```

## Arguments

- x:

  Latent continuous variable (numeric vector).

- breaks:

  Numeric vector of cut-points in ascending order. The vector should
  span the range of `x`; values outside the range are placed in the
  outermost categories unless `strict = TRUE`. An infinite endpoint
  bounds the scale rather than cutting it, so `c(-Inf, 0, Inf)` gives
  the same two categories as `0`.

- labels:

  Optional character vector of category labels, one per category. That
  is `length(breaks) + 1` for interior cut-points, one fewer for each
  infinite endpoint, and `length(breaks) - 1` when `strict = TRUE`. When
  supplied, returns an ordered factor.

- N:

  Must equal `length(x)` if it is given at all, and is an error
  otherwise. It cannot change how many values come back, which is always
  `length(x)`: unlike
  [`draw_binary()`](https://declaredesign.org/r/fabricatr/reference/draw_binary.md)
  and the rest of the family, `draw_ordered()` has never recycled to
  `N`, in either version. 1.x checks `N` against a recycling rule it
  does not apply.

- strict:

  If `TRUE`, observations outside `breaks` are coded `NA` instead of
  being placed in the outermost category.

- latent:

  Alias for `x` (kept for compatibility).

- link:

  Must be `"identity"`, the only link this function has: `breaks` cut
  the latent variable on the scale it arrives. Anything else is an
  error, as it is in fabricatr 1.x.

- break_labels:

  fabricatr 1.x's name for `labels`. Accepted with a warning.

## Value

An integer vector of category indices, one per element of `x`, or an
ordered factor with those `labels` when they are supplied. With
`strict = TRUE`, values outside `breaks` are `NA`.

## Examples

``` r
fabricate(N = 200, x = rnorm(N),
          resp = draw_ordered(x, breaks = c(-1, 0, 1),
                              labels = c("disagree", "neutral",
                                         "agree", "strongly agree")))
#> # A tibble: 200 × 3
#>    ID         x resp          
#>    <chr>  <dbl> <ord>         
#>  1 001   -0.819 neutral       
#>  2 002    2.11  strongly agree
#>  3 003    1.93  strongly agree
#>  4 004    0.246 agree         
#>  5 005   -0.555 neutral       
#>  6 006    0.548 agree         
#>  7 007    1.62  strongly agree
#>  8 008    0.142 agree         
#>  9 009    0.189 agree         
#> 10 010    1.05  strongly agree
#> # ℹ 190 more rows
```
