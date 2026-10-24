# Draw binary (0/1) outcomes

Draw binary (0/1) outcomes

## Usage

``` r
draw_binary(
  prob = apply_link(latent, link),
  N = length(prob),
  link = "identity",
  latent = NULL,
  quantile_y = NULL
)
```

## Arguments

- prob:

  Probability of success. Scalar or per-unit vector. Already on the
  probability scale, so a non-identity `link` has nothing to do to it:
  supplying both is an error naming `latent`.

- N:

  Number of draws. Defaults to `length(prob)`.

- link:

  One of `"identity"` (default), `"logit"`, `"logistic"`, `"probit"`, or
  a function, which is applied to `latent` as it stands. Any other name
  is an error, as it is in fabricatr 1.x; `"logistic"` is a 2.0 synonym
  for `"logit"` that 1.x does not accept. The link acts on `latent`
  only.

- latent:

  Latent variable on an unbounded scale, mapped to the probability scale
  by `link`. Supply this rather than `prob` whenever `link` is not the
  identity.

- quantile_y:

  Optional quantile vector for deterministic draws (used by
  `correlate`).

## Value

An integer vector of 0s and 1s, of length `N`, which defaults to
`length(prob)`.

## Examples

``` r
fabricate(N = 6, p = c(0, 0.2, 0.4, 0.6, 0.8, 1),
          Y = draw_binary(prob = p))
#> # A tibble: 6 × 3
#>   ID        p     Y
#>   <chr> <dbl> <int>
#> 1 1       0       0
#> 2 2       0.2     1
#> 3 3       0.4     0
#> 4 4       0.6     1
#> 5 5       0.8     1
#> 6 6       1       1

# Logit link: supply a latent continuous variable
fabricate(N = 100, x = rnorm(N), Y = draw_binary(latent = x, link = "logit"))
#> # A tibble: 100 × 3
#>    ID         x     Y
#>    <chr>  <dbl> <int>
#>  1 001    0.391     0
#>  2 002    0.608     0
#>  3 003   -0.998     0
#>  4 004    1.20      1
#>  5 005   -1.77      0
#>  6 006    0.785     0
#>  7 007   -0.437     1
#>  8 008   -0.352     0
#>  9 009    1.30      1
#> 10 010   -1.08      0
#> # ℹ 90 more rows
```
