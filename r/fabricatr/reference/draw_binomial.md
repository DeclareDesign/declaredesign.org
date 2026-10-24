# Draw binomial counts

Draw binomial counts

## Usage

``` r
draw_binomial(
  prob = apply_link(latent, link),
  trials = 1L,
  N = length(prob),
  link = "identity",
  latent = NULL,
  quantile_y = NULL
)
```

## Arguments

- prob:

  Probability of success per trial, already on the probability scale.
  Supplying it with a non-identity `link` is an error naming `latent`.

- trials:

  Number of trials per observation (scalar or vector).

- N:

  Number of observations. Defaults to `length(prob)`.

- link:

  One of `"identity"` (default), `"logit"`, `"logistic"`, `"probit"`, or
  a function applied to `latent`. Any other name is an error.

- latent:

  Latent variable on an unbounded scale, mapped to the probability scale
  by `link`.

- quantile_y:

  Optional quantile vector for `correlate`.

## Value

An integer vector of successes out of `trials`, of length `N`.

## Examples

``` r
fabricate(N = 4, p = c(0.1, 0.3, 0.7, 0.9),
          Y = draw_binomial(prob = p, trials = 10))
#> # A tibble: 4 × 3
#>   ID        p     Y
#>   <chr> <dbl> <int>
#> 1 1       0.1     0
#> 2 2       0.3     2
#> 3 3       0.7     8
#> 4 4       0.9     8
```
