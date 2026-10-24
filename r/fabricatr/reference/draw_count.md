# Draw Poisson count data

Draw Poisson count data

## Usage

``` r
draw_count(
  mean = apply_link(latent, link),
  N = length(mean),
  link = "identity",
  latent = NULL,
  quantile_y = NULL,
  dispersion = 0
)
```

## Arguments

- mean:

  Mean count (lambda). Scalar or per-unit vector, already the rate the
  draw uses. Supplying it with a non-identity `link` is an error naming
  `latent`, because `mean = log(3)` with `link = "log"` reads as a
  request to exponentiate and no range check can tell it apart from a
  rate of 1.099.

- N:

  Number of draws. Defaults to `length(mean)`.

- link:

  `"identity"` (default) or `"log"`, or a function applied to `latent`.
  `"log"` exponentiates `latent` to get the rate. Any other name is an
  error. fabricatr 1.x accepts no link here at all, answering "Count
  data does not accept link functions".

- latent:

  Latent variable on the log scale, exponentiated by `link = "log"` to
  give the rate.

- quantile_y:

  Optional quantile vector for `correlate`.

- dispersion:

  Overdispersion, a single non-negative number. The default 0 is the
  Poisson, whose variance equals its mean. A positive value draws from
  the negative binomial with the same mean and variance
  `mean + dispersion * mean^2`. It is the reciprocal of the `theta` that
  [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html) reports,
  and the `alpha` of Stata's `nbreg`.

## Value

An integer vector of non-negative counts, of length `N`.

## Examples

``` r
fabricate(N = 5, rate = c(0, 1, 5, 10, 50),
          Y = draw_count(mean = rate))
#> # A tibble: 5 × 3
#>   ID     rate     Y
#>   <chr> <dbl> <int>
#> 1 1         0     0
#> 2 2         1     0
#> 3 3         5     8
#> 4 4        10    13
#> 5 5        50    53

# same mean, variance 3 + 0.5 * 3^2 = 7.5 rather than 3
y <- draw_count(mean = 3, N = 10000, dispersion = 0.5)
c(mean(y), var(y))
#> [1] 2.994100 7.457611
```
