# Draw normally distributed data with a target intra-cluster correlation

Uses a two-component variance model: each observation's value is the sum
of a cluster-level draw (between-cluster variance) and an
individual-level draw (within-cluster variance). The ICC equals
\\\sigma_b^2 / (\sigma_b^2 + \sigma_w^2)\\.

## Usage

``` r
draw_normal_icc(
  clusters,
  ICC = NULL,
  mean = 0,
  sd = NULL,
  sd_between = NULL,
  total_sd = NULL,
  N = NULL
)
```

## Arguments

- clusters:

  Vector of cluster IDs (factor, integer, or character).

- ICC:

  Target intra-cluster correlation, in \\\[0, 1\]\\. The endpoints are
  the degenerate cases rather than errors: at 0 the cluster variable
  does no work, and at 1 every unit in a cluster takes the same value.

- mean:

  Grand mean or per-cluster mean vector. Default 0. May be given per
  cluster or already expanded to one value per unit.

- sd:

  Within-cluster standard deviation.

- sd_between:

  Between-cluster standard deviation.

- total_sd:

  Overall standard deviation, \\\sqrt{\sigma_b^2 + \sigma_w^2}\\. This
  is a parameter of the distribution, not a rescaling of the draw, so
  the realised [`sd()`](https://rdrr.io/r/stats/sd.html) varies from
  draw to draw as any other sample statistic does. With `ICC`, it gives
  \\\sigma\_{between} = s\sqrt{ICC}\\ and \\\sigma\_{within} =
  s\sqrt{1 - ICC}\\. fabricatr 1.x instead rescales the finished vector
  so that its sample standard deviation is exactly `total_sd` every
  time; see the package vignette for why this diverges.

- N:

  Optional; must equal `length(clusters)` when provided.

## Value

A double vector of the same length as `clusters`, in the order the
clusters were given.

## Details

Supply any two of `ICC`, `sd` (within-cluster), `sd_between`
(between-cluster), and `total_sd`. The other two follow from
\\\sigma\_{total}^2 = \sigma_b^2 + \sigma_w^2\\. Supplying `ICC` alone
is shorthand for `sd = 1`.

## Examples

``` r
clusters <- rep(1:10, each = 20)
y <- draw_normal_icc(clusters = clusters, ICC = 0.4)
summary(lm(y ~ factor(clusters)))$r.squared  # approx 0.4
#> [1] 0.3377665
```
