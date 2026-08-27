# Inclusion probabilities: Cluster Sampling

Returns each unit's probability of being sampled when whole clusters are
drawn. Every unit in a cluster shares its cluster's probability.

## Usage

``` r
cluster_rs_probabilities(
  clusters = NULL,
  n = NULL,
  n_unit = NULL,
  prob = NULL,
  prob_unit = NULL,
  simple = FALSE,
  check_inputs = TRUE
)
```

## Arguments

- clusters:

  A vector of length N indicating which cluster each unit belongs to.
  (required)

- n:

  Use for a design in which exactly `n` clusters are sampled. (optional)

- n_unit:

  `unique(n_unit)` will be passed to `n`; must be the same for all units
  and of length N. (optional)

- prob:

  Use for a design in which either `floor(N_clusters*prob)` or
  `ceiling(N_clusters*prob)` clusters are sampled. Which of the two is
  used is itself random: the ceiling is drawn with probability equal to
  the fractional part of `N_clusters*prob` and the floor otherwise,
  which makes each cluster's probability of inclusion exactly `prob`.
  Must be a real number between 0 and 1 inclusive. (optional)

- prob_unit:

  `unique(prob_unit)` will be passed to `prob`; must be the same for all
  units and of length N. (optional)

- simple:

  Logical, defaults to `FALSE`. If `TRUE`, clusters are drawn
  independently (simple random sampling of clusters), so the number of
  sampled clusters varies from draw to draw. Do not specify `n` when
  `simple = TRUE`. (optional)

- check_inputs:

  Logical. Whether to verify before sampling that the arguments are
  internally consistent: that `n` does not exceed the number of
  clusters, that probabilities lie between 0 and 1, and so on. Defaults
  to `TRUE`. Set to `FALSE` to skip the checks when drawing many samples
  from arguments that have already been verified; declaring the design
  once with
  [`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md)
  and drawing from it with
  [`draw_rs()`](https://declaredesign.org/r/randomizr/reference/draw_rs.md)
  does this for you. (optional)

## Value

A numeric vector of length N giving each unit's probability of being
included in the sample. Every unit in a cluster shares one probability.

## Details

These are the quantities inverse-probability weights are built from:
weight each sampled unit by the reciprocal of its inclusion probability,
which
[`obtain_inclusion_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_inclusion_probabilities.md)
extracts for you.

## See also

[`cluster_rs()`](https://declaredesign.org/r/randomizr/reference/cluster_rs.md)

## Examples

``` r

clusters <- rep(letters[1:10], times = 1:10)

probs <- cluster_rs_probabilities(clusters = clusters)
table(probs, clusters)
#>      clusters
#> probs  a  b  c  d  e  f  g  h  i  j
#>   0.5  1  2  3  4  5  6  7  8  9 10

probs <- cluster_rs_probabilities(clusters = clusters, n = 4)
table(probs, clusters)
#>      clusters
#> probs  a  b  c  d  e  f  g  h  i  j
#>   0.4  1  2  3  4  5  6  7  8  9 10

probs <- cluster_rs_probabilities(clusters = clusters, prob = 0.3)
table(probs, clusters)
#>      clusters
#> probs  a  b  c  d  e  f  g  h  i  j
#>   0.3  1  2  3  4  5  6  7  8  9 10

```
