# Cluster Random Sampling

`cluster_rs` draws whole groups of units (clusters) into the sample, so
that either every unit in a cluster is sampled or none of them is. Use
it when the sampling frame lists groups rather than individuals, for
example when villages are drawn and then everyone in the drawn villages
is interviewed. Because units come in whole clusters, the effective
sample size is closer to the number of clusters than to the number of
units.

## Usage

``` r
cluster_rs(
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

A numeric vector of length N indicating whether each unit is sampled (1)
or not (0). Every unit in a cluster receives the same value.

## Details

By default the clusters are drawn by complete random sampling, so a
fixed number of clusters is sampled on every draw. Setting
`simple = TRUE` draws each cluster independently instead, using
[`simple_rs()`](https://declaredesign.org/r/randomizr/reference/simple_rs.md).

## See also

[`complete_rs()`](https://declaredesign.org/r/randomizr/reference/complete_rs.md),
[`strata_and_cluster_rs()`](https://declaredesign.org/r/randomizr/reference/strata_and_cluster_rs.md),
[`cluster_ra()`](https://declaredesign.org/r/randomizr/reference/cluster_ra.md),
[`cluster_rs_probabilities()`](https://declaredesign.org/r/randomizr/reference/cluster_rs_probabilities.md)

## Examples

``` r
# Ten clusters, of sizes 1 through 10
clusters <- rep(letters[1:10], times = 1:10)

S <- cluster_rs(clusters = clusters)
table(S, clusters)
#>    clusters
#> S    a  b  c  d  e  f  g  h  i  j
#>   0  0  2  0  0  5  0  7  8  9  0
#>   1  1  0  3  4  0  6  0  0  0 10

S <- cluster_rs(clusters = clusters, n = 4)
table(S, clusters)
#>    clusters
#> S    a  b  c  d  e  f  g  h  i  j
#>   0  0  2  0  4  5  6  7  0  9  0
#>   1  1  0  3  0  0  0  0  8  0 10

# Each cluster drawn independently, so the number sampled varies
S <- cluster_rs(clusters = clusters, prob = 0.4, simple = TRUE)
table(S, clusters)
#>    clusters
#> S    a  b  c  d  e  f  g  h  i  j
#>   0  1  2  3  4  5  0  7  0  9 10
#>   1  0  0  0  0  0  6  0  8  0  0
```
