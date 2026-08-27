# Stratified and Clustered Random Sampling

`strata_and_cluster_rs` draws whole clusters, sampling separately within
each stratum. Use it when the sampling unit is a group rather than an
individual and the groups fall into categories you want represented in
fixed proportion. Sampling by cluster costs precision, since the
effective sample size is the number of clusters rather than the number
of units; stratifying buys some of it back by fixing how many clusters
come from each stratum.

## Usage

``` r
strata_and_cluster_rs(
  strata = NULL,
  clusters = NULL,
  prob = NULL,
  prob_unit = NULL,
  n = NULL,
  n_unit = NULL,
  strata_n = NULL,
  strata_prob = NULL,
  check_inputs = TRUE
)
```

## Arguments

- strata:

  A vector of length N indicating which stratum each unit belongs to.
  Every unit in a cluster must belong to the same stratum. (required)

- clusters:

  A vector of length N indicating which cluster each unit belongs to.
  (required)

- prob:

  Use for a design in which either `floor(N_clusters_stratum*prob)` or
  `ceiling(N_clusters_stratum*prob)` clusters are sampled within each
  stratum. Which of the two is used is itself random: the ceiling is
  drawn with probability equal to the fractional part of
  `N_clusters_stratum*prob` and the floor otherwise, which makes each
  cluster's probability of inclusion exactly `prob`. Must be a real
  number between 0 and 1 inclusive. (optional)

- prob_unit:

  Must be of length N. `tapply(prob_unit, strata, unique)` will be
  passed to `strata_prob`, so it must be constant within each stratum.
  (optional)

- n:

  Use for a design in which the scalar `n` gives the fixed number of
  clusters to sample in every stratum. This count does not vary across
  strata. (optional)

- n_unit:

  Must be of length N. `tapply(n_unit, strata, unique)` will be passed
  to `strata_n`, so it must be constant within each stratum. (optional)

- strata_n:

  Use for a design in which `strata_n` gives the number of clusters to
  sample within each stratum. Must be as long as the number of strata,
  in the same order as `sort(unique(strata))`. (optional)

- strata_prob:

  Use for a design in which `strata_prob` gives the probability of being
  sampled within each stratum. Must be in the same order as
  `sort(unique(strata))`. Differs from `prob` in that the probability of
  being sampled can vary across strata. (optional)

- check_inputs:

  Logical. Whether to verify before sampling that the arguments are
  internally consistent: that clusters nest within strata, that counts
  do not exceed the number of clusters in a stratum, that probabilities
  lie between 0 and 1, and so on. Defaults to `TRUE`. Set to `FALSE` to
  skip the checks when drawing many samples from arguments that have
  already been verified; declaring the design once with
  [`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md)
  and drawing from it with
  [`draw_rs()`](https://declaredesign.org/r/randomizr/reference/draw_rs.md)
  does this for you. (optional)

## Value

A numeric vector of length N indicating whether each unit is sampled (1)
or not (0). Every unit in a cluster receives the same value.

## Details

Clusters must nest within strata: every unit in a cluster has to belong
to the same stratum.

## See also

[`cluster_rs()`](https://declaredesign.org/r/randomizr/reference/cluster_rs.md),
[`strata_rs()`](https://declaredesign.org/r/randomizr/reference/strata_rs.md),
[`block_and_cluster_ra()`](https://declaredesign.org/r/randomizr/reference/block_and_cluster_ra.md)

## Examples

``` r
# Twelve clusters, of sizes 1 through 12, nested in four strata of three
clusters <- rep(letters[1:12], times = 1:12)

strata <- rep(NA, length(clusters))
strata[clusters %in% letters[1:3]] <- "stratum_1"
strata[clusters %in% letters[4:6]] <- "stratum_2"
strata[clusters %in% letters[7:9]] <- "stratum_3"
strata[clusters %in% letters[10:12]] <- "stratum_4"

table(strata, clusters)
#>            clusters
#> strata       a  b  c  d  e  f  g  h  i  j  k  l
#>   stratum_1  1  2  3  0  0  0  0  0  0  0  0  0
#>   stratum_2  0  0  0  4  5  6  0  0  0  0  0  0
#>   stratum_3  0  0  0  0  0  0  7  8  9  0  0  0
#>   stratum_4  0  0  0  0  0  0  0  0  0 10 11 12

S <- strata_and_cluster_rs(strata = strata,
                          clusters = clusters)

table(S, strata)
#>    strata
#> S   stratum_1 stratum_2 stratum_3 stratum_4
#>   0         5         6         8        12
#>   1         1         9        16        21
table(S, clusters)
#>    clusters
#> S    a  b  c  d  e  f  g  h  i  j  k  l
#>   0  0  2  3  0  0  6  0  8  0  0  0 12
#>   1  1  0  0  4  5  0  7  0  9 10 11  0


S <- strata_and_cluster_rs(clusters = clusters,
                           strata = strata,
                           prob = 0.5)

table(S, clusters)
#>    clusters
#> S    a  b  c  d  e  f  g  h  i  j  k  l
#>   0  0  2  3  0  0  6  7  8  0  0  0 12
#>   1  1  0  0  4  5  0  0  0  9 10 11  0
table(S, strata)
#>    strata
#> S   stratum_1 stratum_2 stratum_3 stratum_4
#>   0         5         6        15        12
#>   1         1         9         9        21

S <- strata_and_cluster_rs(clusters = clusters,
                           strata = strata,
                           strata_n = c(1, 2, 1, 2))

table(S, clusters)
#>    clusters
#> S    a  b  c  d  e  f  g  h  i  j  k  l
#>   0  0  2  3  0  5  0  0  8  9 10  0  0
#>   1  1  0  0  4  0  6  7  0  0  0 11 12
table(S, strata)
#>    strata
#> S   stratum_1 stratum_2 stratum_3 stratum_4
#>   0         5         5        17        10
#>   1         1        10         7        23

S <- strata_and_cluster_rs(clusters = clusters,
                           strata = strata,
                           strata_prob = c(0.2, 0.4, 0.6, 0.8))

table(S, clusters)
#>    clusters
#> S    a  b  c  d  e  f  g  h  i  j  k  l
#>   0  1  2  3  0  5  6  0  8  0 10  0  0
#>   1  0  0  0  4  0  0  7  0  9  0 11 12
table(S, strata)
#>    strata
#> S   stratum_1 stratum_2 stratum_3 stratum_4
#>   0         6        11         8        10
#>   1         0         4        16        23

```
