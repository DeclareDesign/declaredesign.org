# Cluster Random Assignment

`cluster_ra` assigns entire groups of units (clusters) to treatment
conditions, so that all units within a cluster share the same treatment
status. Cluster assignment is appropriate when the intervention can only
be delivered at the group level (for example, a school-wide program that
cannot be withheld from individual students), when spillovers within
groups make individual-level assignment infeasible, or when the
treatment is itself defined as a group-level condition. Because all
units in a cluster move together, the effective sample size for
estimating average effects is the number of clusters, not the number of
units. Clustering therefore typically increases sampling variability
relative to complete or block random assignment; the precision loss
grows with the intra-cluster correlation in potential outcomes.

## Usage

``` r
cluster_ra(
  clusters = NULL,
  m = NULL,
  m_unit = NULL,
  m_each = NULL,
  prob = NULL,
  prob_unit = NULL,
  prob_each = NULL,
  num_arms = NULL,
  conditions = NULL,
  simple = FALSE,
  check_inputs = TRUE
)
```

## Arguments

- clusters:

  A vector of length N indicating which cluster each unit belongs to.
  (required)

- m:

  Use for a two-arm design in which exactly `m` clusters are assigned to
  treatment. (optional)

- m_unit:

  Use for a two-arm design. `unique(m_unit)` clusters are assigned to
  treatment; must be the same for all units and of length N. (optional)

- m_each:

  Use for a multi-arm design. A numeric vector giving the number of
  clusters assigned to each condition; must sum to the total number of
  clusters. (optional)

- prob:

  Use for a two-arm design in which either `floor(N_clusters*prob)` or
  `ceiling(N_clusters*prob)` clusters are assigned to treatment. Which
  of the two is used is itself random: the ceiling is drawn with
  probability equal to the fractional part of `N_clusters*prob` and the
  floor otherwise, so that each cluster's probability of assignment is
  exactly `prob`. When `N_clusters*prob` is a whole number the count is
  fixed. Must be between 0 and 1. (optional)

- prob_unit:

  Use for a two-arm design. `unique(prob_unit)` will be passed to the
  `prob` argument and must be the same for all units. (optional)

- prob_each:

  Use for a multi-arm design. A numeric vector giving the probability of
  assignment to each condition; entries must be nonnegative, sum to 1.
  Because of integer rounding, the exact number of clusters assigned to
  each condition may differ slightly from assignment to assignment, but
  the overall probability of assignment is exactly `prob_each`.
  (optional)

- num_arms:

  The total number of treatment arms. If unspecified, determined from
  `m_each` or `conditions`. (optional)

- conditions:

  A character vector giving the names of the treatment groups. If
  unspecified, groups will be named T1, T2, T3, etc. (optional)

- simple:

  Logical, defaults to `FALSE`. If `TRUE`, clusters are assigned to
  conditions independently (simple random assignment at the cluster
  level), so the number of treated clusters varies from draw to draw. Do
  not specify `m` or `m_each` when `simple = TRUE`. (optional)

- check_inputs:

  Logical. Whether to verify before assigning that the arguments are
  internally consistent: that counts sum to the number of clusters, that
  probabilities lie between 0 and 1 and sum to 1, and so on. Defaults to
  `TRUE`. `FALSE` skips the checking only: `num_arms` and `conditions`
  are still derived from the other arguments, so the same call draws the
  same assignment either way. What goes is the verification, and an
  impossible design is then no longer refused. `block_m` larger than a
  block, for instance, quietly treats the whole block. Declaring the
  design once with
  [`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md)
  and drawing from it with
  [`conduct_ra()`](https://declaredesign.org/r/randomizr/reference/conduct_ra.md)
  is the usual way to avoid re-checking the same arguments in a
  simulation. (optional)

## Value

A vector of length N indicating the treatment condition of each unit.
Every unit in a cluster receives the same value. Numeric in a two-arm
trial; a factor (ordered by `conditions`) in a multi-arm trial.

## Details

By default, `cluster_ra` conducts complete random assignment at the
cluster level: a fixed number of clusters are assigned to each condition
on every draw. Setting `simple = TRUE` switches to independent Bernoulli
assignment of clusters.

## See also

[`complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.md),
[`block_and_cluster_ra()`](https://declaredesign.org/r/randomizr/reference/block_and_cluster_ra.md),
[`cluster_rs()`](https://declaredesign.org/r/randomizr/reference/cluster_rs.md),
[`cluster_ra_probabilities()`](https://declaredesign.org/r/randomizr/reference/cluster_ra_probabilities.md)

## Examples

``` r
# Ten clusters, of sizes 1 through 10
clusters <- rep(letters[1:10], times = 1:10)

# Two Group Designs

Z <- cluster_ra(clusters = clusters)
table(Z, clusters)
#>    clusters
#> Z    a  b  c  d  e  f  g  h  i  j
#>   0  1  2  0  4  5  0  7  0  0  0
#>   1  0  0  3  0  0  6  0  8  9 10

Z <- cluster_ra(clusters = clusters, m = 4)
table(Z, clusters)
#>    clusters
#> Z    a  b  c  d  e  f  g  h  i  j
#>   0  1  2  3  4  0  0  0  8  0 10
#>   1  0  0  0  0  5  6  7  0  9  0

Z <- cluster_ra(clusters = clusters, m_each = c(6, 4),
                conditions = c("control", "treatment"))
table(Z, clusters)
#>            clusters
#> Z            a  b  c  d  e  f  g  h  i  j
#>   control    0  2  0  4  0  6  7  8  0 10
#>   treatment  1  0  3  0  5  0  0  0  9  0

# Multi-arm Designs
Z <- cluster_ra(clusters = clusters, num_arms = 3)
table(Z, clusters)
#>     clusters
#> Z     a  b  c  d  e  f  g  h  i  j
#>   T1  0  0  0  0  5  0  0  8  9  0
#>   T2  0  2  0  0  0  6  0  0  0 10
#>   T3  1  0  3  4  0  0  7  0  0  0

Z <- cluster_ra(clusters = clusters, m_each = c(3, 3, 4))
table(Z, clusters)
#>     clusters
#> Z     a  b  c  d  e  f  g  h  i  j
#>   T1  1  0  3  0  0  0  0  0  0 10
#>   T2  0  2  0  4  0  0  7  0  0  0
#>   T3  0  0  0  0  5  6  0  8  9  0

Z <- cluster_ra(clusters = clusters, m_each = c(3, 3, 4),
                conditions = c("control", "placebo", "treatment"))
table(Z, clusters)
#>            clusters
#> Z            a  b  c  d  e  f  g  h  i  j
#>   control    1  0  0  4  0  0  0  0  9  0
#>   placebo    0  2  0  0  5  6  0  0  0  0
#>   treatment  0  0  3  0  0  0  7  8  0 10

Z <- cluster_ra(clusters = clusters,
                conditions = c("control", "placebo", "treatment"))
table(Z, clusters)
#>            clusters
#> Z            a  b  c  d  e  f  g  h  i  j
#>   control    0  0  3  0  5  0  0  0  0 10
#>   placebo    0  0  0  4  0  6  0  8  0  0
#>   treatment  1  2  0  0  0  0  7  0  9  0
```
