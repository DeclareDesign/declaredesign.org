# Blocked and Clustered Random Assignment

`block_and_cluster_ra` assigns whole clusters to conditions, conducting
the assignment separately within each block. Use it when treatment can
only be delivered to a group and the groups differ in ways worth
balancing on. Clustering costs precision, since the effective sample
size is the number of clusters rather than the number of units; blocking
buys some of it back by guaranteeing treated and control clusters within
every block.

## Usage

``` r
block_and_cluster_ra(
  blocks = NULL,
  clusters = NULL,
  prob = NULL,
  prob_unit = NULL,
  prob_each = NULL,
  m = NULL,
  m_unit = NULL,
  block_m = NULL,
  block_m_each = NULL,
  block_prob = NULL,
  block_prob_each = NULL,
  num_arms = NULL,
  conditions = NULL,
  check_inputs = TRUE
)
```

## Arguments

- blocks:

  A vector of length N indicating which block each unit belongs to.
  Every unit in a cluster must belong to the same block. (required)

- clusters:

  A vector of length N indicating which cluster each unit belongs to.
  (required)

- prob:

  Use for a two-arm design in which either
  `floor(N_clusters_block*prob)` or `ceiling(N_clusters_block*prob)`
  clusters are assigned to treatment within each block. Which of the two
  is used is itself random: the ceiling is drawn with probability equal
  to the fractional part of `N_clusters_block*prob` and the floor
  otherwise, which makes each cluster's probability of assignment
  exactly `prob`. Must be a real number between 0 and 1 inclusive.
  (optional)

- prob_unit:

  Use for a two-arm design. Must be of length N.
  `tapply(prob_unit, blocks, unique)` will be passed to `block_prob`, so
  it must be constant within each block. (optional)

- prob_each:

  Use for a multi-arm design. A numeric vector giving the probability of
  assignment to each condition. All entries must be between 0 and 1
  inclusive and must sum to 1. Because of integer rounding, the exact
  number of clusters assigned to each condition may differ slightly from
  assignment to assignment, but the overall probability of assignment is
  exactly `prob_each`. (optional)

- m:

  Use for a two-arm design in which the scalar `m` gives the fixed
  number of clusters assigned to treatment within every block. This
  count does not vary across blocks. (optional)

- m_unit:

  Use for a two-arm design. Must be of length N.
  `tapply(m_unit, blocks, unique)` will be passed to `block_m`, so it
  must be constant within each block. (optional)

- block_m:

  Use for a two-arm design in which `block_m` gives the number of
  clusters to assign to treatment within each block. Must be a numeric
  vector as long as the number of blocks, in the same order as
  `sort(unique(blocks))`. (optional)

- block_m_each:

  Use for a multi-arm design in which `block_m_each` gives the number of
  clusters assigned to each condition within each block. Must be a
  matrix with one row per block and one column per treatment arm. Rows
  respect the ordering of blocks by `sort(unique(blocks))`; columns
  should be in the order of `conditions`, if specified. (optional)

- block_prob:

  Use for a two-arm design in which the probability of assignment to
  treatment varies across blocks. Must be in the same order as
  `sort(unique(blocks))`. Differs from `prob` in that the probability of
  assignment can vary across blocks. (optional)

- block_prob_each:

  Use for a multi-arm design in which assignment probabilities vary
  across blocks. Must be a matrix with one row per block and one column
  per treatment arm; each row must sum to 1. Rows respect the ordering
  of `sort(unique(blocks))`. Use only if the probabilities of assignment
  should vary by block, otherwise use `prob_each`. (optional)

- num_arms:

  The number of treatment arms. If unspecified, determined from the
  other arguments. (optional)

- conditions:

  A character vector giving the names of the treatment groups. If
  unspecified, the treatment groups will be named 0 (for control) and 1
  (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm
  trial. A two-group design in which `num_arms` is set to 2 will use
  condition names T1 and T2. (optional)

- check_inputs:

  Logical. Whether to verify before assigning that the arguments are
  internally consistent: that clusters nest within blocks, that counts
  sum to the number of clusters in each block, that probabilities lie
  between 0 and 1 and sum to 1, and so on. Defaults to `TRUE`. `FALSE`
  skips the checking only: `num_arms` and `conditions` are still derived
  from the other arguments, so the same call draws the same assignment
  either way. What goes is the verification, and an impossible design is
  then no longer refused. `block_m` larger than a block, for instance,
  quietly treats the whole block. Declaring the design once with
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

Clusters must nest within blocks: every unit in a cluster has to belong
to the same block.

## See also

[`cluster_ra()`](https://declaredesign.org/r/randomizr/reference/cluster_ra.md),
[`block_ra()`](https://declaredesign.org/r/randomizr/reference/block_ra.md),
[`strata_and_cluster_rs()`](https://declaredesign.org/r/randomizr/reference/strata_and_cluster_rs.md)

## Examples

``` r
# Twelve clusters, of sizes 1 through 12, nested in four blocks of three
clusters <- rep(letters[1:12], times = 1:12)

blocks <- rep(NA, length(clusters))
blocks[clusters %in% letters[1:3]] <- "block_1"
blocks[clusters %in% letters[4:6]] <- "block_2"
blocks[clusters %in% letters[7:9]] <- "block_3"
blocks[clusters %in% letters[10:12]] <- "block_4"


table(blocks, clusters)
#>          clusters
#> blocks     a  b  c  d  e  f  g  h  i  j  k  l
#>   block_1  1  2  3  0  0  0  0  0  0  0  0  0
#>   block_2  0  0  0  4  5  6  0  0  0  0  0  0
#>   block_3  0  0  0  0  0  0  7  8  9  0  0  0
#>   block_4  0  0  0  0  0  0  0  0  0 10 11 12

Z <- block_and_cluster_ra(blocks = blocks,
                          clusters = clusters)

table(Z, blocks)
#>    blocks
#> Z   block_1 block_2 block_3 block_4
#>   0       5       9       9      22
#>   1       1       6      15      11
table(Z, clusters)
#>    clusters
#> Z    a  b  c  d  e  f  g  h  i  j  k  l
#>   0  0  2  3  4  5  0  0  0  9 10  0 12
#>   1  1  0  0  0  0  6  7  8  0  0 11  0

Z <- block_and_cluster_ra(blocks = blocks,
                          clusters = clusters,
                          num_arms = 3)

table(Z, blocks)
#>     blocks
#> Z    block_1 block_2 block_3 block_4
#>   T1       3       4       7      12
#>   T2       1       6       8      10
#>   T3       2       5       9      11
table(Z, clusters)
#>     clusters
#> Z     a  b  c  d  e  f  g  h  i  j  k  l
#>   T1  0  0  3  4  0  0  7  0  0  0  0 12
#>   T2  1  0  0  0  0  6  0  8  0 10  0  0
#>   T3  0  2  0  0  5  0  0  0  9  0 11  0

Z <- block_and_cluster_ra(blocks = blocks,
                          clusters = clusters,
                          prob_each = c(0.2, 0.5, 0.3))

# One row per block, one column per arm: how many clusters go where
block_m_each <- rbind(c(1, 2),
                      c(2, 1),
                      c(1, 2),
                      c(2, 1))

Z <- block_and_cluster_ra(blocks = blocks,
                          clusters = clusters,
                          block_m_each = block_m_each)

table(Z, blocks)
#>    blocks
#> Z   block_1 block_2 block_3 block_4
#>   0       3      10       8      21
#>   1       3       5      16      12
table(Z, clusters)
#>    clusters
#> Z    a  b  c  d  e  f  g  h  i  j  k  l
#>   0  0  0  3  4  0  6  0  8  0 10 11  0
#>   1  1  2  0  0  5  0  7  0  9  0  0 12
```
