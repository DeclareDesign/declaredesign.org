# Probabilities of assignment: Blocked and Clustered Random Assignment

Returns the probability that each unit is assigned to each condition
when clusters are assigned within blocks. Probabilities vary across
blocks and are constant within a cluster.

## Usage

``` r
block_and_cluster_ra_probabilities(
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

A matrix with N rows and one column per treatment condition, with
columns named `prob_<condition>`. Entry (i, j) is the probability that
unit i is assigned to condition j, and every row sums to 1.

## Details

These are the quantities inverse-probability weights are built from:
weight each unit by the reciprocal of the probability of the condition
it landed in, which
[`obtain_condition_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_condition_probabilities.md)
extracts for you.

## See also

[`block_and_cluster_ra()`](https://declaredesign.org/r/randomizr/reference/block_and_cluster_ra.md)

## Examples

``` r

clusters <- rep(letters[1:12], times = 1:12)
blocks <- rep(NA, length(clusters))
blocks[clusters %in% letters[1:3]] <- "block_1"
blocks[clusters %in% letters[4:6]] <- "block_2"
blocks[clusters %in% letters[7:9]] <- "block_3"
blocks[clusters %in% letters[10:12]] <- "block_4"


prob_mat <- block_and_cluster_ra_probabilities(clusters = clusters,
                                               blocks = blocks)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.5    0.5
#> [2,]    0.5    0.5
#> [3,]    0.5    0.5
#> [4,]    0.5    0.5
#> [5,]    0.5    0.5
#> [6,]    0.5    0.5
                                    
prob_mat <- block_and_cluster_ra_probabilities(clusters = clusters,
                                               blocks = blocks,
                                               num_arms = 3)
head(prob_mat)
#>        prob_T1   prob_T2   prob_T3
#> [1,] 0.3333333 0.3333333 0.3333333
#> [2,] 0.3333333 0.3333333 0.3333333
#> [3,] 0.3333333 0.3333333 0.3333333
#> [4,] 0.3333333 0.3333333 0.3333333
#> [5,] 0.3333333 0.3333333 0.3333333
#> [6,] 0.3333333 0.3333333 0.3333333
                                    
prob_mat <- block_and_cluster_ra_probabilities(clusters = clusters,
                                               blocks = blocks,
                                               prob_each = c(0.2, 0.5, 0.3))
head(prob_mat)                                    
#>      prob_T1 prob_T2 prob_T3
#> [1,]     0.2     0.5     0.3
#> [2,]     0.2     0.5     0.3
#> [3,]     0.2     0.5     0.3
#> [4,]     0.2     0.5     0.3
#> [5,]     0.2     0.5     0.3
#> [6,]     0.2     0.5     0.3

# One row per block, one column per arm: how many clusters go where
block_m_each <- rbind(c(1, 2),
                      c(2, 1),
                      c(1, 2),
                      c(2, 1))

prob_mat <- block_and_cluster_ra_probabilities(clusters = clusters, 
                                               blocks = blocks, 
                                               block_m_each = block_m_each)
head(prob_mat)                                    
#>         prob_0    prob_1
#> [1,] 0.3333333 0.6666667
#> [2,] 0.3333333 0.6666667
#> [3,] 0.3333333 0.6666667
#> [4,] 0.3333333 0.6666667
#> [5,] 0.3333333 0.6666667
#> [6,] 0.3333333 0.6666667

```
