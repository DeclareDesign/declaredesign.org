# Probabilities of assignment: Cluster Random Assignment

Returns the probability that each unit is assigned to each condition
under cluster random assignment. Every unit in a cluster shares its
cluster's probability, since clusters move together.

## Usage

``` r
cluster_ra_probabilities(
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

[`cluster_ra()`](https://declaredesign.org/r/randomizr/reference/cluster_ra.md)

## Examples

``` r

# Two Group Designs
clusters <- rep(letters[1:10], times = 1:10)
prob_mat <- cluster_ra_probabilities(clusters = clusters)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.5    0.5
#> [2,]    0.5    0.5
#> [3,]    0.5    0.5
#> [4,]    0.5    0.5
#> [5,]    0.5    0.5
#> [6,]    0.5    0.5

prob_mat <- cluster_ra_probabilities(clusters = clusters, m = 4)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.6    0.4
#> [2,]    0.6    0.4
#> [3,]    0.6    0.4
#> [4,]    0.6    0.4
#> [5,]    0.6    0.4
#> [6,]    0.6    0.4

prob_mat <- cluster_ra_probabilities(clusters = clusters,
                                     m_each = c(6, 4),
                                     conditions = c("control", "treatment"))

# Multi-arm Designs
prob_mat <- cluster_ra_probabilities(clusters = clusters, num_arms = 3)
head(prob_mat)
#>        prob_T1   prob_T2   prob_T3
#> [1,] 0.3333333 0.3333333 0.3333333
#> [2,] 0.3333333 0.3333333 0.3333333
#> [3,] 0.3333333 0.3333333 0.3333333
#> [4,] 0.3333333 0.3333333 0.3333333
#> [5,] 0.3333333 0.3333333 0.3333333
#> [6,] 0.3333333 0.3333333 0.3333333

prob_mat <- cluster_ra_probabilities(clusters = clusters, m_each = c(3, 3, 4))
head(prob_mat)
#>      prob_T1 prob_T2 prob_T3
#> [1,]     0.3     0.3     0.4
#> [2,]     0.3     0.3     0.4
#> [3,]     0.3     0.3     0.4
#> [4,]     0.3     0.3     0.4
#> [5,]     0.3     0.3     0.4
#> [6,]     0.3     0.3     0.4

prob_mat <- cluster_ra_probabilities(clusters = clusters, m_each = c(3, 3, 4),
                         conditions = c("control", "placebo", "treatment"))
head(prob_mat)
#>      prob_control prob_placebo prob_treatment
#> [1,]          0.3          0.3            0.4
#> [2,]          0.3          0.3            0.4
#> [3,]          0.3          0.3            0.4
#> [4,]          0.3          0.3            0.4
#> [5,]          0.3          0.3            0.4
#> [6,]          0.3          0.3            0.4

prob_mat <- cluster_ra_probabilities(clusters = clusters,
                         conditions = c("control", "placebo", "treatment"))
head(prob_mat)
#>      prob_control prob_placebo prob_treatment
#> [1,]    0.3333333    0.3333333      0.3333333
#> [2,]    0.3333333    0.3333333      0.3333333
#> [3,]    0.3333333    0.3333333      0.3333333
#> [4,]    0.3333333    0.3333333      0.3333333
#> [5,]    0.3333333    0.3333333      0.3333333
#> [6,]    0.3333333    0.3333333      0.3333333

prob_mat <- cluster_ra_probabilities(clusters = clusters,
                                     prob_each = c(0.1, 0.2, 0.7))
head(prob_mat)
#>      prob_T1 prob_T2 prob_T3
#> [1,]     0.1     0.2     0.7
#> [2,]     0.1     0.2     0.7
#> [3,]     0.1     0.2     0.7
#> [4,]     0.1     0.2     0.7
#> [5,]     0.1     0.2     0.7
#> [6,]     0.1     0.2     0.7


```
