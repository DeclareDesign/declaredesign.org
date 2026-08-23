# probabilities of assignment: Cluster Random Assignment

probabilities of assignment: Cluster Random Assignment

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

  A vector of length N that indicates which cluster each unit belongs
  to.

- m:

  Use for a two-arm design in which m clusters are assigned to treatment
  and N_clusters-m clusters are assigned to control. (optional)

- m_unit:

  Use for a two-arm design in which exactly unique(m_unit) clusters are
  assigned to treatment and the remainder are assigned to control.
  m_unit must be of length N and must be the same for all units
  (optional)

- m_each:

  Use for a multi-arm design in which the values of m_each determine the
  number of clusters assigned to each condition. m_each must be a
  numeric vector in which each entry is a nonnegative integer that
  describes how many clusters should be assigned to the 1st, 2nd, 3rd...
  treatment condition. m_each must sum to N. (optional)

- prob:

  Use for a two-arm design in which either floor(N_clusters\*prob) or
  ceiling(N_clusters\*prob) clusters are assigned to treatment. The
  probability of assignment to treatment is exactly prob because with
  probability 1-prob, floor(N_clusters\*prob) clusters will be assigned
  to treatment and with probability prob, ceiling(N_clusters\*prob)
  clusters will be assigned to treatment. prob must be a real number
  between 0 and 1 inclusive. (optional)

- prob_unit:

  Use for a two-arm design. unique(prob_unit) will be passed to the prob
  argument and must be the same for all units.

- prob_each:

  Use for a multi-arm design in which the values of prob_each determine
  the probabilities of assignment to each treatment condition. prob_each
  must be a numeric vector giving the probability of assignment to each
  condition. All entries must be nonnegative real numbers between 0 and
  1 inclusive and the total must sum to 1. Because of integer issues,
  the exact number of clusters assigned to each condition may differ
  (slightly) from assignment to assignment, but the overall probability
  of assignment is exactly prob_each. (optional)

- num_arms:

  The total number of treatment arms. If unspecified, will be determined
  from the length of m_each or conditions.

- conditions:

  A character vector giving the names of the treatment groups. If
  unspecified, the treatment groups will be named T1, T2, T3, etc.

- simple:

  logical, defaults to FALSE. If TRUE, simple random assignment of
  clusters to conditions is used. When simple = TRUE, please do not
  specify m or m_each.

- check_inputs:

  logical. Defaults to TRUE.

## Value

A matrix of probabilities of assignment

## Examples

``` r

# Two Group Designs
clusters <- rep(letters, times = 1:26)
prob_mat <- cluster_ra_probabilities(clusters = clusters)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.5    0.5
#> [2,]    0.5    0.5
#> [3,]    0.5    0.5
#> [4,]    0.5    0.5
#> [5,]    0.5    0.5
#> [6,]    0.5    0.5

prob_mat <- cluster_ra_probabilities(clusters = clusters, m = 10)
head(prob_mat)
#>         prob_0    prob_1
#> [1,] 0.6153846 0.3846154
#> [2,] 0.6153846 0.3846154
#> [3,] 0.6153846 0.3846154
#> [4,] 0.6153846 0.3846154
#> [5,] 0.6153846 0.3846154
#> [6,] 0.6153846 0.3846154

prob_mat <- cluster_ra_probabilities(clusters = clusters,
                                     m_each = c(9, 17),
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

prob_mat <- cluster_ra_probabilities(clusters = clusters, m_each = c(7, 7, 12))
head(prob_mat)
#>        prob_T1   prob_T2   prob_T3
#> [1,] 0.2692308 0.2692308 0.4615385
#> [2,] 0.2692308 0.2692308 0.4615385
#> [3,] 0.2692308 0.2692308 0.4615385
#> [4,] 0.2692308 0.2692308 0.4615385
#> [5,] 0.2692308 0.2692308 0.4615385
#> [6,] 0.2692308 0.2692308 0.4615385

prob_mat <- cluster_ra_probabilities(clusters = clusters, m_each = c(7, 7, 12),
                         conditions=c("control", "placebo", "treatment"))
head(prob_mat)
#>      prob_control prob_placebo prob_treatment
#> [1,]    0.2692308    0.2692308      0.4615385
#> [2,]    0.2692308    0.2692308      0.4615385
#> [3,]    0.2692308    0.2692308      0.4615385
#> [4,]    0.2692308    0.2692308      0.4615385
#> [5,]    0.2692308    0.2692308      0.4615385
#> [6,]    0.2692308    0.2692308      0.4615385

prob_mat <- cluster_ra_probabilities(clusters = clusters,
                         conditions=c("control", "placebo", "treatment"))
head(prob_mat)
#>      prob_control prob_placebo prob_treatment
#> [1,]    0.3333333    0.3333333      0.3333333
#> [2,]    0.3333333    0.3333333      0.3333333
#> [3,]    0.3333333    0.3333333      0.3333333
#> [4,]    0.3333333    0.3333333      0.3333333
#> [5,]    0.3333333    0.3333333      0.3333333
#> [6,]    0.3333333    0.3333333      0.3333333

prob_mat <- cluster_ra_probabilities(clusters = clusters,
                                     prob_each = c(.1, .2, .7))
head(prob_mat)
#>      prob_T1 prob_T2 prob_T3
#> [1,]     0.1     0.2     0.7
#> [2,]     0.1     0.2     0.7
#> [3,]     0.1     0.2     0.7
#> [4,]     0.1     0.2     0.7
#> [5,]     0.1     0.2     0.7
#> [6,]     0.1     0.2     0.7


```
