# Cluster Random Assignment

cluster_ra implements a random assignment procedure in which groups of
units are assigned together (as a cluster) to treatment conditions. This
function conducts complete random assignment at the cluster level,
unless simple = TRUE, in which case
[`simple_ra`](https://declaredesign.org/r/randomizr/reference/simple_ra.md)
analogues are used.

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

A vector of length N that indicates the treatment condition of each
unit.

## Examples

``` r
# Two Group Designs
clusters <- rep(letters, times=1:26)

Z <- cluster_ra(clusters = clusters)
table(Z, clusters)
#>    clusters
#> Z    a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y
#>   0  1  2  0  4  5  0  0  8  9  0  0 12  0  0  0  0  0 18  0 20  0 22 23  0 25
#>   1  0  0  3  0  0  6  7  0  0 10 11  0 13 14 15 16 17  0 19  0 21  0  0 24  0
#>    clusters
#> Z    z
#>   0 26
#>   1  0

Z <- cluster_ra(clusters = clusters, m = 13)
table(Z, clusters)
#>    clusters
#> Z    a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y
#>   0  0  2  3  0  5  0  7  0  0  0 11  0 13 14 15 16 17  0 19  0  0  0  0 24  0
#>   1  1  0  0  4  0  6  0  8  9 10  0 12  0  0  0  0  0 18  0 20 21 22 23  0 25
#>    clusters
#> Z    z
#>   0 26
#>   1  0

Z <- cluster_ra(clusters = clusters, m_each = c(10, 16),
                conditions = c("control", "treatment"))
table(Z, clusters)
#>            clusters
#> Z            a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v
#>   control    0  0  3  0  0  0  7  8  9  0 11  0 13  0  0  0 17 18  0 20  0  0
#>   treatment  1  2  0  4  5  6  0  0  0 10  0 12  0 14 15 16  0  0 19  0 21 22
#>            clusters
#> Z            w  x  y  z
#>   control    0 24  0  0
#>   treatment 23  0 25 26

# Multi-arm Designs
Z <- cluster_ra(clusters = clusters, num_arms = 3)
table(Z, clusters)
#>     clusters
#> Z     a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y
#>   T1  0  0  0  4  0  0  7  0  9  0  0  0  0  0 15  0 17  0 19  0  0 22 23  0 25
#>   T2  1  2  0  0  0  6  0  0  0  0  0 12 13 14  0 16  0 18  0  0  0  0  0  0  0
#>   T3  0  0  3  0  5  0  0  8  0 10 11  0  0  0  0  0  0  0  0 20 21  0  0 24  0
#>     clusters
#> Z     z
#>   T1  0
#>   T2 26
#>   T3  0

Z <- cluster_ra(clusters = clusters, m_each = c(7, 7, 12))
table(Z, clusters)
#>     clusters
#> Z     a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y
#>   T1  0  0  3  0  5  0  0  0  0  0  0  0 13  0 15  0  0  0 19  0  0  0 23 24  0
#>   T2  0  2  0  4  0  0  0  8  0  0  0 12  0  0  0  0  0  0  0  0 21  0  0  0 25
#>   T3  1  0  0  0  0  6  7  0  9 10 11  0  0 14  0 16 17 18  0 20  0 22  0  0  0
#>     clusters
#> Z     z
#>   T1  0
#>   T2 26
#>   T3  0

Z <- cluster_ra(clusters = clusters, m_each = c(7, 7, 12),
                conditions = c("control", "placebo", "treatment"))
table(Z, clusters)
#>            clusters
#> Z            a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v
#>   control    0  0  0  4  0  6  0  8  0  0 11  0 13  0  0  0  0  0  0  0  0 22
#>   placebo    0  0  0  0  0  0  7  0  9  0  0  0  0  0 15  0  0  0  0 20  0  0
#>   treatment  1  2  3  0  5  0  0  0  0 10  0 12  0 14  0 16 17 18 19  0 21  0
#>            clusters
#> Z            w  x  y  z
#>   control   23  0  0  0
#>   placebo    0 24 25 26
#>   treatment  0  0  0  0

Z <- cluster_ra(clusters = clusters,
                conditions = c("control", "placebo", "treatment"))
table(Z, clusters)
#>            clusters
#> Z            a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v
#>   control    0  0  0  0  0  6  7  8  9  0  0  0  0 14  0  0  0  0  0 20  0 22
#>   placebo    1  2  3  4  0  0  0  0  0  0 11  0 13  0  0  0  0  0  0  0  0  0
#>   treatment  0  0  0  0  5  0  0  0  0 10  0 12  0  0 15 16 17 18 19  0 21  0
#>            clusters
#> Z            w  x  y  z
#>   control    0  0 25 26
#>   placebo   23 24  0  0
#>   treatment  0  0  0  0
```
