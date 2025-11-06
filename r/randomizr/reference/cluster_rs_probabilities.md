# Inclusion Probabilities: Cluster Sampling

Inclusion Probabilities: Cluster Sampling

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

  A vector of length N that indicates which cluster each unit belongs
  to.

- n:

  Use for a design in which n clusters are sampled. (optional)

- n_unit:

  unique(n_unit) will be passed to `n`. Must be the same for all units
  (optional)

- prob:

  Use for a design in which either floor(N_clusters\*prob) or
  ceiling(N_clusters\*prob) clusters are sampled. The probability of
  being sampled is exactly prob because with probability 1-prob,
  floor(N_clusters\*prob) clusters will be sampled and with probability
  prob, ceiling(N_clusters\*prob) clusters will be sampled. prob must be
  a real number between 0 and 1 inclusive. (optional)

- prob_unit:

  unique(prob_unit) will be passed to the prob argument and must be the
  same for all units.

- simple:

  logical, defaults to FALSE. If TRUE, simple random sampling of
  clusters. When simple = TRUE, please do not specify n.

- check_inputs:

  logical. Defaults to TRUE.

## Value

A vector length N indicating the probability of being sampled.

## Examples

``` r
# Two Group Designs
clusters <- rep(letters, times = 1:26)
probs <- cluster_rs_probabilities(clusters = clusters)
table(probs, clusters)
#>      clusters
#> probs  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x
#>   0.5  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
#>      clusters
#> probs  y  z
#>   0.5 25 26

prob_mat <- cluster_rs_probabilities(clusters = clusters, n = 10)
table(probs, clusters)
#>      clusters
#> probs  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x
#>   0.5  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
#>      clusters
#> probs  y  z
#>   0.5 25 26

prob_mat <- cluster_rs_probabilities(clusters = clusters, prob = .3)
table(probs, clusters)
#>      clusters
#> probs  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x
#>   0.5  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
#>      clusters
#> probs  y  z
#>   0.5 25 26

```
