# Cluster Random Sampling

cluster_rs implements a random sampling procedure in which groups of
units are sampled together (as a cluster). This function conducts
complete random sampling at the cluster level, unless simple = TRUE, in
which case
[`simple_rs`](https://declaredesign.org/r/randomizr/reference/simple_rs.md)
analogues are used.

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

A numeric vector of length N that indicates if a unit is sampled (1) or
not (0).

## Examples

``` r
clusters <- rep(letters, times=1:26)

S <- cluster_rs(clusters = clusters)
table(S, clusters)
#>    clusters
#> S    a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y
#>   0  0  0  3  4  0  6  7  0  9  0 11 12  0  0 15  0  0 18 19 20  0  0  0  0 25
#>   1  1  2  0  0  5  0  0  8  0 10  0  0 13 14  0 16 17  0  0  0 21 22 23 24  0
#>    clusters
#> S    z
#>   0 26
#>   1  0

S <- cluster_rs(clusters = clusters, n = 13)
table(S, clusters)
#>    clusters
#> S    a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y
#>   0  1  0  3  4  5  0  7  8  9  0  0 12  0 14  0 16  0 18  0  0 21  0 23  0  0
#>   1  0  2  0  0  0  6  0  0  0 10 11  0 13  0 15  0 17  0 19 20  0 22  0 24 25
#>    clusters
#> S    z
#>   0  0
#>   1 26
```
