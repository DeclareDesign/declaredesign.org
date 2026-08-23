# Stratified and Clustered Random Sampling

A random sampling procedure in which units are sampled as clusters and
clusters are nested within strata.

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

  A vector of length N that indicates which stratum each unit belongs
  to.

- clusters:

  A vector of length N that indicates which cluster each unit belongs
  to.

- prob:

  Use for a design in which either floor(N_clusters_stratum\*prob) or
  ceiling(N_clusters_stratum\*prob) clusters are sampled within each
  stratum. The probability of being sampled is exactly prob because with
  probability 1-prob, floor(N_clusters_stratum\*prob) clusters will be
  sampled and with probability prob, ceiling(N_clusters_stratum\*prob)
  clusters will be sampled. prob must be a real number between 0 and 1
  inclusive. (optional)

- prob_unit:

  Must of be of length N. tapply(prob_unit, blocks, unique) will be
  passed to `strata_prob`.

- n:

  Use for a design in which the scalar n describes the fixed number of
  units to sample in each stratum. This number does not vary across
  strata.

- n_unit:

  Must be of length N. tapply(m_unit, blocks, unique) will be passed to
  `strata_n`.

- strata_n:

  Use for a design in which strata_n describes the number of units to
  sample within each stratum.

- strata_prob:

  Use for a design in which strata_prob describes the probability of
  being sampled within each stratum. Differs from prob in that the
  probability of being sampled can vary across strata.

- check_inputs:

  logical. Defaults to TRUE.

## Value

A numeric vector of length N that indicates if a unit is sampled (1) or
not (0).

## Examples

``` r
clusters <- rep(letters, times = 1:26)

strata <- rep(NA, length(clusters))
strata[clusters %in% letters[1:5]] <- "stratum_1"
strata[clusters %in% letters[6:10]] <- "stratum_2"
strata[clusters %in% letters[11:15]] <- "stratum_3"
strata[clusters %in% letters[16:20]] <- "stratum_4"
strata[clusters %in% letters[21:26]] <- "stratum_5"

table(strata, clusters)
#>            clusters
#> strata       a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v
#>   stratum_1  1  2  3  4  5  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   stratum_2  0  0  0  0  0  6  7  8  9 10  0  0  0  0  0  0  0  0  0  0  0  0
#>   stratum_3  0  0  0  0  0  0  0  0  0  0 11 12 13 14 15  0  0  0  0  0  0  0
#>   stratum_4  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 16 17 18 19 20  0  0
#>   stratum_5  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 21 22
#>            clusters
#> strata       w  x  y  z
#>   stratum_1  0  0  0  0
#>   stratum_2  0  0  0  0
#>   stratum_3  0  0  0  0
#>   stratum_4  0  0  0  0
#>   stratum_5 23 24 25 26

S <- strata_and_cluster_rs(strata = strata,
                          clusters = clusters)

table(S, strata)
#>    strata
#> S   stratum_1 stratum_2 stratum_3 stratum_4 stratum_5
#>   0         6        27        27        53        70
#>   1         9        13        38        37        71
table(S, clusters)
#>    clusters
#> S    a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y
#>   0  1  2  3  0  0  0  0  8  9 10  0  0 13 14  0 16 17  0  0 20 21  0  0 24 25
#>   1  0  0  0  4  5  6  7  0  0  0 11 12  0  0 15  0  0 18 19  0  0 22 23  0  0
#>    clusters
#> S    z
#>   0  0
#>   1 26


S <- strata_and_cluster_rs(clusters = clusters,
                           strata = strata,
                           prob = .5)

table(S, clusters)
#>    clusters
#> S    a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y
#>   0  1  0  0  4  5  0  7  8  0 10 11 12  0  0  0 16 17  0  0 20 21  0 23  0 25
#>   1  0  2  3  0  0  6  0  0  9  0  0  0 13 14 15  0  0 18 19  0  0 22  0 24  0
#>    clusters
#> S    z
#>   0  0
#>   1 26
table(S, strata)
#>    strata
#> S   stratum_1 stratum_2 stratum_3 stratum_4 stratum_5
#>   0        10        25        23        53        69
#>   1         5        15        42        37        72

S <- strata_and_cluster_rs(clusters = clusters,
                           strata = strata,
                           strata_n = c(2, 3, 2, 3, 2))

table(S, clusters)
#>    clusters
#> S    a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y
#>   0  0  0  3  4  5  0  7  8  0  0  0 12 13  0 15  0 17 18  0  0  0 22 23 24 25
#>   1  1  2  0  0  0  6  0  0  9 10 11  0  0 14  0 16  0  0 19 20 21  0  0  0  0
#>    clusters
#> S    z
#>   0  0
#>   1 26
table(S, strata)
#>    strata
#> S   stratum_1 stratum_2 stratum_3 stratum_4 stratum_5
#>   0        12        15        40        35        94
#>   1         3        25        25        55        47

S <- strata_and_cluster_rs(clusters = clusters,
                           strata = strata,
                           strata_prob = c(.1, .2, .3, .4, .5))

table(S, clusters)
#>    clusters
#> S    a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y
#>   0  1  2  3  4  5  0  7  8  9 10 11  0 13 14 15  0 17 18 19  0  0 22  0 24  0
#>   1  0  0  0  0  0  6  0  0  0  0  0 12  0  0  0 16  0  0  0 20 21  0 23  0 25
#>    clusters
#> S    z
#>   0 26
#>   1  0
table(S, strata)
#>    strata
#> S   stratum_1 stratum_2 stratum_3 stratum_4 stratum_5
#>   0        15        34        53        54        72
#>   1         0         6        12        36        69

```
