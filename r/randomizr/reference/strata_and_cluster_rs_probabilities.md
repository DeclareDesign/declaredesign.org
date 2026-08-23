# Inclusion Probabilities: Stratified and Clustered Random Sampling

Inclusion Probabilities: Stratified and Clustered Random Sampling

## Usage

``` r
strata_and_cluster_rs_probabilities(
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

A vector length N indicating the probability of being sampled.

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

probs <- strata_and_cluster_rs_probabilities(strata = strata,
                                         clusters = clusters)

table(probs, strata)
#>      strata
#> probs stratum_1 stratum_2 stratum_3 stratum_4 stratum_5
#>   0.5        15        40        65        90       141
table(probs, clusters)
#>      clusters
#> probs  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x
#>   0.5  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
#>      clusters
#> probs  y  z
#>   0.5 25 26


probs <- strata_and_cluster_rs_probabilities(clusters = clusters,
                                         strata = strata,
                                         prob = .5)

table(probs, clusters)
#>      clusters
#> probs  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x
#>   0.5  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
#>      clusters
#> probs  y  z
#>   0.5 25 26
table(probs, strata)
#>      strata
#> probs stratum_1 stratum_2 stratum_3 stratum_4 stratum_5
#>   0.5        15        40        65        90       141

probs <- strata_and_cluster_rs_probabilities(clusters = clusters,
                                         strata = strata,
                                         strata_n = c(2, 3, 2, 3, 2))

table(probs, clusters)
#>                    clusters
#> probs                a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t
#>   0.333333333333333  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   0.4                1  2  3  4  5  0  0  0  0  0 11 12 13 14 15  0  0  0  0  0
#>   0.6                0  0  0  0  0  6  7  8  9 10  0  0  0  0  0 16 17 18 19 20
#>                    clusters
#> probs                u  v  w  x  y  z
#>   0.333333333333333 21 22 23 24 25 26
#>   0.4                0  0  0  0  0  0
#>   0.6                0  0  0  0  0  0
table(probs, strata)
#>                    strata
#> probs               stratum_1 stratum_2 stratum_3 stratum_4 stratum_5
#>   0.333333333333333         0         0         0         0       141
#>   0.4                      15         0        65         0         0
#>   0.6                       0        40         0        90         0

probs <- strata_and_cluster_rs_probabilities(clusters = clusters,
                                         strata = strata,
                                         strata_prob = c(.1, .2, .3, .4, .5))

table(probs, clusters)
#>      clusters
#> probs  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x
#>   0.1  1  2  3  4  5  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   0.2  0  0  0  0  0  6  7  8  9 10  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   0.3  0  0  0  0  0  0  0  0  0  0 11 12 13 14 15  0  0  0  0  0  0  0  0  0
#>   0.4  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 16 17 18 19 20  0  0  0  0
#>   0.5  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 21 22 23 24
#>      clusters
#> probs  y  z
#>   0.1  0  0
#>   0.2  0  0
#>   0.3  0  0
#>   0.4  0  0
#>   0.5 25 26
table(probs, strata)
#>      strata
#> probs stratum_1 stratum_2 stratum_3 stratum_4 stratum_5
#>   0.1        15         0         0         0         0
#>   0.2         0        40         0         0         0
#>   0.3         0         0        65         0         0
#>   0.4         0         0         0        90         0
#>   0.5         0         0         0         0       141

```
