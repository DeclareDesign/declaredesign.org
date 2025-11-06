# Stratified Random Sampling

strata_rs implements a random sampling procedure in which units that are
grouped into strata defined by covariates are sample using complete
random sampling within stratum For example, imagine that 50 of 100 men
are sampled and 75 of 200 women are sampled.

## Usage

``` r
strata_rs(
  strata = NULL,
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
  to. Can be a character, factor, or numeric vector. (required)

- prob:

  Use for a design in which either floor(N_stratum\*prob) or
  ceiling(N_stratum\*prob) units are sampled within each stratum. The
  probability of being sampled is exactly prob because with probability
  1-prob, floor(N_stratum\*prob) units will be sampled and with
  probability prob, ceiling(N_stratum\*prob) units will be sampled. prob
  must be a real number between 0 and 1 inclusive. (optional)

- prob_unit:

  Must of be of length N. tapply(prob_unit, strata, unique) will be
  passed to `strata_prob`.

- n:

  Use for a design in which the scalar n describes the fixed number of
  units to sample in each stratum. This number does not vary across
  strata.

- n_unit:

  Must be of length N. tapply(m_unit, strata, unique) will be passed to
  `strata_n`.

- strata_n:

  Use for a design in which the numeric vector strata_n describes the
  number of units to sample within each stratum.

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
strata <- rep(c("A", "B","C"), times = c(50, 100, 200))
Z <- strata_rs(strata = strata)
table(strata, Z)
#>       Z
#> strata   0   1
#>      A  25  25
#>      B  50  50
#>      C 100 100

Z <- strata_rs(strata = strata, prob = .3)
table(strata, Z)
#>       Z
#> strata   0   1
#>      A  35  15
#>      B  70  30
#>      C 140  60

Z <- strata_rs(strata = strata, n = 20)
table(strata, Z)
#>       Z
#> strata   0   1
#>      A  30  20
#>      B  80  20
#>      C 180  20

Z <- strata_rs(strata = strata, strata_prob = c(.1, .2, .3))
table(strata, Z)
#>       Z
#> strata   0   1
#>      A  45   5
#>      B  80  20
#>      C 140  60

Z <- strata_rs(strata = strata, 
               prob_unit = rep(c(.1, .2, .3), times = c(50, 100, 200)))
table(strata, Z)
#>       Z
#> strata   0   1
#>      A  45   5
#>      B  80  20
#>      C 140  60

Z <- strata_rs(strata = strata, strata_n = c(20, 30, 40))
table(strata, Z)
#>       Z
#> strata   0   1
#>      A  30  20
#>      B  70  30
#>      C 160  40

Z <- strata_rs(strata = strata, 
               n_unit = rep(c(20, 30, 40), times = c(50, 100, 200)))
table(strata, Z)
#>       Z
#> strata   0   1
#>      A  30  20
#>      B  70  30
#>      C 160  40

```
