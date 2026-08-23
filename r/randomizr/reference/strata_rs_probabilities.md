# Inclusion Probabilities: Stratified Random Sampling

Inclusion Probabilities: Stratified Random Sampling

## Usage

``` r
strata_rs_probabilities(
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

A vector length N indicating the probability of being sampled.

## Examples

``` r

strata <- rep(c("A", "B","C"), times = c(50, 100, 200))
probs <- strata_rs_probabilities(strata = strata)
table(strata, probs)
#>       probs
#> strata 0.5
#>      A  50
#>      B 100
#>      C 200

probs <- strata_rs_probabilities(strata = strata, prob = .2)
table(strata, probs)
#>       probs
#> strata 0.2
#>      A  50
#>      B 100
#>      C 200

probs <- strata_rs_probabilities(strata = strata, strata_prob = c(.1, .2, .3))
table(strata, probs)
#>       probs
#> strata 0.1 0.2 0.3
#>      A  50   0   0
#>      B   0 100   0
#>      C   0   0 200

probs <- strata_rs_probabilities(strata = strata, strata_n = c(10, 40, 70))
table(strata, probs)
#>       probs
#> strata 0.2 0.35 0.4
#>      A  50    0   0
#>      B   0    0 100
#>      C   0  200   0
```
