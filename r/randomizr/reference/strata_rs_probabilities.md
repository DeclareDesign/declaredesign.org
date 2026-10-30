# Inclusion probabilities: Stratified Random Sampling

Returns each unit's probability of being sampled under stratified random
sampling. Units in different strata routinely have different
probabilities, and a sample drawn that way is not self-weighting.

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

  A vector of length N indicating which stratum each unit belongs to.
  Can be a character, factor, or numeric vector. (required)

- prob:

  Use for a design in which either `floor(N_stratum*prob)` or
  `ceiling(N_stratum*prob)` units are sampled within each stratum. Which
  of the two is used is itself random: the ceiling is drawn with
  probability equal to the fractional part of `N_stratum*prob` and the
  floor otherwise, which makes each unit's probability of inclusion
  exactly `prob`. Must be a real number between 0 and 1 inclusive.
  (optional)

- prob_unit:

  Must be of length N. `tapply(prob_unit, strata, unique)` will be
  passed to `strata_prob`, so it must be constant within each stratum.
  (optional)

- n:

  Use for a design in which the scalar `n` gives the fixed number of
  units to sample in every stratum. This count does not vary across
  strata. (optional)

- n_unit:

  Must be of length N. `tapply(n_unit, strata, unique)` will be passed
  to `strata_n`, so it must be constant within each stratum. (optional)

- strata_n:

  Use for a design in which the numeric vector `strata_n` gives the
  number of units to sample within each stratum. Must be as long as the
  number of strata, in the same order as `sort(unique(strata))`.
  (optional)

- strata_prob:

  Use for a design in which `strata_prob` gives the probability of being
  sampled within each stratum. Must be in the same order as
  `sort(unique(strata))`. Differs from `prob` in that the probability of
  being sampled can vary across strata. (optional)

- check_inputs:

  Logical. Whether to verify before sampling that the arguments are
  internally consistent: that counts do not exceed the stratum sizes,
  that probabilities lie between 0 and 1, that stratum-level arguments
  have one entry per stratum, and so on. Defaults to `TRUE`. Set to
  `FALSE` to skip the checks when drawing many samples from arguments
  that have already been verified; declaring the design once with
  [`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md)
  and drawing from it with
  [`draw_rs()`](https://declaredesign.org/r/randomizr/reference/draw_rs.md)
  does this for you. (optional)

## Value

A numeric vector of length N giving each unit's probability of being
included in the sample.

## Details

These are the quantities inverse-probability weights are built from:
weight each sampled unit by the reciprocal of its inclusion probability,
which
[`obtain_inclusion_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_inclusion_probabilities.md)
extracts for you.

## See also

[`strata_rs()`](https://declaredesign.org/r/randomizr/reference/strata_rs.md)

## Examples

``` r

strata <- rep(c("A", "B", "C"), times = c(50, 100, 200))

probs <- strata_rs_probabilities(strata = strata)
table(strata, probs)
#>       probs
#> strata 0.5
#>      A  50
#>      B 100
#>      C 200

probs <- strata_rs_probabilities(strata = strata, prob = 0.2)
table(strata, probs)
#>       probs
#> strata 0.2
#>      A  50
#>      B 100
#>      C 200

probs <- strata_rs_probabilities(strata = strata, strata_prob = c(0.1, 0.2, 0.3))
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
