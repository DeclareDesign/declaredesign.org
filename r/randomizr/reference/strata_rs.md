# Stratified Random Sampling

`strata_rs` draws a sample separately within each of several groups
(strata) defined by covariates, using complete random sampling inside
every stratum. For example, 50 of 100 men and 75 of 200 women might be
sampled. Stratifying guarantees how much of the sample comes from each
group, which keeps small groups from being underrepresented by chance.

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

A numeric vector of length N indicating whether each unit is sampled (1)
or not (0).

## Details

The number sampled per stratum can be left to the function, set as a
common count or probability across strata (`n`, `prob`), or set stratum
by stratum (`strata_n`, `strata_prob`). When the probability varies
across strata the sample is not self-weighting, and
[`strata_rs_probabilities()`](https://declaredesign.org/r/randomizr/reference/strata_rs_probabilities.md)
gives the inclusion probabilities needed to weight it.

## See also

[`complete_rs()`](https://declaredesign.org/r/randomizr/reference/complete_rs.md),
[`strata_and_cluster_rs()`](https://declaredesign.org/r/randomizr/reference/strata_and_cluster_rs.md),
[`block_ra()`](https://declaredesign.org/r/randomizr/reference/block_ra.md),
[`strata_rs_probabilities()`](https://declaredesign.org/r/randomizr/reference/strata_rs_probabilities.md)

## Examples

``` r

strata <- rep(c("A", "B", "C"), times = c(50, 100, 200))

S <- strata_rs(strata = strata)
table(strata, S)
#>       S
#> strata   0   1
#>      A  25  25
#>      B  50  50
#>      C 100 100

# The same probability in every stratum
S <- strata_rs(strata = strata, prob = 0.3)
table(strata, S)
#>       S
#> strata   0   1
#>      A  35  15
#>      B  70  30
#>      C 140  60

# The same count in every stratum
S <- strata_rs(strata = strata, n = 20)
table(strata, S)
#>       S
#> strata   0   1
#>      A  30  20
#>      B  80  20
#>      C 180  20

# A different probability in each stratum, in the order of sort(unique(strata))
S <- strata_rs(strata = strata, strata_prob = c(0.1, 0.2, 0.3))
table(strata, S)
#>       S
#> strata   0   1
#>      A  45   5
#>      B  80  20
#>      C 140  60

# The same, specified unit by unit
S <- strata_rs(strata = strata,
               prob_unit = rep(c(0.1, 0.2, 0.3), times = c(50, 100, 200)))
table(strata, S)
#>       S
#> strata   0   1
#>      A  45   5
#>      B  80  20
#>      C 140  60

# A different count in each stratum
S <- strata_rs(strata = strata, strata_n = c(20, 30, 40))
table(strata, S)
#>       S
#> strata   0   1
#>      A  30  20
#>      B  70  30
#>      C 160  40

S <- strata_rs(strata = strata,
               n_unit = rep(c(20, 30, 40), times = c(50, 100, 200)))
table(strata, S)
#>       S
#> strata   0   1
#>      A  30  20
#>      B  70  30
#>      C 160  40

```
