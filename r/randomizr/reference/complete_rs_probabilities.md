# Inclusion probabilities: Complete Random Sampling

Returns each unit's probability of being sampled under complete random
sampling, where the sample size is fixed on every draw.

## Usage

``` r
complete_rs_probabilities(
  N,
  n = NULL,
  n_unit = NULL,
  prob = NULL,
  prob_unit = NULL,
  check_inputs = TRUE
)
```

## Arguments

- N:

  The number of units in the sampling frame. Must be a positive integer.
  (required)

- n:

  Use for a design in which exactly `n` units are sampled. (optional)

- n_unit:

  `unique(n_unit)` will be passed to `n`; must be the same for all units
  and of length N. (optional)

- prob:

  Use for a design in which either `floor(N*prob)` or `ceiling(N*prob)`
  units are sampled, chosen so that each unit's probability of inclusion
  is exactly `prob`. Must be a real number between 0 and 1 inclusive.
  (optional)

- prob_unit:

  `unique(prob_unit)` will be passed to `prob`; must be the same for all
  units and of length N. Under complete random sampling the probability
  cannot vary by unit; use
  [`simple_rs()`](https://declaredesign.org/r/randomizr/reference/simple_rs.md)
  if it must. (optional)

- check_inputs:

  Logical. Whether to verify before sampling that the arguments are
  internally consistent: that `n` does not exceed N, that probabilities
  lie between 0 and 1, that vectors are of length N, and so on. Defaults
  to `TRUE`. Set to `FALSE` to skip the checks when drawing many samples
  from arguments that have already been verified; declaring the design
  once with
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

[`complete_rs()`](https://declaredesign.org/r/randomizr/reference/complete_rs.md)

## Examples

``` r
probs <- complete_rs_probabilities(N = 100)
table(probs)
#> probs
#> 0.5 
#> 100 

probs <- complete_rs_probabilities(N = 100, n = 50)
table(probs)
#> probs
#> 0.5 
#> 100 

probs <- complete_rs_probabilities(N = 100, prob = 0.3)
table(probs)
#> probs
#> 0.3 
#> 100 
```
