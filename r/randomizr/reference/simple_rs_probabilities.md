# Inclusion probabilities: Simple Random Sampling

Returns each unit's probability of being sampled under simple random
sampling. Every unit is sampled independently, so the probabilities do
not depend on which other units were drawn.

## Usage

``` r
simple_rs_probabilities(
  N,
  prob = NULL,
  prob_unit = NULL,
  check_inputs = TRUE,
  simple = TRUE
)
```

## Arguments

- N:

  The number of units in the sampling frame. Must be a positive integer.
  (required)

- prob:

  The probability of being sampled; must be a real number between 0 and
  1 inclusive and of length 1. (optional)

- prob_unit:

  The probability of being sampled for each unit; must be a real number
  between 0 and 1 inclusive and of length N. Because units are drawn
  independently, this probability may differ from unit to unit.
  (optional)

- check_inputs:

  Logical. Whether to verify before sampling that the arguments are
  internally consistent: that probabilities lie between 0 and 1, that
  vectors are of length N, and that only one of `prob` and `prob_unit`
  is supplied. Defaults to `TRUE`. Set to `FALSE` to skip the checks
  when drawing many samples from arguments that have already been
  verified; declaring the design once with
  [`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md)
  and drawing from it with
  [`draw_rs()`](https://declaredesign.org/r/randomizr/reference/draw_rs.md)
  does this for you. (optional)

- simple:

  Logical. Internal use only; leave at its default. `simple_rs` always
  draws units independently, and this argument exists so that the
  argument checker knows as much. (optional)

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

[`simple_rs()`](https://declaredesign.org/r/randomizr/reference/simple_rs.md)

## Examples

``` r
probs <- simple_rs_probabilities(N = 100)
table(probs)
#> probs
#> 0.5 
#> 100 

probs <- simple_rs_probabilities(N = 100, prob = 0.3)
table(probs)
#> probs
#> 0.3 
#> 100 
```
