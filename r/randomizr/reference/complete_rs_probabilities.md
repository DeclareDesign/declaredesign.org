# Inclusion Probabilities: Complete Random Sampling

Inclusion Probabilities: Complete Random Sampling

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

  The number of units. N must be a positive integer. (required)

- n:

  Use for a design in which exactly n units are sampled. (optional)

- n_unit:

  unique(n_unit) will be passed to `n`. Must be the same for all units
  (optional)

- prob:

  Use for a design in which either floor(N\*prob) or ceiling(N\*prob)
  units are sampled. The probability of being sampled is exactly prob
  because with probability 1-prob, floor(N\*prob) units will be sampled
  and with probability prob, ceiling(N\*prob) units will be sampled.
  prob must be a real number between 0 and 1 inclusive. (optional)

- prob_unit:

  unique(prob_unit) will be passed to the prob argument and must be the
  same for all units.

- check_inputs:

  logical. Defaults to TRUE.

## Value

A vector length N indicating the probability of being sampled.

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

probs <- complete_rs_probabilities(N=100, prob = .3)
table(probs)
#> probs
#> 0.3 
#> 100 
```
