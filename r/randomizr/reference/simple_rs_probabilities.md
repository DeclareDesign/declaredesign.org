# Inclusion Probabilities: Simple Random Sampling

Inclusion Probabilities: Simple Random Sampling

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

  The number of units. N must be a positive integer. (required)

- prob:

  prob is the probability of being sampled must be a real number between
  0 and 1 inclusive, and must be of length 1. (optional)

- prob_unit:

  prob is the probability of being sampled must be a real number between
  0 and 1 inclusive, and must be of length N. (optional)

- check_inputs:

  logical. Defaults to TRUE.

- simple:

  logical. internal use only.

## Value

A vector length N indicating the probability of being sampled.

## Examples

``` r
probs <- simple_ra_probabilities(N = 100)
table(probs)
#> probs
#> 0.5 
#> 200 

probs <- simple_ra_probabilities(N = 100, prob = 0.3)
table(probs)
#> probs
#> 0.3 0.7 
#> 100 100 
```
