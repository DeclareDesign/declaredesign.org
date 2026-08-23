# Simple Random Sampling

simple_rs implements a random sampling procedure in which units are
independently sampled. Because units are sampled independently, the
number of units that are sampled can vary from sample to sample. For
most applications in which the number of units in the sampling frame is
known in advance,
[`complete_rs`](https://declaredesign.org/r/randomizr/reference/complete_rs.md)
is better because the number of units sampled is fixed across sampled.  
  

## Usage

``` r
simple_rs(N, prob = NULL, prob_unit = NULL, check_inputs = TRUE, simple = TRUE)
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

A numeric vector of length N that indicates if a unit is sampled (1) or
not (0).

## Examples

``` r

S <- simple_rs(N = 100)
table(S)
#> S
#>  0  1 
#> 47 53 

S <- simple_rs(N = 100, prob = 0.3)
table(S)
#> S
#>  0  1 
#> 69 31 
```
