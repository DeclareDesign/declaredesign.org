# Complete Random Sampling

complete_rs implements a random sampling procedure in which fixed
numbers of units are sampled. The canonical example of complete random
sampling is a procedure in which exactly n of N units are sampled.  
  
Users can set the exact number of units to sample with n. Alternatively,
users can specify the probability of being sampled with prob and
complete_rs will infer the correct number of units to sample.
complete_rs will either sample floor(N\*prob) or ceiling(N\*prob) units,
choosing between these two values to ensure that the overall probability
of being sampled is exactly prob. Users should specify N and not more
than one of n or prob.  
  
If only N is specified, N/2 units will be sampled. If N is odd, either
floor(N/2) units or ceiling(N/2) units will be sampled.

## Usage

``` r
complete_rs(
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

A numeric vector of length N that indicates if a unit is sampled (1) or
not (0).

## Examples

``` r
S <- complete_rs(N = 100)
table(S)
#> S
#>  0  1 
#> 50 50 

S <- complete_rs(N = 100, n = 50)
table(S)
#> S
#>  0  1 
#> 50 50 

S <- complete_rs(N = 100, n_unit = rep(50, 100))
table(S)
#> S
#>  0  1 
#> 50 50 

S <- complete_rs(N = 100, prob = .111)
table(S)
#> S
#>  0  1 
#> 89 11 

S <- complete_rs(N = 100, prob_unit = rep(.1, 100))
table(S)
#> S
#>  0  1 
#> 90 10 

# If N = n, sample with 100% probability...
complete_rs(N=2, n=2)
#> [1] 1 1

# Up through randomizr 0.12.0, 
# This behavior has been deprecated
complete_rs(N=1, n=1) # sampled with 50% probability
#> [1] 1

```
