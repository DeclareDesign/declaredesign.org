# Complete Random Sampling

`complete_rs` draws a sample of a fixed size: exactly `n` of `N` units
are sampled on every draw. Fixing the sample size is what distinguishes
it from simple random sampling, where the realized size varies.

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

A numeric vector of length N indicating whether each unit is sampled (1)
or not (0).

## Details

Set the number of units to sample directly with `n`, or give a target
probability with `prob` and let `complete_rs` work out the number. When
`N*prob` is not a whole number, either `floor(N*prob)` or
`ceiling(N*prob)` units are sampled: the ceiling is drawn with
probability equal to the fractional part of `N*prob` and the floor
otherwise, which makes each unit's probability of inclusion exactly
`prob`. Specify `N` and not more than one of `n` or `prob`.

If only `N` is specified, half the units are sampled. When `N` is odd,
either `floor(N/2)` or `ceiling(N/2)` units are sampled.

## See also

[`simple_rs()`](https://declaredesign.org/r/randomizr/reference/simple_rs.md),
[`strata_rs()`](https://declaredesign.org/r/randomizr/reference/strata_rs.md),
[`cluster_rs()`](https://declaredesign.org/r/randomizr/reference/cluster_rs.md),
[`complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.md),
[`complete_rs_probabilities()`](https://declaredesign.org/r/randomizr/reference/complete_rs_probabilities.md)

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

S <- complete_rs(N = 100, n_unit = rep(30, 100))
table(S)
#> S
#>  0  1 
#> 70 30 

S <- complete_rs(N = 100, prob = 0.111)
table(S)
#> S
#>  0  1 
#> 89 11 

S <- complete_rs(N = 100, prob_unit = rep(0.1, 100))
table(S)
#> S
#>  0  1 
#> 90 10 

# If N = n, every unit is sampled with probability 1
complete_rs(N = 2, n = 2)
#> [1] 1 1

# The single-unit case works the same way: n = 1 out of N = 1 is sampled
# with probability 1. Up through randomizr 0.12.0 this case was instead
# treated as a coin flip, so the unit was sampled only half of the time.
# The change is noted here because it silently alters the inclusion
# probabilities in code written against those versions.
complete_rs(N = 1, n = 1)
#> [1] 1

```
