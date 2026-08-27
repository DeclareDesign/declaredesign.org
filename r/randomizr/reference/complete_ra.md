# Complete Random Assignment

`complete_ra` assigns exactly fixed numbers of units to each treatment
condition. In the canonical two-arm case, exactly `m` of `N` units are
assigned to treatment and `N-m` to control on every draw. This guarantee
that the counts are fixed is the defining feature of complete random
assignment, and it is what distinguishes it from simple random
assignment (where counts vary from draw to draw).

## Usage

``` r
complete_ra(
  N,
  m = NULL,
  m_unit = NULL,
  m_each = NULL,
  prob = NULL,
  prob_unit = NULL,
  prob_each = NULL,
  num_arms = NULL,
  conditions = NULL,
  check_inputs = TRUE
)
```

## Arguments

- N:

  The number of units. Must be a positive integer. (required)

- m:

  Use for a two-arm design: exactly `m` units are assigned to treatment
  and `N-m` to control. (optional)

- m_unit:

  Use for a two-arm design. `unique(m_unit)` units are assigned to
  treatment; must be the same for all units and of length N. (optional)

- m_each:

  Use for a multi-arm design. A numeric vector giving the exact number
  of units assigned to each condition; must sum to N. (optional)

- prob:

  Use for a two-arm design: either `floor(N*prob)` or `ceiling(N*prob)`
  units are assigned to treatment so that the marginal probability of
  assignment equals exactly `prob`. Must be between 0 and 1. One edge is
  deliberate: when `ceiling(N*prob) == N` (for instance
  `N = 3, prob = 0.9`), exactly `floor(N*prob)` units are treated, never
  all `N`, so the marginal probability is `floor(N*prob)/N`;
  [`complete_ra_probabilities()`](https://declaredesign.org/r/randomizr/reference/complete_ra_probabilities.md)
  reports the probability actually used. (optional)

- prob_unit:

  Use for a two-arm design. `unique(prob_unit)` will be passed to the
  `prob` argument; must be the same for all units. (optional)

- prob_each:

  Use for a multi-arm design. A numeric vector giving the probability of
  assignment to each condition; entries must be nonnegative and sum
  to 1. Due to integer rounding the exact count assigned to each
  condition may differ slightly from draw to draw, but the overall
  probability of assignment is exactly `prob_each`. (optional)

- num_arms:

  The number of treatment arms. If unspecified, determined from the
  other arguments. (optional)

- conditions:

  A character vector giving the names of the treatment groups. If
  unspecified, groups will be named 0 and 1 in a two-arm trial and T1,
  T2, T3, in a multi-arm trial. A two-group design in which `num_arms`
  is set to 2 will use condition names T1 and T2. (optional)

- check_inputs:

  Logical. Whether to verify before assigning that the arguments are
  internally consistent: that counts sum to N, that probabilities lie
  between 0 and 1 and sum to 1, that vectors are of length N, and so on.
  Defaults to `TRUE`. `FALSE` skips the checking only: `num_arms` and
  `conditions` are still derived from the other arguments, so the same
  call draws the same assignment either way. What goes is the
  verification, and an impossible design is then no longer refused.
  `block_m` larger than a block, for instance, quietly treats the whole
  block. Declaring the design once with
  [`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md)
  and drawing from it with
  [`conduct_ra()`](https://declaredesign.org/r/randomizr/reference/conduct_ra.md)
  is the usual way to avoid re-checking the same arguments in a
  simulation. (optional)

## Value

A vector of length N indicating the treatment condition of each unit.
Numeric in a two-arm trial; a factor (ordered by `conditions`) in a
multi-arm trial.

## Details

Researchers can specify counts directly (via `m` or `m_each`) or target
probabilities (via `prob` or `prob_each`). When probabilities are
specified and the implied counts are not integers, `complete_ra` uses
stochastic rounding to ensure that the overall probability of assignment
exactly equals the target. In a two-arm design, either `floor(N*prob)`
or `ceiling(N*prob)` units are assigned to treatment, with the draw
between these two values chosen so that `Pr(treatment)` equals exactly
`prob`. In a multi-arm design, the remaining units after floor
allocation are assigned using a single round of simple random assignment
calibrated to hit the exact target probabilities.

If only `N` is specified, a balanced two-arm trial (`prob = 0.5`) is
assumed. When `N` is odd, either `floor(N/2)` or `ceiling(N/2)` units
are assigned to treatment.

## See also

[`simple_ra()`](https://declaredesign.org/r/randomizr/reference/simple_ra.md),
[`block_ra()`](https://declaredesign.org/r/randomizr/reference/block_ra.md),
[`cluster_ra()`](https://declaredesign.org/r/randomizr/reference/cluster_ra.md),
[`complete_rs()`](https://declaredesign.org/r/randomizr/reference/complete_rs.md),
[`complete_ra_probabilities()`](https://declaredesign.org/r/randomizr/reference/complete_ra_probabilities.md)

## Examples

``` r
# Two-arm Designs
Z <- complete_ra(N = 100)
table(Z)
#> Z
#>  0  1 
#> 50 50 

Z <- complete_ra(N = 100, m = 50)
table(Z)
#> Z
#>  0  1 
#> 50 50 

Z <- complete_ra(N = 100, m_unit = rep(30, 100))
table(Z)
#> Z
#>  0  1 
#> 70 30 

Z <- complete_ra(N = 100, prob = 0.111)
table(Z)
#> Z
#>  0  1 
#> 89 11 

Z <- complete_ra(N = 100, prob_unit = rep(0.1, 100))
table(Z)
#> Z
#>  0  1 
#> 90 10 

Z <- complete_ra(N = 100, conditions = c("control", "treatment"))
table(Z)
#> Z
#>   control treatment 
#>        50        50 


# Multi-arm Designs
Z <- complete_ra(N = 100, num_arms = 3)
table(Z)
#> Z
#> T1 T2 T3 
#> 33 33 34 

Z <- complete_ra(N = 100, m_each = c(30, 30, 40))
table(Z)
#> Z
#> T1 T2 T3 
#> 30 30 40 

Z <- complete_ra(N = 100, prob_each = c(0.1, 0.2, 0.7))
table(Z)
#> Z
#> T1 T2 T3 
#> 10 20 70 

Z <- complete_ra(N = 100, conditions = c("control", "placebo", "treatment"))
table(Z)
#> Z
#>   control   placebo treatment 
#>        33        33        34 

# Special Cases
# Two-arm trial where the conditions are by default "T1" and "T2"
Z <- complete_ra(N = 100, num_arms = 2)
table(Z)
#> Z
#> T1 T2 
#> 50 50 

# If N = m, every unit is assigned to treatment with probability 1
complete_ra(N = 2, m = 2)
#> [1] 1 1

# The single-unit case works the same way: m = 1 out of N = 1 is treated
# with probability 1. Up through randomizr 0.12.0 this case was instead
# treated as a coin flip, so the unit was assigned to treatment only half of
# the time. The change is noted here because it silently alters the
# probabilities of assignment in code written against those versions.
complete_ra(N = 1, m = 1)
#> [1] 1
```
