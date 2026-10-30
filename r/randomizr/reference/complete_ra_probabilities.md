# Probabilities of assignment: Complete Random Assignment

Returns the probability that each unit is assigned to each condition
under complete random assignment. When the implied counts are not
integers the probabilities account for the stochastic rounding
`complete_ra` uses, so they equal the target exactly rather than
approximately.

## Usage

``` r
complete_ra_probabilities(
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
  `complete_ra_probabilities()` reports the probability actually used.
  (optional)

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

A matrix with N rows and one column per treatment condition, with
columns named `prob_<condition>`. Entry (i, j) is the probability that
unit i is assigned to condition j, and every row sums to 1.

## Details

These are the quantities inverse-probability weights are built from:
weight each unit by the reciprocal of the probability of the condition
it landed in, which
[`obtain_condition_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_condition_probabilities.md)
extracts for you.

## See also

[`complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.md)

## Examples

``` r
# 2-arm designs
prob_mat <- complete_ra_probabilities(N = 100)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.5    0.5
#> [2,]    0.5    0.5
#> [3,]    0.5    0.5
#> [4,]    0.5    0.5
#> [5,]    0.5    0.5
#> [6,]    0.5    0.5

prob_mat <- complete_ra_probabilities(N = 100, m = 50)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.5    0.5
#> [2,]    0.5    0.5
#> [3,]    0.5    0.5
#> [4,]    0.5    0.5
#> [5,]    0.5    0.5
#> [6,]    0.5    0.5

prob_mat <- complete_ra_probabilities(N = 100, prob = 0.3)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.7    0.3
#> [2,]    0.7    0.3
#> [3,]    0.7    0.3
#> [4,]    0.7    0.3
#> [5,]    0.7    0.3
#> [6,]    0.7    0.3

prob_mat <- complete_ra_probabilities(N = 100, m_each = c(30, 70),
                          conditions = c("control", "treatment"))
head(prob_mat)
#>      prob_control prob_treatment
#> [1,]          0.3            0.7
#> [2,]          0.3            0.7
#> [3,]          0.3            0.7
#> [4,]          0.3            0.7
#> [5,]          0.3            0.7
#> [6,]          0.3            0.7

# Multi-arm Designs
prob_mat <- complete_ra_probabilities(N = 100, num_arms = 3)
head(prob_mat)
#>        prob_T1   prob_T2   prob_T3
#> [1,] 0.3333333 0.3333333 0.3333333
#> [2,] 0.3333333 0.3333333 0.3333333
#> [3,] 0.3333333 0.3333333 0.3333333
#> [4,] 0.3333333 0.3333333 0.3333333
#> [5,] 0.3333333 0.3333333 0.3333333
#> [6,] 0.3333333 0.3333333 0.3333333

prob_mat <- complete_ra_probabilities(N = 100, m_each = c(30, 30, 40))
head(prob_mat)
#>      prob_T1 prob_T2 prob_T3
#> [1,]     0.3     0.3     0.4
#> [2,]     0.3     0.3     0.4
#> [3,]     0.3     0.3     0.4
#> [4,]     0.3     0.3     0.4
#> [5,]     0.3     0.3     0.4
#> [6,]     0.3     0.3     0.4

prob_mat <- complete_ra_probabilities(N = 100, m_each = c(30, 30, 40),
                          conditions = c("control", "placebo", "treatment"))
head(prob_mat)
#>      prob_control prob_placebo prob_treatment
#> [1,]          0.3          0.3            0.4
#> [2,]          0.3          0.3            0.4
#> [3,]          0.3          0.3            0.4
#> [4,]          0.3          0.3            0.4
#> [5,]          0.3          0.3            0.4
#> [6,]          0.3          0.3            0.4

prob_mat <- complete_ra_probabilities(N = 100, conditions = c("control", "placebo", "treatment"))
head(prob_mat)
#>      prob_control prob_placebo prob_treatment
#> [1,]    0.3333333    0.3333333      0.3333333
#> [2,]    0.3333333    0.3333333      0.3333333
#> [3,]    0.3333333    0.3333333      0.3333333
#> [4,]    0.3333333    0.3333333      0.3333333
#> [5,]    0.3333333    0.3333333      0.3333333
#> [6,]    0.3333333    0.3333333      0.3333333

prob_mat <- complete_ra_probabilities(N = 100, prob_each = c(0.2, 0.7, 0.1))
head(prob_mat)
#>      prob_T1 prob_T2 prob_T3
#> [1,]     0.2     0.7     0.1
#> [2,]     0.2     0.7     0.1
#> [3,]     0.2     0.7     0.1
#> [4,]     0.2     0.7     0.1
#> [5,]     0.2     0.7     0.1
#> [6,]     0.2     0.7     0.1
```
