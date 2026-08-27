# Probabilities of assignment: Simple Random Assignment

Returns the probability that each unit is assigned to each condition
under simple random assignment. Every unit is assigned independently, so
the probabilities do not depend on how the other units came out.

## Usage

``` r
simple_ra_probabilities(
  N,
  prob = NULL,
  prob_unit = NULL,
  prob_each = NULL,
  num_arms = NULL,
  conditions = NULL,
  check_inputs = TRUE,
  simple = TRUE
)
```

## Arguments

- N:

  The number of units. Must be a positive integer. (required)

- prob:

  Use for a two-arm design. The probability of assignment to treatment;
  must be a real number between 0 and 1 and of length 1. (optional)

- prob_unit:

  Use for a two-arm design. The probability of assignment to treatment
  for each unit; must be a real number between 0 and 1 and of length N.
  (optional)

- prob_each:

  Use for a multi-arm design. A numeric vector or N-by-conditions matrix
  giving the probability of assignment to each condition; entries must
  be nonnegative and sum to 1. (optional)

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
  internally consistent: that probabilities lie between 0 and 1 and sum
  to 1, that vectors are of length N, that only one of `prob`,
  `prob_unit`, and `prob_each` is supplied, and so on. Defaults to
  `TRUE`. `FALSE` skips the checking only: `num_arms` and `conditions`
  are still derived from the other arguments, so the same call draws the
  same assignment either way. What goes is the verification, and an
  impossible design is then no longer refused. `block_m` larger than a
  block, for instance, quietly treats the whole block. Declaring the
  design once with
  [`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md)
  and drawing from it with
  [`conduct_ra()`](https://declaredesign.org/r/randomizr/reference/conduct_ra.md)
  is the usual way to avoid re-checking the same arguments in a
  simulation. (optional)

- simple:

  Logical. Internal use only; leave at its default. `simple_ra` always
  assigns units independently, and this argument exists so that the
  argument checker knows as much. Setting it to `FALSE` does not change
  how units are assigned, but it will cause a `prob_unit` that varies
  across units to be rejected. (optional)

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

[`simple_ra()`](https://declaredesign.org/r/randomizr/reference/simple_ra.md)

## Examples

``` r
# Two Group Designs
prob_mat <- simple_ra_probabilities(N = 100)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.5    0.5
#> [2,]    0.5    0.5
#> [3,]    0.5    0.5
#> [4,]    0.5    0.5
#> [5,]    0.5    0.5
#> [6,]    0.5    0.5

prob_mat <- simple_ra_probabilities(N = 100, prob = 0.5)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.5    0.5
#> [2,]    0.5    0.5
#> [3,]    0.5    0.5
#> [4,]    0.5    0.5
#> [5,]    0.5    0.5
#> [6,]    0.5    0.5

prob_mat <- simple_ra_probabilities(N = 100, prob_each = c(0.3, 0.7),
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
prob_mat <- simple_ra_probabilities(N = 100, num_arms = 3)
head(prob_mat)
#>        prob_T1   prob_T2   prob_T3
#> [1,] 0.3333333 0.3333333 0.3333333
#> [2,] 0.3333333 0.3333333 0.3333333
#> [3,] 0.3333333 0.3333333 0.3333333
#> [4,] 0.3333333 0.3333333 0.3333333
#> [5,] 0.3333333 0.3333333 0.3333333
#> [6,] 0.3333333 0.3333333 0.3333333

prob_mat <- simple_ra_probabilities(N = 100, prob_each = c(0.3, 0.3, 0.4))
head(prob_mat)
#>      prob_T1 prob_T2 prob_T3
#> [1,]     0.3     0.3     0.4
#> [2,]     0.3     0.3     0.4
#> [3,]     0.3     0.3     0.4
#> [4,]     0.3     0.3     0.4
#> [5,]     0.3     0.3     0.4
#> [6,]     0.3     0.3     0.4

prob_mat <- simple_ra_probabilities(N = 100, prob_each = c(0.3, 0.3, 0.4),
                        conditions = c("control", "placebo", "treatment"))
head(prob_mat)
#>      prob_control prob_placebo prob_treatment
#> [1,]          0.3          0.3            0.4
#> [2,]          0.3          0.3            0.4
#> [3,]          0.3          0.3            0.4
#> [4,]          0.3          0.3            0.4
#> [5,]          0.3          0.3            0.4
#> [6,]          0.3          0.3            0.4

prob_mat <- simple_ra_probabilities(N = 100, conditions = c("control", "placebo", "treatment"))
head(prob_mat)
#>      prob_control prob_placebo prob_treatment
#> [1,]    0.3333333    0.3333333      0.3333333
#> [2,]    0.3333333    0.3333333      0.3333333
#> [3,]    0.3333333    0.3333333      0.3333333
#> [4,]    0.3333333    0.3333333      0.3333333
#> [5,]    0.3333333    0.3333333      0.3333333
#> [6,]    0.3333333    0.3333333      0.3333333
```
