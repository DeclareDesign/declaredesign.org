# Probabilities of assignment: Block Random Assignment

Returns the probability that each unit is assigned to each condition
under block random assignment. Units in different blocks routinely have
different probabilities, which is exactly when these numbers are needed.

## Usage

``` r
block_ra_probabilities(
  blocks = NULL,
  prob = NULL,
  prob_unit = NULL,
  prob_each = NULL,
  m = NULL,
  m_unit = NULL,
  block_m = NULL,
  block_m_each = NULL,
  block_prob = NULL,
  block_prob_each = NULL,
  num_arms = NULL,
  conditions = NULL,
  check_inputs = TRUE
)
```

## Arguments

- blocks:

  A vector of length N indicating which block each unit belongs to. Can
  be character, factor, or numeric. (required)

- prob:

  Use for a two-arm design in which either `floor(N_block*prob)` or
  `ceiling(N_block*prob)` units are assigned to treatment within each
  block. Which of the two is used is itself random: the ceiling is drawn
  with probability equal to the fractional part of `N_block*prob` and
  the floor otherwise, which makes each unit's probability of assignment
  exactly `prob`. When `N_block*prob` is a whole number the count is
  fixed. Must be a real number between 0 and 1. (optional)

- prob_unit:

  Use for a two-arm design. Must be of length N.
  `tapply(prob_unit, blocks, unique)` will be passed to `block_prob`.
  (optional)

- prob_each:

  Use for a multi-arm design in which the values of `prob_each`
  determine the probabilities of assignment to each treatment condition.
  Must be a numeric vector giving the probability of assignment to each
  condition. All entries must be nonnegative real numbers between 0 and
  1 and the total must sum to 1. Because of integer rounding, the exact
  number of units assigned to each condition may differ slightly from
  assignment to assignment, but the overall probability of assignment is
  exactly `prob_each`. (optional)

- m:

  Use for a two-arm design in which the scalar `m` gives the fixed
  number of units to assign to treatment within every block. This count
  does not vary across blocks. (optional)

- m_unit:

  Use for a two-arm design. Must be of length N.
  `tapply(m_unit, blocks, unique)` will be passed to `block_m`.
  (optional)

- block_m:

  Use for a two-arm design in which `block_m` gives the number of units
  to assign to treatment within each block. Must be a numeric vector as
  long as the number of blocks, in the same order as
  `sort(unique(blocks))`. (optional)

- block_m_each:

  Use for a multi-arm design in which `block_m_each` gives the number of
  units assigned to each condition within each block. Must be a matrix
  with one row per block and one column per treatment arm. Rows should
  respect the ordering of blocks by `sort(unique(blocks))`; columns
  should be in the order of `conditions`, if specified. (optional)

- block_prob:

  Use for a two-arm design in which the probability of assignment to
  treatment varies across blocks. Must be in the same order as
  `sort(unique(blocks))`. (optional)

- block_prob_each:

  Use for a multi-arm design in which assignment probabilities vary
  across blocks. Must be a matrix with one row per block and one column
  per treatment arm. Each row must sum to 1. Rows respect the ordering
  of `sort(unique(blocks))`. (optional)

- num_arms:

  The number of treatment arms. If unspecified, determined from the
  other arguments. (optional)

- conditions:

  A character vector giving the names of the treatment groups. If
  unspecified, the treatment groups will be named 0 (for control) and 1
  (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm
  trial. A two-group design in which `num_arms` is set to 2 will use
  condition names T1 and T2. (optional)

- check_inputs:

  Logical. Whether to verify before assigning that the arguments are
  internally consistent: that counts sum to the block sizes, that
  probabilities lie between 0 and 1 and sum to 1, that matrices have one
  row per block, and so on. Defaults to `TRUE`. `FALSE` skips the
  checking only: `num_arms` and `conditions` are still derived from the
  other arguments, so the same call draws the same assignment either
  way. What goes is the verification, and an impossible design is then
  no longer refused. `block_m` larger than a block, for instance,
  quietly treats the whole block. Declaring the design once with
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

[`block_ra()`](https://declaredesign.org/r/randomizr/reference/block_ra.md)

## Examples

``` r

blocks <- rep(c("A", "B","C"), times = c(50, 100, 200))
prob_mat <- block_ra_probabilities(blocks = blocks)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.5    0.5
#> [2,]    0.5    0.5
#> [3,]    0.5    0.5
#> [4,]    0.5    0.5
#> [5,]    0.5    0.5
#> [6,]    0.5    0.5

prob_mat <- block_ra_probabilities(blocks = blocks, m = 20)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.6    0.4
#> [2,]    0.6    0.4
#> [3,]    0.6    0.4
#> [4,]    0.6    0.4
#> [5,]    0.6    0.4
#> [6,]    0.6    0.4

block_m_each <- rbind(c(25, 25),
                 c(50, 50),
                 c(100, 100))

prob_mat <- block_ra_probabilities(blocks = blocks, block_m_each = block_m_each)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.5    0.5
#> [2,]    0.5    0.5
#> [3,]    0.5    0.5
#> [4,]    0.5    0.5
#> [5,]    0.5    0.5
#> [6,]    0.5    0.5

block_m_each <- rbind(c(10, 40),
                 c(30, 70),
                 c(50, 150))

prob_mat <- block_ra_probabilities(blocks = blocks,
                                   block_m_each = block_m_each,
                                   conditions = c("control", "treatment"))
head(prob_mat)
#>      prob_control prob_treatment
#> [1,]          0.2            0.8
#> [2,]          0.2            0.8
#> [3,]          0.2            0.8
#> [4,]          0.2            0.8
#> [5,]          0.2            0.8
#> [6,]          0.2            0.8

prob_mat <- block_ra_probabilities(blocks = blocks, num_arms = 3)
head(prob_mat)
#>        prob_T1   prob_T2   prob_T3
#> [1,] 0.3333333 0.3333333 0.3333333
#> [2,] 0.3333333 0.3333333 0.3333333
#> [3,] 0.3333333 0.3333333 0.3333333
#> [4,] 0.3333333 0.3333333 0.3333333
#> [5,] 0.3333333 0.3333333 0.3333333
#> [6,] 0.3333333 0.3333333 0.3333333

block_m_each <- rbind(c(10, 20, 20),
                 c(30, 50, 20),
                 c(50, 75, 75))
prob_mat <- block_ra_probabilities(blocks = blocks, block_m_each = block_m_each)
head(prob_mat)
#>      prob_T1 prob_T2 prob_T3
#> [1,]     0.2     0.4     0.4
#> [2,]     0.2     0.4     0.4
#> [3,]     0.2     0.4     0.4
#> [4,]     0.2     0.4     0.4
#> [5,]     0.2     0.4     0.4
#> [6,]     0.2     0.4     0.4

prob_mat <- block_ra_probabilities(blocks = blocks, block_m_each = block_m_each,
                       conditions = c("control", "placebo", "treatment"))
head(prob_mat)
#>      prob_control prob_placebo prob_treatment
#> [1,]          0.2          0.4            0.4
#> [2,]          0.2          0.4            0.4
#> [3,]          0.2          0.4            0.4
#> [4,]          0.2          0.4            0.4
#> [5,]          0.2          0.4            0.4
#> [6,]          0.2          0.4            0.4

prob_mat <- block_ra_probabilities(blocks = blocks, prob_each = c(0.1, 0.1, 0.8))
head(prob_mat)
#>      prob_T1 prob_T2 prob_T3
#> [1,]     0.1     0.1     0.8
#> [2,]     0.1     0.1     0.8
#> [3,]     0.1     0.1     0.8
#> [4,]     0.1     0.1     0.8
#> [5,]     0.1     0.1     0.8
#> [6,]     0.1     0.1     0.8
```
