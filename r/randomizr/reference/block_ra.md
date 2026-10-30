# Block Random Assignment

`block_ra` assigns units to treatment conditions within pre-defined
groups called blocks (or strata). Within each block, complete random
assignment determines which units are treated. Blocking typically
reduces the sampling variability of an experiment relative to simple or
complete random assignment: by guaranteeing that treated and control
units are drawn from every covariate-defined subgroup, it rules out the
unlucky assignments that would otherwise pull estimates far from the
true average treatment effect. The precision gain is largest when the
blocking variable is strongly correlated with potential outcomes; if the
blocking variable is uncorrelated with outcomes, blocking neither helps
nor hurts.

## Usage

``` r
block_ra(
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
  check_inputs = TRUE,
  .block_int = NULL,
  .N_per_block = NULL
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

- .block_int:

  Internal use only. Pre-computed integer encoding of `blocks`, passed
  by
  [`conduct_ra()`](https://declaredesign.org/r/randomizr/reference/conduct_ra.md)
  when a declaration was created with
  [`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md).
  Users should never set this argument. (optional)

- .N_per_block:

  Internal use only. Pre-computed block sizes corresponding to
  `.block_int`, passed by
  [`conduct_ra()`](https://declaredesign.org/r/randomizr/reference/conduct_ra.md).
  Users should never set this argument. (optional)

## Value

A vector of length N indicating the treatment condition of each unit.
Numeric in a two-arm trial; a factor (ordered by `conditions`) in a
multi-arm trial.

## Details

In the simplest two-arm case with no arguments beyond `blocks`, the
function assigns approximately half the units in each block to
treatment. Researchers can specify exact counts (via `block_m`) or
target probabilities that are held constant (via `prob`) or allowed to
vary (via `block_prob`) across blocks.

## See also

[`complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.md),
[`block_and_cluster_ra()`](https://declaredesign.org/r/randomizr/reference/block_and_cluster_ra.md),
[`strata_rs()`](https://declaredesign.org/r/randomizr/reference/strata_rs.md),
[`block_ra_probabilities()`](https://declaredesign.org/r/randomizr/reference/block_ra_probabilities.md)

## Examples

``` r

# Two-arm Designs

blocks <- rep(c("A", "B","C"), times = c(50, 100, 200))
Z <- block_ra(blocks = blocks)
table(blocks, Z)
#>       Z
#> blocks   0   1
#>      A  25  25
#>      B  50  50
#>      C 100 100

Z <- block_ra(blocks = blocks, prob = 0.3)
table(blocks, Z)
#>       Z
#> blocks   0   1
#>      A  35  15
#>      B  70  30
#>      C 140  60

Z <- block_ra(blocks = blocks, block_prob = c(0.1, 0.2, 0.3))
table(blocks, Z)
#>       Z
#> blocks   0   1
#>      A  45   5
#>      B  80  20
#>      C 140  60

Z <- block_ra(blocks = blocks, 
              prob_unit = rep(c(0.1, 0.2, 0.3), 
                              times = c(50, 100, 200)))
table(blocks, Z)
#>       Z
#> blocks   0   1
#>      A  45   5
#>      B  80  20
#>      C 140  60

Z <- block_ra(blocks = blocks, m = 20)
table(blocks, Z)
#>       Z
#> blocks   0   1
#>      A  30  20
#>      B  80  20
#>      C 180  20

Z <- block_ra(blocks = blocks, block_m = c(20, 30, 40))
table(blocks, Z)
#>       Z
#> blocks   0   1
#>      A  30  20
#>      B  70  30
#>      C 160  40

Z <- block_ra(blocks = blocks, 
              m_unit = rep(c(20, 30, 40),
                           times = c(50, 100, 200)))
table(blocks, Z)
#>       Z
#> blocks   0   1
#>      A  30  20
#>      B  70  30
#>      C 160  40

block_m_each <- rbind(c(25, 25),
                 c(50, 50),
                 c(100, 100))

Z <- block_ra(blocks = blocks, block_m_each = block_m_each)
table(blocks, Z)
#>       Z
#> blocks   0   1
#>      A  25  25
#>      B  50  50
#>      C 100 100

block_m_each <- rbind(c(10, 40),
                 c(30, 70),
                 c(50, 150))

Z <- block_ra(blocks = blocks, block_m_each = block_m_each,
              conditions = c("control", "treatment"))
table(blocks, Z)
#>       Z
#> blocks control treatment
#>      A      10        40
#>      B      30        70
#>      C      50       150

# Multi-arm Designs
Z <- block_ra(blocks = blocks, num_arms = 3)
table(blocks, Z)
#>       Z
#> blocks T1 T2 T3
#>      A 17 17 16
#>      B 33 34 33
#>      C 67 66 67

block_m_each <- rbind(c(10, 20, 20),
                 c(30, 50, 20),
                 c(50, 75, 75))
Z <- block_ra(blocks = blocks, block_m_each = block_m_each)
table(blocks, Z)
#>       Z
#> blocks T1 T2 T3
#>      A 10 20 20
#>      B 30 50 20
#>      C 50 75 75

Z <- block_ra(blocks = blocks, block_m_each = block_m_each,
              conditions = c("control", "placebo", "treatment"))
table(blocks, Z)
#>       Z
#> blocks control placebo treatment
#>      A      10      20        20
#>      B      30      50        20
#>      C      50      75        75

Z <- block_ra(blocks = blocks, prob_each = c(0.1, 0.1, 0.8))
table(blocks, Z)
#>       Z
#> blocks  T1  T2  T3
#>      A   5   5  40
#>      B  10  10  80
#>      C  20  20 160


```
