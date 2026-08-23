# Block Random Assignment

block_ra implements a random assignment procedure in which units that
are grouped into blocks defined by pre-treatment covariates are assigned
using complete random assignment within block. For example, imagine that
50 of 100 men are assigned to treatment and 75 of 200 women are assigned
to treatment.

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
  check_inputs = TRUE
)
```

## Arguments

- blocks:

  A vector of length N that indicates which block each unit belongs to.
  Can be a character, factor, or numeric vector. (required)

- prob:

  Use for a two-arm design in which either floor(N_block\*prob) or
  ceiling(N_block\*prob) units are assigned to treatment within each
  block. The probability of assignment to treatment is exactly prob
  because with probability 1-prob, floor(N_block\*prob) units will be
  assigned to treatment and with probability prob,
  ceiling(N_block\*prob) units will be assigned to treatment. prob must
  be a real number between 0 and 1 inclusive. (optional)

- prob_unit:

  Use for a two arm design. Must of be of length N. tapply(prob_unit,
  blocks, unique) will be passed to `block_prob`.

- prob_each:

  Use for a multi-arm design in which the values of prob_each determine
  the probabilities of assignment to each treatment condition. prob_each
  must be a numeric vector giving the probability of assignment to each
  condition. All entries must be nonnegative real numbers between 0 and
  1 inclusive and the total must sum to 1. Because of integer issues,
  the exact number of units assigned to each condition may differ
  (slightly) from assignment to assignment, but the overall probability
  of assignment is exactly prob_each. (optional)

- m:

  Use for a two-arm design in which the scalar m describes the fixed
  number of units to assign in each block. This number does not vary
  across blocks.

- m_unit:

  Use for a two-arm design. Must be of length N. tapply(m_unit, blocks,
  unique) will be passed to `block_m`.

- block_m:

  Use for a two-arm design in which the vector block_m describes the
  number of units to assign to treatment within each block. block_m must
  be a numeric vector that is as long as the number of blocks and is in
  the same order as sort(unique(blocks)).

- block_m_each:

  Use for a multi-arm design in which the values of block_m_each
  determine the number of units assigned to each condition. block_m_each
  must be a matrix with the same number of rows as blocks and the same
  number of columns as treatment arms. Cell entries are the number of
  units to be assigned to each treatment arm within each block. The rows
  should respect the ordering of the blocks as determined by
  sort(unique(blocks)). The columns should be in the order of
  conditions, if specified.

- block_prob:

  Use for a two-arm design in which block_prob describes the probability
  of assignment to treatment within each block. Must be in the same
  order as sort(unique(blocks)). Differs from prob in that the
  probability of assignment can vary across blocks.

- block_prob_each:

  Use for a multi-arm design in which the values of block_prob_each
  determine the probabilities of assignment to each treatment condition.
  block_prob_each must be a matrix with the same number of rows as
  blocks and the same number of columns as treatment arms. Cell entries
  are the probabilities of assignment to treatment within each block.
  The rows should respect the ordering of the blocks as determined by
  sort(unique(blocks)). Use only if the probabilities of assignment
  should vary by block, otherwise use prob_each. Each row of
  block_prob_each must sum to 1.

- num_arms:

  The number of treatment arms. If unspecified, num_arms will be
  determined from the other arguments. (optional)

- conditions:

  A character vector giving the names of the treatment groups. If
  unspecified, the treatment groups will be named 0 (for control) and 1
  (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm
  trial. An exception is a two-group design in which num_arms is set to
  2, in which case the condition names are T1 and T2, as in a multi-arm
  trial with two arms. (optional)

- check_inputs:

  logical. Defaults to TRUE.

## Value

A vector of length N that indicates the treatment condition of each
unit. Is numeric in a two-arm trial and a factor variable (ordered by
conditions) in a multi-arm trial.

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

Z <- block_ra(blocks = blocks, prob = .3)
table(blocks, Z)
#>       Z
#> blocks   0   1
#>      A  35  15
#>      B  70  30
#>      C 140  60

Z <- block_ra(blocks = blocks, block_prob = c(.1, .2, .3))
table(blocks, Z)
#>       Z
#> blocks   0   1
#>      A  45   5
#>      B  80  20
#>      C 140  60

Z <- block_ra(blocks = blocks, 
              prob_unit = rep(c(.1, .2, .3), 
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
#>      A 17 16 17
#>      B 34 33 33
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

Z <- block_ra(blocks = blocks, prob_each = c(.1, .1, .8))
table(blocks, Z)
#>       Z
#> blocks  T1  T2  T3
#>      A   5   5  40
#>      B  10  10  80
#>      C  20  20 160


```
