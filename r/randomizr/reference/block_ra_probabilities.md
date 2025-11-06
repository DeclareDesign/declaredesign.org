# probabilities of assignment: Block Random Assignment

probabilities of assignment: Block Random Assignment

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

A matrix of probabilities of assignment

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

prob_mat <- block_ra_probabilities(blocks=blocks, block_m_each=block_m_each,
                       conditions=c("control", "placebo", "treatment"))
head(prob_mat)
#>      prob_control prob_placebo prob_treatment
#> [1,]          0.2          0.4            0.4
#> [2,]          0.2          0.4            0.4
#> [3,]          0.2          0.4            0.4
#> [4,]          0.2          0.4            0.4
#> [5,]          0.2          0.4            0.4
#> [6,]          0.2          0.4            0.4

prob_mat <- block_ra_probabilities(blocks=blocks, prob_each=c(.1, .1, .8))
head(prob_mat)
#>      prob_T1 prob_T2 prob_T3
#> [1,]     0.1     0.1     0.8
#> [2,]     0.1     0.1     0.8
#> [3,]     0.1     0.1     0.8
#> [4,]     0.1     0.1     0.8
#> [5,]     0.1     0.1     0.8
#> [6,]     0.1     0.1     0.8
```
