# Declare a Random Assignment Procedure

`declare_ra` creates a reusable declaration object that captures all the
parameters of a random assignment procedure. The declaration separates
the specification of the design from the act of conducting it: call
`declare_ra` once to fix the design, then call
[`conduct_ra()`](https://declaredesign.org/r/randomizr/reference/conduct_ra.md)
repeatedly (for example, across simulation iterations) to draw
assignments from the declared procedure. The declaration also
precomputes and caches the probability of assignment for each unit,
which
[`obtain_condition_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_condition_probabilities.md)
returns for use in inverse-probability-weighted estimators.

## Usage

``` r
declare_ra(
  N = NULL,
  blocks = NULL,
  clusters = NULL,
  m = NULL,
  m_unit = NULL,
  m_each = NULL,
  prob = NULL,
  prob_unit = NULL,
  prob_each = NULL,
  prob_unit_each = NULL,
  block_m = NULL,
  block_m_each = NULL,
  block_prob = NULL,
  block_prob_each = NULL,
  num_arms = NULL,
  conditions = NULL,
  simple = FALSE,
  ra_type = NULL,
  formula = NULL,
  permutation_matrix = NULL,
  check_inputs = TRUE,
  data = NULL
)
```

## Arguments

- N:

  The number of units. A positive integer. Optional when `data`,
  `formula`, or the length of `prob_unit` (or `blocks`, or `clusters`)
  identifies N.

- blocks:

  A vector of length N indicating which block each unit belongs to, or,
  when `data` is supplied, the name of the column holding it. Supply to
  use blocked random assignment. (optional)

- clusters:

  A vector of length N indicating which cluster each unit belongs to,
  or, when `data` is supplied, the name of the column holding it. Supply
  to use cluster random assignment. (optional)

- m:

  Use for a two-arm design: exactly `m` units (or clusters) are assigned
  to treatment. In a blocked design, exactly `m` units in each block are
  treated. (optional)

- m_unit:

  Use for a two-arm trial. A vector of length N; a single number is
  refused, since that is what `m` is for. Under complete random
  assignment, must be constant across units. Under blocked random
  assignment, must be constant within blocks. When `data` is supplied,
  names a column of it. (optional)

- m_each:

  Use for a multi-arm design. A numeric vector giving the number of
  units (or clusters) assigned to each condition; must sum to N.
  (optional)

- prob:

  Use for a two-arm design: either `floor(N*prob)` or `ceiling(N*prob)`
  units (or clusters) are assigned to treatment so that the marginal
  probability of assignment equals exactly `prob`. A single number
  between 0 and 1; use `prob_unit` to let it vary across units.
  (optional)

- prob_unit:

  Use for a two-arm design. Of length N. Under simple random assignment,
  may differ by unit or cluster. Under complete random assignment, must
  be constant across units. Under blocked random assignment, must be
  constant within blocks. Under balanced assignment
  (`ra_type = "balanced"`), may differ by unit. A single number is
  refused on every path, including the balanced one: use `prob`. When
  `data` is supplied, names a column of it. (optional)

- prob_each:

  Use for a multi-arm design. A numeric vector giving the probability of
  assignment to each condition; entries must be nonnegative and sum
  to 1. Due to integer rounding the exact count in each condition may
  differ slightly from draw to draw, but the overall probability is
  exactly `prob_each`. Under balanced assignment the same vector is
  expanded to one row per unit. (optional)

- prob_unit_each:

  Use for balanced assignment with two or more arms. A numeric matrix
  with one row per unit and one column per condition, giving each unit's
  probability of assignment to each condition. Rows must sum to 1.
  Supplying this argument selects
  [`balanced_ra()`](https://declaredesign.org/r/randomizr/reference/balanced_ra.md).
  When `data` is supplied, build it from columns, as in
  `cbind(p_a, p_b)`. (optional)

- block_m:

  Use for a two-arm blocked design: a vector giving the number of units
  to assign to treatment within each block, in the order of
  `sort(unique(blocks))`. (optional)

- block_m_each:

  Use for a multi-arm blocked design. A matrix with one row per block
  and one column per treatment arm giving the number of units assigned
  to each condition within each block. Rows respect the ordering of
  `sort(unique(blocks))`. (optional)

- block_prob:

  Use for a two-arm blocked design in which the treatment probability
  varies across blocks. In the order of `sort(unique(blocks))`.
  (optional)

- block_prob_each:

  Use for a multi-arm blocked design in which treatment probabilities
  vary across blocks. A matrix with one row per block and one column per
  arm; each row must sum to 1. (optional)

- num_arms:

  The number of treatment arms. If unspecified, determined from the
  other arguments. (optional)

- conditions:

  A character vector giving the names of the treatment groups. If
  unspecified, groups will be named 0 and 1 in a two-arm trial and T1,
  T2, T3, in a multi-arm trial. A two-group design in which `num_arms`
  is set to 2 will use condition names T1 and T2. (optional)

- simple:

  Logical, defaults to `FALSE`. If `TRUE`, simple random assignment is
  used. Do not specify `m`, `m_each`, `block_m`, or `block_m_each` when
  `simple = TRUE`. (optional)

- ra_type:

  Optional override. The only accepted value is `"balanced"`, which
  selects
  [`balanced_ra()`](https://declaredesign.org/r/randomizr/reference/balanced_ra.md)
  and allows `prob_unit` to vary across units. Other designs are
  inferred from the arguments supplied; they cannot be forced with this
  argument. (optional)

- formula:

  For balanced assignment. A model formula whose model matrix is the
  balancing matrix \\X\\ in the cube method, e.g. `~ x + B`. The
  intercept is the count constraint. Do not also pass `blocks`.
  Supplying `formula` selects
  [`balanced_ra()`](https://declaredesign.org/r/randomizr/reference/balanced_ra.md).
  Two-arm only. The formula's variables are taken from `data` when it is
  supplied. They are looked up once, when the design is declared;
  [`conduct_ra()`](https://declaredesign.org/r/randomizr/reference/conduct_ra.md)
  reuses the matrix built then, so a later change to those variables
  does not change the declared design. (optional)

- permutation_matrix:

  For random assignment procedures that none of the other arguments can
  describe. A matrix with one row per unit and one column per assignment
  the procedure can produce, whose entries are condition names.
  Supplying it declares a design that draws one of those columns at
  random with equal probability, and the probabilities of assignment are
  read off the matrix by counting how often each unit appears in each
  condition. Build the matrix by calling your own assignment function
  many times and binding the results, or with
  [`obtain_permutation_matrix()`](https://declaredesign.org/r/randomizr/reference/obtain_permutation_matrix.md)
  for a design randomizr already knows. Ignored if `NULL`. (optional)

- check_inputs:

  Logical. Whether to verify before declaring that the arguments are
  internally consistent: that counts sum to N, that probabilities lie
  between 0 and 1 and sum to 1, that block-level arguments have one
  entry per block, and so on. Defaults to `TRUE`. `FALSE` skips the
  checking only: `num_arms` and `conditions` are still derived from the
  other arguments. It is skipped entirely when `permutation_matrix` is
  supplied. (optional)

- data:

  A data frame holding the design's variables. When supplied, every
  argument that carries one value per unit names columns of it and is
  looked up there and nowhere else: `blocks`, `clusters`, `m_unit`,
  `prob_unit`, `prob_unit_each`, and the variables in `formula`.
  Anything they name that is not a column is an error rather than a
  fall-through to the calling environment. A bare column name is the
  ordinary case; any expression works so long as every variable in it is
  a column, so `blocks = interaction(region, year)` and
  `prob_unit_each = cbind(p_a, p_b)` are fine and `blocks = df$bl` is
  not, because it names `df`. A string naming a column is also accepted.
  `N` defaults to `nrow(data)`. A declaration outlives the frame it was
  written in, so this is how to make it say exactly which variables it
  is built from. `permutation_matrix` is not resolved this way: it has
  one row per unit but enumerates assignments rather than describing
  units. When `data` is omitted, everything resolves in the calling
  environment as before. `data` itself is not stored in the declaration;
  the variables it supplies are. (optional)

## Value

An object of class `"ra_declaration"` (an environment, addressable like
a list) with entries:

- `ra_function`:

  A function that draws a random assignment from the declared procedure.

- `ra_type`:

  A string indicating the type of random assignment used.

- `probabilities_matrix`:

  A matrix with N rows and `num_arms` columns giving each unit's
  probability of assignment to each condition.

- `blocks`:

  The blocking variable, if supplied.

- `clusters`:

  The clustering variable, if supplied.

## Details

`declare_ra` supports simple, complete, blocked, clustered,
blocked-and-clustered, and balanced designs. It dispatches to the
appropriate low-level function
([`simple_ra()`](https://declaredesign.org/r/randomizr/reference/simple_ra.md),
[`complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.md),
[`block_ra()`](https://declaredesign.org/r/randomizr/reference/block_ra.md),
[`cluster_ra()`](https://declaredesign.org/r/randomizr/reference/cluster_ra.md),
[`block_and_cluster_ra()`](https://declaredesign.org/r/randomizr/reference/block_and_cluster_ra.md),
or
[`balanced_ra()`](https://declaredesign.org/r/randomizr/reference/balanced_ra.md))
based on which arguments are supplied. Balanced assignment is opt-in:
`declare_ra(N, prob = 0.5)` remains complete assignment. Use
`ra_type = "balanced"` or supply `prob_unit_each` or `formula`.

## See also

[`conduct_ra()`](https://declaredesign.org/r/randomizr/reference/conduct_ra.md),
[`obtain_condition_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_condition_probabilities.md),
[`balanced_ra()`](https://declaredesign.org/r/randomizr/reference/balanced_ra.md),
[`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md)

## Examples

``` r
# A declaration is used in three ways.

# 1. To obtain some basic facts about a randomization:

declaration <- declare_ra(N = 100, m_each = c(30, 30, 40))
declaration
#> Random assignment procedure: Complete random assignment 
#> Number of units: 100 
#> Number of treatment arms: 3 
#> The possible treatment categories are T1 and T2 and T3.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are constant across units: 
#> prob_T1 prob_T2 prob_T3 
#>     0.3     0.3     0.4 

# 2. To conduct a random assignment:

Z <- conduct_ra(declaration)
table(Z)
#> Z
#> T1 T2 T3 
#> 30 30 40 

# 3. To obtain the probability that each unit is in the condition it is in:

probs <- obtain_condition_probabilities(declaration, Z)
table(probs, Z)
#>      Z
#> probs T1 T2 T3
#>   0.3 30 30  0
#>   0.4  0  0 40


# Simple Random Assignment Declarations

declare_ra(N = 100, simple = TRUE)
#> Random assignment procedure: Simple random assignment 
#> Number of units: 100 
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is 1.26765060022823e+30.  
#> The probabilities of assignment are constant across units: 
#> prob_0 prob_1 
#>    0.5    0.5 

declare_ra(N = 100, prob = 0.4, simple = TRUE)
#> Random assignment procedure: Simple random assignment 
#> Number of units: 100 
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is 1.26765060022823e+30.  
#> The probabilities of assignment are constant across units: 
#> prob_0 prob_1 
#>    0.6    0.4 

declare_ra(N = 100, prob_each = c(0.3, 0.3, 0.4),
           conditions = c("control", "placebo", "treatment"), simple = TRUE)
#> Random assignment procedure: Simple random assignment 
#> Number of units: 100 
#> Number of treatment arms: 3 
#> The possible treatment categories are control and placebo and treatment.
#> The number of possible random assignments is 5.15377520732011e+47.  
#> The probabilities of assignment are constant across units: 
#>   prob_control   prob_placebo prob_treatment 
#>            0.3            0.3            0.4 


# Complete Random Assignment Declarations

declare_ra(N = 100)
#> Random assignment procedure: Complete random assignment 
#> Number of units: 100 
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are constant across units: 
#> prob_0 prob_1 
#>    0.5    0.5 

declare_ra(N = 100, m_each = c(30, 70),
           conditions = c("control", "treatment"))
#> Random assignment procedure: Complete random assignment 
#> Number of units: 100 
#> Number of treatment arms: 2 
#> The possible treatment categories are control and treatment.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are constant across units: 
#>   prob_control prob_treatment 
#>            0.3            0.7 

declare_ra(N = 100, m_each = c(30, 30, 40))
#> Random assignment procedure: Complete random assignment 
#> Number of units: 100 
#> Number of treatment arms: 3 
#> The possible treatment categories are T1 and T2 and T3.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are constant across units: 
#> prob_T1 prob_T2 prob_T3 
#>     0.3     0.3     0.4 


# Block Random Assignment Declarations

blocks <- rep(c("A", "B", "C"), times = c(50, 100, 200))
declare_ra(blocks = blocks)
#> Random assignment procedure: Block random assignment 
#> Number of units: 350 
#> Number of blocks: 3
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are constant across units: 
#> prob_0 prob_1 
#>    0.5    0.5 

# One row per block, one column per arm
block_m_each <- rbind(c(10, 40),
                      c(30, 70),
                      c(50, 150))

declare_ra(blocks = blocks, block_m_each = block_m_each)
#> Random assignment procedure: Block random assignment 
#> Number of units: 350 
#> Number of blocks: 3
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are NOT constant across units. Your analysis strategy must account for differential probabilities of assignment, typically by employing inverse probability weights.


# Cluster Random Assignment Declarations

clusters <- rep(letters[1:10], times = 1:10)

declare_ra(clusters = clusters)
#> Random assignment procedure: Cluster random assignment 
#> Number of units: 55 
#> Number of clusters: 10
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is 252.  
#> The probabilities of assignment are constant across units: 
#> prob_0 prob_1 
#>    0.5    0.5 

declare_ra(clusters = clusters, m_each = c(3, 3, 4))
#> Random assignment procedure: Cluster random assignment 
#> Number of units: 55 
#> Number of clusters: 10
#> Number of treatment arms: 3 
#> The possible treatment categories are T1 and T2 and T3.
#> The number of possible random assignments is 4200.  
#> The probabilities of assignment are constant across units: 
#> prob_T1 prob_T2 prob_T3 
#>     0.3     0.3     0.4 


# Blocked and Clustered Random Assignment Declarations

clusters <- rep(letters[1:12], times = 1:12)

blocks <- rep(NA, length(clusters))
blocks[clusters %in% letters[1:3]] <- "block_1"
blocks[clusters %in% letters[4:6]] <- "block_2"
blocks[clusters %in% letters[7:9]] <- "block_3"
blocks[clusters %in% letters[10:12]] <- "block_4"

table(blocks, clusters)
#>          clusters
#> blocks     a  b  c  d  e  f  g  h  i  j  k  l
#>   block_1  1  2  3  0  0  0  0  0  0  0  0  0
#>   block_2  0  0  0  4  5  6  0  0  0  0  0  0
#>   block_3  0  0  0  0  0  0  7  8  9  0  0  0
#>   block_4  0  0  0  0  0  0  0  0  0 10 11 12

declare_ra(clusters = clusters, blocks = blocks)
#> Random assignment procedure: Blocked and clustered random assignment 
#> Number of units: 78 
#> Number of blocks: 4
#> Number of clusters: 12
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is 1296.  
#> The probabilities of assignment are constant across units: 
#> prob_0 prob_1 
#>    0.5    0.5 

declare_ra(clusters = clusters, blocks = blocks, prob_each = c(0.2, 0.5, 0.3))
#> Random assignment procedure: Blocked and clustered random assignment 
#> Number of units: 78 
#> Number of blocks: 4
#> Number of clusters: 12
#> Number of treatment arms: 3 
#> The possible treatment categories are T1 and T2 and T3.
#> The number of possible random assignments is 923521.  
#> The probabilities of assignment are constant across units: 
#> prob_T1 prob_T2 prob_T3 
#>     0.2     0.5     0.3 


# Balanced assignment (tight counts; probabilities may vary).
# Opt-in: without ra_type or prob_unit_each this remains complete assignment.

p <- c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)
declare_ra(prob_unit = p, ra_type = "balanced")
#> Random assignment procedure: Balanced random assignment 
#> Number of units: 6 
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are NOT constant across units. Your analysis strategy must account for differential probabilities of assignment, typically by employing inverse probability weights.

P <- cbind(c(0.15, 0.47), c(0.65, 0.48), c(0.20, 0.05))
declare_ra(prob_unit_each = P)
#> Random assignment procedure: Balanced random assignment 
#> Number of units: 2 
#> Number of treatment arms: 2 
#> The possible treatment categories are T1 and T2.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are NOT constant across units. Your analysis strategy must account for differential probabilities of assignment, typically by employing inverse probability weights.

x <- c(0, 1, 5, 6, 8, 9)
declare_ra(formula = ~ x)
#> Random assignment procedure: Balanced random assignment 
#> Number of units: 6 
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are constant across units: 
#> prob_0 prob_1 
#>    0.5    0.5 

# Name the table the design is built from, and blocks, clusters and the
# formula's variables are its columns rather than whatever the calling
# environment happens to hold.
dat <- data.frame(bl = rep(c("a", "b"), each = 3), x = c(0, 1, 5, 6, 8, 9),
                  p = c(0.2, 0.4, 0.5, 0.5, 0.6, 0.8))
declare_ra(blocks = bl, data = dat)
#> Random assignment procedure: Block random assignment 
#> Number of units: 6 
#> Number of blocks: 2
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is 36.  
#> The probabilities of assignment are constant across units: 
#> prob_0 prob_1 
#>    0.5    0.5 
declare_ra(formula = ~ x, data = dat)
#> Random assignment procedure: Balanced random assignment 
#> Number of units: 6 
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are constant across units: 
#> prob_0 prob_1 
#>    0.5    0.5 
declare_ra(prob_unit = p, ra_type = "balanced", data = dat)
#> Random assignment procedure: Balanced random assignment 
#> Number of units: 6 
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are NOT constant across units. Your analysis strategy must account for differential probabilities of assignment, typically by employing inverse probability weights.
```
