# Obtain the Probability of the Condition Each Unit Is In

A declaration holds the probability of every condition for every unit.
`obtain_condition_probabilities` picks out, for each unit, the one
probability that corresponds to the condition it was actually assigned
to. Give it a declaration made by
[`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md),
or describe the design inline with the same arguments
[`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md)
takes.  
  
This function is especially useful when units have different
probabilities of assignment and the analyst plans to use
inverse-probability weights: the weights are the reciprocals of what it
returns.

## Usage

``` r
obtain_condition_probabilities(
  declaration = NULL,
  assignment,
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

- declaration:

  A random assignment declaration, created by
  [`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md).
  Supply either a declaration or the design arguments that
  [`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md)
  takes. (optional)

- assignment:

  A vector of random assignments, often created by
  [`conduct_ra()`](https://declaredesign.org/r/randomizr/reference/conduct_ra.md).
  (required)

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

A vector of length N giving, for each unit, the probability that it was
assigned to the condition it is actually in. These are the quantities
inverse-probability weights are built from: weight each unit by the
reciprocal of its value here.

## See also

[`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md),
[`conduct_ra()`](https://declaredesign.org/r/randomizr/reference/conduct_ra.md)

## Examples

``` r

# Conduct a block random assignment in which the blocks have different
# probabilities of assignment to treatment
blocks <- rep(c("A", "B", "C"), times = c(50, 100, 200))

block_m_each <- rbind(c(10, 40),
                      c(30, 70),
                      c(50, 150))

declaration <- declare_ra(blocks = blocks, block_m_each = block_m_each)

Z <- conduct_ra(declaration = declaration)
table(Z, blocks)
#>    blocks
#> Z     A   B   C
#>   0  10  30  50
#>   1  40  70 150

observed_probabilities <-
   obtain_condition_probabilities(declaration = declaration, assignment = Z)

# Probabilities in the control group:
table(observed_probabilities[Z == 0], blocks[Z == 0])
#>       
#>         A  B  C
#>   0.2  10  0  0
#>   0.25  0  0 50
#>   0.3   0 30  0

# Probabilities in the treatment group:
table(observed_probabilities[Z == 1], blocks[Z == 1])
#>       
#>          A   B   C
#>   0.7    0  70   0
#>   0.75   0   0 150
#>   0.8   40   0   0

# The weights for an inverse-probability-weighted regression
ipw <- 1 / observed_probabilities


# Sometimes it is convenient to skip the declaration step
Z <- conduct_ra(blocks = blocks, block_m_each = block_m_each)

observed_probabilities <-
   obtain_condition_probabilities(assignment = Z,
                                  blocks = blocks,
                                  block_m_each = block_m_each)

table(observed_probabilities[Z == 0], blocks[Z == 0])
#>       
#>         A  B  C
#>   0.2  10  0  0
#>   0.25  0  0 50
#>   0.3   0 30  0
table(observed_probabilities[Z == 1], blocks[Z == 1])
#>       
#>          A   B   C
#>   0.7    0  70   0
#>   0.75   0   0 150
#>   0.8   40   0   0
```
