# Conduct a Random Assignment

`conduct_ra` draws one random assignment from a design. Give it a
declaration made by
[`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md),
or describe the design inline with the same arguments
[`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md)
takes. Declaring first pays off when the same design is drawn
repeatedly, or when the assignment probabilities are needed later by
[`obtain_condition_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_condition_probabilities.md).

## Usage

``` r
conduct_ra(
  declaration = NULL,
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
  Supply either a declaration or the design arguments listed below,
  which are the ones
  [`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md)
  takes: given those, `conduct_ra` builds a declaration internally and
  draws one assignment from it. (optional)

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
  `conduct_ra()` reuses the matrix built then, so a later change to
  those variables does not change the declared design. (optional)

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

A vector of length N giving the treatment condition of each unit,
numeric in a two-arm design and a factor (ordered by `conditions`) in a
multi-arm design.

## See also

[`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md),
[`obtain_condition_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_condition_probabilities.md)

## Examples

``` r
# Declare the design once, then draw from it
declaration <- declare_ra(N = 100, m_each = c(30, 30, 40))

Z <- conduct_ra(declaration = declaration)
table(Z)
#> Z
#> T1 T2 T3 
#> 30 30 40 

# Equivalent, and convenient for a one-off assignment: describe the design
# inline and skip the declaration
Z <- conduct_ra(N = 100, m_each = c(30, 30, 40))
table(Z)
#> Z
#> T1 T2 T3 
#> 30 30 40 
```
