# Probabilities of assignment: Balanced Random Assignment

**Experimental.** Returns the probability that each unit is assigned to
each condition under
[`balanced_ra()`](https://declaredesign.org/r/randomizr/reference/balanced_ra.md).
Because those probabilities are supplied by the caller rather than
derived from a design, this function mainly validates and normalizes
them into the matrix form the other `_probabilities` functions return.

## Usage

``` r
balanced_ra_probabilities(
  N = NULL,
  prob = NULL,
  prob_unit = NULL,
  prob_unit_each = NULL,
  blocks = NULL,
  clusters = NULL,
  num_arms = NULL,
  conditions = NULL,
  formula = NULL,
  check_inputs = TRUE
)
```

## Arguments

- N:

  The number of units. Optional when `formula` or the length of
  `prob_unit` (or `blocks` or `clusters`) identifies N. A single
  positive integer. If supplied it must match. (optional)

- prob:

  A single number between 0 and 1: the probability of assignment to
  treatment, shared by every unit, for a two-arm design. Defaults to 0.5
  when no probability argument is supplied, so `balanced_ra(4)` is
  complete assignment of four units. Supply exactly one of `prob`,
  `prob_unit` and `prob_unit_each`. (optional)

- prob_unit:

  A numeric vector of length N giving each unit's probability of
  assignment to treatment, for a two-arm design. Unlike elsewhere in
  randomizr these need not be equal across units. A single number is
  refused, since that is what `prob` is for. Supply exactly one of
  `prob`, `prob_unit` and `prob_unit_each`. (optional)

- prob_unit_each:

  A numeric matrix with one row per unit and one column per condition,
  giving each unit's probability of assignment to each condition, for a
  multi-arm design. Rows must sum to 1. Supply exactly one of `prob`,
  `prob_unit` and `prob_unit_each`. (optional)

- blocks:

  A vector of length N indicating which block each unit belongs to. When
  supplied, two-arm counts are held tight within each block and overall;
  with three or more arms the tight counts are the within-block ones.
  (optional)

- clusters:

  A vector of length N indicating which cluster each unit belongs to.
  Whole clusters are assigned together, so the probabilities must be the
  same for every unit in a cluster, and the tight counts become counts
  of clusters rather than of units. May be combined with `blocks`, in
  which case every cluster must sit entirely inside one block. May also
  be combined with `formula`, in which case each cluster's covariates
  are the averages of its units' covariates, so that a cluster counts
  once however many units it holds and the treated count that is held
  tight remains a count of clusters. (optional)

- num_arms:

  The number of treatment arms. Inferred when omitted. Supplied without
  any probability argument, `num_arms` (or `conditions`) of three or
  more expands to equal-probability assignment, as in
  [`complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.md).
  (optional)

- conditions:

  A vector giving the names of the conditions. (optional)

- formula:

  A model formula whose model matrix is the balancing matrix \\X\\ in
  the cube method, e.g. `~ x + B`. The intercept column is the count
  constraint; `~ 0 + x` drops it and the treated count may wander. Names
  are looked up where the formula was written, then in the calling
  frame, so the usual `dat |> mutate(Z = balanced_ra(formula = ~ x))`
  finds the column `x`. Two-arm only. May be combined with `clusters`;
  cannot be combined with `blocks` or `prob_unit_each`. (optional)

- check_inputs:

  Logical. Whether to verify before assigning that the arguments are
  internally consistent: that probabilities lie between 0 and 1, that
  rows of a probability matrix sum to 1, that probabilities are constant
  within a cluster, and that clusters nest within blocks. Defaults to
  `TRUE`. Set to `FALSE` to skip the checks when drawing many
  assignments from probabilities that have already been verified.
  (optional)

## Value

A matrix of probabilities of assignment, one row per unit and one column
per condition, with columns named `prob_<condition>`.

## Details

These are the quantities inverse-probability weights are built from:
weight each unit by the reciprocal of the probability of the condition
it landed in.

## See also

[`balanced_ra()`](https://declaredesign.org/r/randomizr/reference/balanced_ra.md)

## Examples

``` r
balanced_ra_probabilities(prob_unit = c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5))
#>      prob_0 prob_1
#> [1,]    0.8    0.2
#> [2,]    0.6    0.4
#> [3,]    0.4    0.6
#> [4,]    0.2    0.8
#> [5,]    0.5    0.5
#> [6,]    0.5    0.5
```
