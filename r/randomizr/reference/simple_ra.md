# Simple Random Assignment

`simple_ra` assigns units to treatment conditions independently, with
each unit's assignment drawn as a separate Bernoulli trial. Because
units are assigned independently, the number of units assigned to each
condition varies from draw to draw. For most experimental applications
in which the number of units is known in advance,
[`complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.md)
is preferable because it fixes the counts in each condition and thereby
reduces sampling variability.

## Usage

``` r
simple_ra(
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

A vector of length N indicating the treatment condition of each unit.
Numeric in a two-arm trial; a factor (ordered by `conditions`) in a
multi-arm trial.

## Details

Simple random assignment is appropriate when units arrive sequentially
and the total sample size is not known in advance, or when the
assignment must proceed without coordinating across units. If only `N`
is specified, a two-arm trial with `prob = 0.5` is assumed.

## See also

[`complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.md),
[`block_ra()`](https://declaredesign.org/r/randomizr/reference/block_ra.md),
[`simple_rs()`](https://declaredesign.org/r/randomizr/reference/simple_rs.md),
[`simple_ra_probabilities()`](https://declaredesign.org/r/randomizr/reference/simple_ra_probabilities.md)

## Examples

``` r
# Two Group Designs

Z <- simple_ra(N = 100)
table(Z)
#> Z
#>  0  1 
#> 52 48 

Z <- simple_ra(N = 100, prob = 0.5)
table(Z)
#> Z
#>  0  1 
#> 56 44 

Z <- simple_ra(N = 100, prob_each = c(0.3, 0.7),
               conditions = c("control", "treatment"))
table(Z)
#> Z
#>   control treatment 
#>        26        74 

# A probability of assignment that varies unit by unit
Z <- simple_ra(N = 100, prob_unit = seq(0.1, 0.9, length.out = 100))
table(Z)
#> Z
#>  0  1 
#> 52 48 

# Skipping the input checks. The checks are also what fill in defaults, so
# conditions has to be given explicitly once they are skipped. In a
# simulation, declare_ra() and conduct_ra() are the tidier way to check the
# arguments once and then draw many assignments from them.
Z <- simple_ra(N = 100, prob = 0.3, conditions = c(0, 1), check_inputs = FALSE)
table(Z)
#> Z
#>  0  1 
#> 72 28 

# Multi-arm Designs
Z <- simple_ra(N = 100, num_arms = 3)
table(Z)
#> Z
#> T1 T2 T3 
#> 27 32 41 

Z <- simple_ra(N = 100, prob_each = c(0.3, 0.3, 0.4))
table(Z)
#> Z
#> T1 T2 T3 
#> 32 30 38 

Z <- simple_ra(N = 100, prob_each = c(0.3, 0.3, 0.4),
               conditions = c("control", "placebo", "treatment"))
table(Z)
#> Z
#>   control   placebo treatment 
#>        33        36        31 

Z <- simple_ra(N = 100, conditions = c("control", "placebo", "treatment"))
table(Z)
#> Z
#>   control   placebo treatment 
#>        30        41        29 
```
