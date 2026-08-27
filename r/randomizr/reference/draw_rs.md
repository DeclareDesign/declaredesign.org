# Draw a Random Sample

`draw_rs` draws one random sample from a design. Give it a declaration
made by
[`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md),
or describe the design inline with the same arguments
[`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md)
takes. Declaring first pays off when the same design is drawn
repeatedly, or when the inclusion probabilities are needed later by
[`obtain_inclusion_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_inclusion_probabilities.md).

## Usage

``` r
draw_rs(
  declaration = NULL,
  N = NULL,
  strata = NULL,
  clusters = NULL,
  n = NULL,
  n_unit = NULL,
  prob = NULL,
  prob_unit = NULL,
  strata_n = NULL,
  strata_prob = NULL,
  simple = FALSE,
  check_inputs = TRUE
)
```

## Arguments

- declaration:

  A random sampling declaration, created by
  [`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md).
  Supply either a declaration or the design arguments listed below,
  which are the ones
  [`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md)
  takes: given those, `draw_rs` builds a declaration internally and
  draws one sample from it. (optional)

- N:

  The number of units in the sampling frame. Must be a positive integer.
  (required)

- strata:

  A vector of length N indicating which stratum each unit belongs to.
  Supply to use stratified random sampling. (optional)

- clusters:

  A vector of length N indicating which cluster each unit belongs to.
  Supply to sample whole clusters. (optional)

- n:

  Use for a design in which exactly `n` units (or clusters) are sampled.
  In a stratified design, exactly `n` units in each stratum are sampled.
  (optional)

- n_unit:

  Of length N. Under complete random sampling, must be constant across
  units. Under stratified random sampling, must be constant within
  strata. (optional)

- prob:

  Use for a design in which either `floor(N*prob)` or `ceiling(N*prob)`
  units (or clusters) are sampled. Which of the two is used is itself
  random: the ceiling is drawn with probability equal to the fractional
  part of `N*prob` and the floor otherwise, which makes each unit's
  probability of inclusion exactly `prob`. Must be a real number between
  0 and 1 inclusive. (optional)

- prob_unit:

  Of length N. Under simple random sampling, may differ for each unit or
  cluster. Under complete random sampling, must be constant across
  units. Under stratified random sampling, must be constant within
  strata. (optional)

- strata_n:

  Use for a design in which `strata_n` gives the number of units to
  sample within each stratum, in the order of `sort(unique(strata))`.
  (optional)

- strata_prob:

  Use for a design in which `strata_prob` gives the probability of being
  sampled within each stratum, in the order of `sort(unique(strata))`.
  Differs from `prob` in that the probability of being sampled can vary
  across strata. (optional)

- simple:

  Logical, defaults to `FALSE`. If `TRUE`, simple random sampling is
  used, so the size of the realized sample varies from draw to draw. Do
  not specify `n` or `strata_n` when `simple = TRUE`; `prob` may then
  vary by unit. (optional)

- check_inputs:

  Logical. Whether to verify before declaring that the arguments are
  internally consistent: that counts do not exceed the frame, that
  probabilities lie between 0 and 1, that stratum-level arguments have
  one entry per stratum, and so on. Defaults to `TRUE`. Set to `FALSE`
  to skip the checks when declaring many designs from arguments that
  have already been verified. (optional)

## Value

A numeric vector of length N indicating whether each unit is sampled (1)
or not (0).

## See also

[`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md),
[`obtain_inclusion_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_inclusion_probabilities.md)

## Examples

``` r
# Declare the design once, then draw from it
declaration <- declare_rs(N = 100, n = 30)

S <- draw_rs(declaration = declaration)
table(S)
#> S
#>  0  1 
#> 70 30 

# Equivalent, and convenient for a one-off sample: describe the design
# inline and skip the declaration
S <- draw_rs(N = 100, n = 30)
table(S)
#> S
#>  0  1 
#> 70 30 
```
