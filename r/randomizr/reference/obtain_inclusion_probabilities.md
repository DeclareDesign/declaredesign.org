# Obtain Inclusion Probabilities

Returns each unit's probability of being included in the sample under a
declared design. Give `obtain_inclusion_probabilities()` a declaration
made by
[`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md),
or describe the design inline with the same arguments
[`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md)
takes.  
  
This function is especially useful when units have different inclusion
probabilities and the analyst plans to use inverse-probability weights:
the weights are the reciprocals of what it returns.

## Usage

``` r
obtain_inclusion_probabilities(
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
  Supply either a declaration or the design arguments that
  [`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md)
  takes. (optional)

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

A numeric vector of length N giving each unit's probability of being
included in the sample. These are the quantities inverse-probability
weights are built from: weight each sampled unit by the reciprocal of
its value here.

## See also

[`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md),
[`draw_rs()`](https://declaredesign.org/r/randomizr/reference/draw_rs.md)

## Examples

``` r

# A stratified design in which the strata are sampled at different rates
strata <- rep(c("A", "B", "C"), times = c(50, 100, 200))

declaration <- declare_rs(strata = strata, strata_n = c(20, 30, 40))

observed_probabilities <-
   obtain_inclusion_probabilities(declaration = declaration)

table(strata, observed_probabilities)
#>       observed_probabilities
#> strata 0.2 0.3 0.4
#>      A   0   0  50
#>      B   0 100   0
#>      C 200   0   0

# The weights for an inverse-probability-weighted analysis
ipw <- 1 / observed_probabilities


# Sometimes it is convenient to skip the declaration step
observed_probabilities <-
   obtain_inclusion_probabilities(strata = strata, strata_n = c(20, 30, 40))

table(strata, observed_probabilities)
#>       observed_probabilities
#> strata 0.2 0.3 0.4
#>      A   0   0  50
#>      B   0 100   0
#>      C 200   0   0
```
