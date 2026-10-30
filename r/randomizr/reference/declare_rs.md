# Declare a Random Sampling Procedure

`declare_rs` describes a sampling design once so that the rest of the
package can work from it. Pass the result to
[`draw_rs()`](https://declaredesign.org/r/randomizr/reference/draw_rs.md)
to draw a sample, or to
[`obtain_inclusion_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_inclusion_probabilities.md)
to recover each unit's probability of selection. Declaring is worth the
extra line whenever a design is drawn more than once, since the
probabilities are then computed from the same object that produced the
sample rather than reconstructed by hand.

## Usage

``` r
declare_rs(
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

An object of class `"rs_declaration"` (an environment, addressable like
a list) with entries:

- `rs_function`:

  A function that draws a random sample from the declared procedure.

- `rs_type`:

  A string indicating the type of random sampling used.

- `probabilities_vector`:

  A vector of length N giving each unit's probability of being included
  in the sample.

- `strata`:

  The stratification variable, if supplied.

- `clusters`:

  The clustering variable, if supplied.

## Details

`declare_rs` covers the same four designs as the sampling functions
themselves: simple, complete, stratified, and clustered, in any
combination. Which one it declares is inferred from the arguments given.

## See also

[`draw_rs()`](https://declaredesign.org/r/randomizr/reference/draw_rs.md),
[`obtain_inclusion_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_inclusion_probabilities.md),
[`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md)

## Examples

``` r
# A declaration is used in three ways.

# 1. To obtain some basic facts about a sampling procedure:

declaration <- declare_rs(N = 100, n = 30)
declaration
#> Random sampling procedure: Complete random sampling 
#> Number of units: 100 
#> The inclusion probabilities are constant across units.

# 2. To draw a random sample:

S <- draw_rs(declaration)
table(S)
#> S
#>  0  1 
#> 70 30 

# 3. To obtain inclusion probabilities:

probs <- obtain_inclusion_probabilities(declaration)
table(probs, S)
#>      S
#> probs  0  1
#>   0.3 70 30


# Simple Random Sampling Declarations

declare_rs(N = 100, simple = TRUE)
#> Random sampling procedure: Simple random sampling 
#> Number of units: 100 
#> The inclusion probabilities are constant across units.

declare_rs(N = 100, prob = 0.4, simple = TRUE)
#> Random sampling procedure: Simple random sampling 
#> Number of units: 100 
#> The inclusion probabilities are constant across units.


# Complete Random Sampling Declarations

declare_rs(N = 100)
#> Random sampling procedure: Complete random sampling 
#> Number of units: 100 
#> The inclusion probabilities are constant across units.

declare_rs(N = 100, n = 30)
#> Random sampling procedure: Complete random sampling 
#> Number of units: 100 
#> The inclusion probabilities are constant across units.


# Stratified Random Sampling Declarations

strata <- rep(c("A", "B", "C"), times = c(50, 100, 200))

declare_rs(strata = strata)
#> Random sampling procedure: Stratified random sampling 
#> Number of units: 350 
#> Number of strata: 3 
#> The inclusion probabilities are constant across units.

declare_rs(strata = strata, prob = 0.5)
#> Random sampling procedure: Stratified random sampling 
#> Number of units: 350 
#> Number of strata: 3 
#> The inclusion probabilities are constant across units.


# Cluster Random Sampling Declarations

clusters <- rep(letters[1:10], times = 1:10)

declare_rs(clusters = clusters)
#> Random sampling procedure: Cluster random sampling 
#> Number of units: 55 
#> Number of clusters: 10 
#> The inclusion probabilities are constant across units.

declare_rs(clusters = clusters, n = 4)
#> Random sampling procedure: Cluster random sampling 
#> Number of units: 55 
#> Number of clusters: 10 
#> The inclusion probabilities are constant across units.


# Stratified and Clustered Random Sampling Declarations

clusters <- rep(letters[1:12], times = 1:12)

strata <- rep(NA, length(clusters))
strata[clusters %in% letters[1:3]] <- "stratum_1"
strata[clusters %in% letters[4:6]] <- "stratum_2"
strata[clusters %in% letters[7:9]] <- "stratum_3"
strata[clusters %in% letters[10:12]] <- "stratum_4"

table(strata, clusters)
#>            clusters
#> strata       a  b  c  d  e  f  g  h  i  j  k  l
#>   stratum_1  1  2  3  0  0  0  0  0  0  0  0  0
#>   stratum_2  0  0  0  4  5  6  0  0  0  0  0  0
#>   stratum_3  0  0  0  0  0  0  7  8  9  0  0  0
#>   stratum_4  0  0  0  0  0  0  0  0  0 10 11 12

declare_rs(clusters = clusters, strata = strata)
#> Random sampling procedure: Stratified and clustered random sampling 
#> Number of units: 78 
#> Number of strata: 4 
#> Number of clusters: 12 
#> The inclusion probabilities are constant across units.

declare_rs(clusters = clusters, strata = strata, prob = 0.3)
#> Random sampling procedure: Stratified and clustered random sampling 
#> Number of units: 78 
#> Number of strata: 4 
#> Number of clusters: 12 
#> The inclusion probabilities are constant across units.
```
