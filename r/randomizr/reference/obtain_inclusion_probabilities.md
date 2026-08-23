# Obtain inclusion probabilities

You can either give obtain_inclusion_probabilities() an declaration, as
created by
[`declare_rs`](https://declaredesign.org/r/randomizr/reference/declare_rs.md)
or you can specify the other arguments to describe a random sampling
procedure.  
  
This function is especially useful when units have different inclusion
probabilities and the analyst plans to use inverse-probability weights.

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
  [`declare_rs`](https://declaredesign.org/r/randomizr/reference/declare_rs.md).

- N:

  The number of units. N must be a positive integer. (required)

- strata:

  A vector of length N that indicates which stratum each unit belongs
  to.

- clusters:

  A vector of length N that indicates which cluster each unit belongs
  to.

- n:

  Use for a design in which n units (or clusters) are sampled. In a
  stratified design, exactly n units in each stratum will be sampled.
  (optional)

- n_unit:

  Under complete random sampling, must be constant across units. Under
  stratified random sampling, must be constant within strata.

- prob:

  Use for a design in which either floor(N\*prob) or ceiling(N\*prob)
  units (or clusters) are sampled. The probability of being sampled is
  exactly prob because with probability 1-prob, floor(N\*prob) units (or
  clusters) will be sampled and with probability prob, ceiling(N\*prob)
  units (or clusters) will be sampled. prob must be a real number
  between 0 and 1 inclusive. (optional)

- prob_unit:

  Must of be of length N. Under simple random sampling, can be different
  for each unit or cluster. Under complete random sampling, must be
  constant across units. Under stratified random sampling, must be
  constant within strata.

- strata_n:

  Use for a design in which strata_n describes the number of units to
  sample within each stratum.

- strata_prob:

  Use for a design in which strata_prob describes the probability of
  being sampled within each stratum. Differs from prob in that the
  probability of being sampled can vary across strata.

- simple:

  logical, defaults to FALSE. If TRUE, simple random sampling is used.
  When `simple = TRUE`, please do not specify n or strata_n. When
  `simple = TRUE`, `prob` may vary by unit.

- check_inputs:

  logical. Defaults to TRUE.

## Examples

``` r

# Draw a stratified random sample
strata <- rep(c("A", "B","C"), times=c(50, 100, 200))

declaration <- declare_rs(strata = strata)

observed_probabilities <-
   obtain_inclusion_probabilities(declaration = declaration)

table(strata, observed_probabilities)
#>       observed_probabilities
#> strata 0.5
#>      A  50
#>      B 100
#>      C 200


# Sometimes it is convenient to skip the declaration step
observed_probabilities <-
   obtain_inclusion_probabilities(strata = strata)

table(strata, observed_probabilities)
#>       observed_probabilities
#> strata 0.5
#>      A  50
#>      B 100
#>      C 200
```
