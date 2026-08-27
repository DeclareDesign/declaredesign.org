# Create a design with multiple experimental arms

Creates a design with `m_arms` experimental arms, each assigned with
equal probability.

## Usage

``` r
multi_arm_designer(
  N = 30,
  m_arms = 4,
  outcome_means = rep(1, m_arms),
  sd_i = 1,
  outcome_sds = rep(0, m_arms),
  conditions = 1:m_arms,
  args_to_fix = NULL
)
```

## Arguments

- N:

  An integer. Sample size.

- m_arms:

  An integer. Number of arms.

- outcome_means:

  A numeric vector of length `m_arms`. Average outcome in each arm.

- sd_i:

  A nonnegative scalar. Standard deviation of individual-level shock
  (common across arms).

- outcome_sds:

  A nonnegative numeric vector of length `m_arms`. Standard deviations
  for condition-level shocks.

- conditions:

  A vector of length `m_arms`. The names of each arm. It can be given as
  numeric or character class (without blank spaces).

- args_to_fix:

  A character vector. Names of arguments to be args_to_fix in design. By
  default, `m_arms` and `conditions` are always args_to_fix.

## Value

A function that returns a design.

## Details

See [vignette
online](https://declaredesign.org/r/designlibrary/articles/multi_arm.html).

## Author

[DeclareDesign Team](https://declaredesign.org/)

## Examples

``` r

# To make a design using default arguments:
design <- multi_arm_designer()


# A design with different means and standard deviations in each arm
design <- multi_arm_designer(
        m_arms = 3,
        outcome_means = c(0, 0.5, 2), 
        outcome_sds =  c(1, 0.1, 0.5))

design <- multi_arm_designer(N = 80, m_arms = 4, outcome_means = 1:4,
                             args_to_fix = c("outcome_means", "outcome_sds"))
```
