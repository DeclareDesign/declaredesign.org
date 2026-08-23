# probabilities of assignment: Complete Random Assignment

probabilities of assignment: Complete Random Assignment

## Usage

``` r
complete_ra_probabilities(
  N,
  m = NULL,
  m_unit = NULL,
  m_each = NULL,
  prob = NULL,
  prob_unit = NULL,
  prob_each = NULL,
  num_arms = NULL,
  conditions = NULL,
  check_inputs = TRUE
)
```

## Arguments

- N:

  The number of units. N must be a positive integer. (required)

- m:

  Use for a two-arm design in which m units are assigned to treatment
  and N-m units are assigned to control. (optional)

- m_unit:

  Use for a two-arm design in which exactly unique(m_unit) units are
  assigned to treatment and the remainder are assigned to control.
  m_unit must be of length N and must be the same for all units
  (optional)

- m_each:

  Use for a multi-arm design in which the values of m_each determine the
  number of units assigned to each condition. m_each must be a numeric
  vector in which each entry is a nonnegative integer that describes how
  many units should be assigned to the 1st, 2nd, 3rd... treatment
  condition. m_each must sum to N. (optional)

- prob:

  Use for a two-arm design in which either floor(N\*prob) or
  ceiling(N\*prob) units are assigned to treatment. The probability of
  assignment to treatment is exactly prob because with probability
  1-prob, floor(N\*prob) units will be assigned to treatment and with
  probability prob, ceiling(N\*prob) units will be assigned to
  treatment. prob must be a real number between 0 and 1 inclusive.
  (optional)

- prob_unit:

  Use for a two-arm design. unique(prob_unit) will be passed to the prob
  argument and must be the same for all units.

- prob_each:

  Use for a multi-arm design in which the values of prob_each determine
  the probabilities of assignment to each treatment condition. prob_each
  must be a numeric vector giving the probability of assignment to each
  condition. All entries must be nonnegative real numbers between 0 and
  1 inclusive and the total must sum to 1. Because of integer issues,
  the exact number of units assigned to each condition may differ
  (slightly) from assignment to assignment, but the overall probability
  of assignment is exactly prob_each. (optional)

- num_arms:

  The number of treatment arms. If unspecified, num_arms will be
  determined from the other arguments. (optional)

- conditions:

  A character vector giving the names of the treatment groups. If
  unspecified, the treatment groups will be named 0 (for control) and 1
  (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm
  trial. An exception is a two-group design in which num_arms is set to
  2, in which case the condition names are T1 and T2, as in a multi-arm
  trial with two arms. (optional)

- check_inputs:

  logical. Defaults to TRUE.

## Value

A matrix of probabilities of assignment

## Examples

``` r
# 2-arm designs
prob_mat <- complete_ra_probabilities(N=100)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.5    0.5
#> [2,]    0.5    0.5
#> [3,]    0.5    0.5
#> [4,]    0.5    0.5
#> [5,]    0.5    0.5
#> [6,]    0.5    0.5

prob_mat <- complete_ra_probabilities(N=100, m=50)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.5    0.5
#> [2,]    0.5    0.5
#> [3,]    0.5    0.5
#> [4,]    0.5    0.5
#> [5,]    0.5    0.5
#> [6,]    0.5    0.5

prob_mat <- complete_ra_probabilities(N=100, prob = .3)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.7    0.3
#> [2,]    0.7    0.3
#> [3,]    0.7    0.3
#> [4,]    0.7    0.3
#> [5,]    0.7    0.3
#> [6,]    0.7    0.3

prob_mat <- complete_ra_probabilities(N=100, m_each = c(30, 70),
                          conditions = c("control", "treatment"))
head(prob_mat)
#>      prob_control prob_treatment
#> [1,]          0.3            0.7
#> [2,]          0.3            0.7
#> [3,]          0.3            0.7
#> [4,]          0.3            0.7
#> [5,]          0.3            0.7
#> [6,]          0.3            0.7

# Multi-arm Designs
prob_mat <- complete_ra_probabilities(N=100, num_arms=3)
head(prob_mat)
#>        prob_T1   prob_T2   prob_T3
#> [1,] 0.3333333 0.3333333 0.3333333
#> [2,] 0.3333333 0.3333333 0.3333333
#> [3,] 0.3333333 0.3333333 0.3333333
#> [4,] 0.3333333 0.3333333 0.3333333
#> [5,] 0.3333333 0.3333333 0.3333333
#> [6,] 0.3333333 0.3333333 0.3333333

prob_mat <- complete_ra_probabilities(N=100, m_each=c(30, 30, 40))
head(prob_mat)
#>      prob_T1 prob_T2 prob_T3
#> [1,]     0.3     0.3     0.4
#> [2,]     0.3     0.3     0.4
#> [3,]     0.3     0.3     0.4
#> [4,]     0.3     0.3     0.4
#> [5,]     0.3     0.3     0.4
#> [6,]     0.3     0.3     0.4

prob_mat <- complete_ra_probabilities(N=100, m_each=c(30, 30, 40),
                          conditions=c("control", "placebo", "treatment"))
head(prob_mat)
#>      prob_control prob_placebo prob_treatment
#> [1,]          0.3          0.3            0.4
#> [2,]          0.3          0.3            0.4
#> [3,]          0.3          0.3            0.4
#> [4,]          0.3          0.3            0.4
#> [5,]          0.3          0.3            0.4
#> [6,]          0.3          0.3            0.4

prob_mat <- complete_ra_probabilities(N=100, conditions=c("control", "placebo", "treatment"))
head(prob_mat)
#>      prob_control prob_placebo prob_treatment
#> [1,]    0.3333333    0.3333333      0.3333333
#> [2,]    0.3333333    0.3333333      0.3333333
#> [3,]    0.3333333    0.3333333      0.3333333
#> [4,]    0.3333333    0.3333333      0.3333333
#> [5,]    0.3333333    0.3333333      0.3333333
#> [6,]    0.3333333    0.3333333      0.3333333

prob_mat <- complete_ra_probabilities(N=100, prob_each = c(.2, .7, .1))
head(prob_mat)
#>      prob_T1 prob_T2 prob_T3
#> [1,]     0.2     0.7     0.1
#> [2,]     0.2     0.7     0.1
#> [3,]     0.2     0.7     0.1
#> [4,]     0.2     0.7     0.1
#> [5,]     0.2     0.7     0.1
#> [6,]     0.2     0.7     0.1
```
