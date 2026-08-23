# probabilities of assignment: Simple Random Assignment

probabilities of assignment: Simple Random Assignment

## Usage

``` r
simple_ra_probabilities(
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

  The number of units. N must be a positive integer. (required)

- prob:

  Use for a two-arm design. prob is the probability of assignment to
  treatment and must be a real number between 0 and 1 inclusive and must
  be length 1. (optional)

- prob_unit:

  Use for a two-arm design. prob is the probability of assignment to
  treatment and must be a real number between 0 and 1 inclusive and must
  be length N. (optional)

- prob_each:

  Use for a multi-arm design in which the values of prob_each determine
  the probabilities of assignment to each treatment condition. prob_each
  must be a numeric vector giving the probability of assignment to each
  condition. All entries must be nonnegative real numbers between 0 and
  1 inclusive and the total must sum to 1. It may be a conditions-length
  vector or a N-by-conditions matrix. (optional)

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

- simple:

  logical. internal use only.

## Value

A matrix of probabilities of assignment

## Examples

``` r
# Two Group Designs
prob_mat <- simple_ra_probabilities(N=100)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.5    0.5
#> [2,]    0.5    0.5
#> [3,]    0.5    0.5
#> [4,]    0.5    0.5
#> [5,]    0.5    0.5
#> [6,]    0.5    0.5

prob_mat <- simple_ra_probabilities(N=100, prob=0.5)
head(prob_mat)
#>      prob_0 prob_1
#> [1,]    0.5    0.5
#> [2,]    0.5    0.5
#> [3,]    0.5    0.5
#> [4,]    0.5    0.5
#> [5,]    0.5    0.5
#> [6,]    0.5    0.5

prob_mat <- simple_ra_probabilities(N=100, prob_each = c(0.3, 0.7),
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
prob_mat <- simple_ra_probabilities(N=100, num_arms=3)
head(prob_mat)
#>        prob_T1   prob_T2   prob_T3
#> [1,] 0.3333333 0.3333333 0.3333333
#> [2,] 0.3333333 0.3333333 0.3333333
#> [3,] 0.3333333 0.3333333 0.3333333
#> [4,] 0.3333333 0.3333333 0.3333333
#> [5,] 0.3333333 0.3333333 0.3333333
#> [6,] 0.3333333 0.3333333 0.3333333

prob_mat <- simple_ra_probabilities(N=100, prob_each=c(0.3, 0.3, 0.4))
head(prob_mat)
#>      prob_T1 prob_T2 prob_T3
#> [1,]     0.3     0.3     0.4
#> [2,]     0.3     0.3     0.4
#> [3,]     0.3     0.3     0.4
#> [4,]     0.3     0.3     0.4
#> [5,]     0.3     0.3     0.4
#> [6,]     0.3     0.3     0.4

prob_mat <- simple_ra_probabilities(N=100, prob_each=c(0.3, 0.3, 0.4),
                        conditions=c("control", "placebo", "treatment"))
head(prob_mat)
#>      prob_control prob_placebo prob_treatment
#> [1,]          0.3          0.3            0.4
#> [2,]          0.3          0.3            0.4
#> [3,]          0.3          0.3            0.4
#> [4,]          0.3          0.3            0.4
#> [5,]          0.3          0.3            0.4
#> [6,]          0.3          0.3            0.4

prob_mat <- simple_ra_probabilities(N=100, conditions=c("control", "placebo", "treatment"))
head(prob_mat)
#>      prob_control prob_placebo prob_treatment
#> [1,]    0.3333333    0.3333333      0.3333333
#> [2,]    0.3333333    0.3333333      0.3333333
#> [3,]    0.3333333    0.3333333      0.3333333
#> [4,]    0.3333333    0.3333333      0.3333333
#> [5,]    0.3333333    0.3333333      0.3333333
#> [6,]    0.3333333    0.3333333      0.3333333
```
