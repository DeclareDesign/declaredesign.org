# Simple Random Assignment

simple_ra implements a random assignment procedure in which units are
independently assigned to treatment conditions. Because units are
assigned independently, the number of units that are assigned to each
condition can vary from assignment to assignment. For most experimental
applications in which the number of experimental units is known in
advance,
[`complete_ra`](https://declaredesign.org/r/randomizr/reference/complete_ra.md)
is better because the number of units assigned to each condition is
fixed across assignments.  
  
In most cases, users should specify N and not more than one of prob,
prob_each, or num_arms.  
  
If only N is specified, a two-arm trial with prob = 0.5 is assumed.

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

A vector of length N that indicates the treatment condition of each
unit. Is numeric in a two-arm trial and a factor variable (ordered by
conditions) in a multi-arm trial.

## Examples

``` r
# Two Group Designs

Z <- simple_ra(N=100)
table(Z)
#> Z
#>  0  1 
#> 53 47 

Z <- simple_ra(N=100, prob=0.5)
table(Z)
#> Z
#>  0  1 
#> 52 48 

Z <- simple_ra(N=100, prob_each = c(0.3, 0.7),
               conditions = c("control", "treatment"))
table(Z)
#> Z
#>   control treatment 
#>        38        62 

# Multi-arm Designs
Z <- simple_ra(N=100, num_arms=3)
table(Z)
#> Z
#> T1 T2 T3 
#> 41 35 24 

Z <- simple_ra(N=100, prob_each=c(0.3, 0.3, 0.4))
table(Z)
#> Z
#> T1 T2 T3 
#> 28 31 41 

Z <- simple_ra(N=100, prob_each=c(0.3, 0.3, 0.4),
               conditions=c("control", "placebo", "treatment"))
table(Z)
#> Z
#>   control   placebo treatment 
#>        26        25        49 

Z <- simple_ra(N=100, conditions=c("control", "placebo", "treatment"))
table(Z)
#> Z
#>   control   placebo treatment 
#>        27        38        35 
```
