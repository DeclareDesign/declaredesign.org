# Complete Random Assignment

complete_ra implements a random assignment procedure in which fixed
numbers of units are assigned to treatment conditions. The canonical
example of complete random assignment is a procedure in which exactly m
of N units are assigned to treatment and N-m units are assigned to
control.  
  
Users can set the exact number of units to assign to each condition with
m or m_each. Alternatively, users can specify probabilities of
assignment with prob or prob_each and complete_ra will infer the correct
number of units to assign to each condition. In a two-arm design,
complete_ra will either assign floor(N\*prob) or ceiling(N\*prob) units
to treatment, choosing between these two values to ensure that the
overall probability of assignment is exactly prob. In a multi-arm
design, complete_ra will first assign floor(N\*prob_each) units to their
respective conditions, then will assign the remaining units using simple
random assignment, choosing these second-stage probabilities so that the
overall probabilities of assignment are exactly prob_each.  
  
In most cases, users should specify N and not more than one of m,
m_each, prob, prob_each, or num_arms.  
  
If only N is specified, a two-arm trial in which N/2 units are assigned
to treatment is assumed. If N is odd, either floor(N/2) units or
ceiling(N/2) units will be assigned to treatment.

## Usage

``` r
complete_ra(
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

A vector of length N that indicates the treatment condition of each
unit. Is numeric in a two-arm trial and a factor variable (ordered by
conditions) in a multi-arm trial.

## Examples

``` r
# Two-arm Designs
Z <- complete_ra(N = 100)
table(Z)
#> Z
#>  0  1 
#> 50 50 

Z <- complete_ra(N = 100, m = 50)
table(Z)
#> Z
#>  0  1 
#> 50 50 

Z <- complete_ra(N = 100, m_unit = rep(50, 100))
table(Z)
#> Z
#>  0  1 
#> 50 50 

Z <- complete_ra(N = 100, prob = .111)
table(Z)
#> Z
#>  0  1 
#> 89 11 

Z <- complete_ra(N = 100, prob_unit = rep(0.1, 100))
table(Z)
#> Z
#>  0  1 
#> 90 10 

Z <- complete_ra(N = 100, conditions = c("control", "treatment"))
table(Z)
#> Z
#>   control treatment 
#>        50        50 


# Multi-arm Designs
Z <- complete_ra(N = 100, num_arms = 3)
table(Z)
#> Z
#> T1 T2 T3 
#> 34 33 33 

Z <- complete_ra(N = 100, m_each = c(30, 30, 40))
table(Z)
#> Z
#> T1 T2 T3 
#> 30 30 40 

Z <- complete_ra(N = 100, prob_each = c(.1, .2, .7))
table(Z)
#> Z
#> T1 T2 T3 
#> 10 20 70 

Z <- complete_ra(N = 100, conditions = c("control", "placebo", "treatment"))
table(Z)
#> Z
#>   control   placebo treatment 
#>        33        34        33 

# Special Cases
# Two-arm trial where the conditions are by default "T1" and "T2"
Z <- complete_ra(N = 100, num_arms = 2)
table(Z)
#> Z
#> T1 T2 
#> 50 50 

# If N = m, assign with 100% probability
complete_ra(N=2, m=2)
#> [1] 1 1

# Up through randomizr 0.12.0, 
complete_ra(N=1, m=1) # assigned with 50% probability
#> [1] 1
# This behavior has been deprecated
```
