# Create a two-by-two factorial design

Builds a two-by-two factorial design in which assignments to each factor
are independent of each other.

## Usage

``` r
two_by_two_designer(
  N = 100,
  prob_A = 0.5,
  prob_B = 0.5,
  weight_A = 0.5,
  weight_B = 0.5,
  outcome_means = rep(0, 4),
  mean_A0B0 = outcome_means[1],
  mean_A0B1 = outcome_means[2],
  mean_A1B0 = outcome_means[3],
  mean_A1B1 = outcome_means[4],
  sd_i = 1,
  outcome_sds = rep(0, 4),
  args_to_fix = NULL
)
```

## Arguments

- N:

  An integer. Size of sample.

- prob_A:

  A number in \[0,1\]. Probability of assignment to treatment A.

- prob_B:

  A number in \[0,1\]. Probability of assignment to treatment B.

- weight_A:

  A number. Weight placed on A=1 condition in definition of "average
  effect of B" estimand.

- weight_B:

  A number. Weight placed on B=1 condition in definition of "average
  effect of A" estimand.

- outcome_means:

  A vector of length 4. Average outcome in each A,B condition, in order
  AB = 00, 01, 10, 11. Values overridden by mean_A0B0, mean_A0B1,
  mean_A1B0, mean_A1B1, if provided.

- mean_A0B0:

  A number. Mean outcome in A=0, B=0 condition.

- mean_A0B1:

  A number. Mean outcome in A=0, B=1 condition.

- mean_A1B0:

  A number. Mean outcome in A=1, B=0 condition.

- mean_A1B1:

  A number. Mean outcome in A=1, B=1 condition.

- sd_i:

  A nonnegative scalar. Standard deviation of individual-level shock
  (common across arms).

- outcome_sds:

  A nonnegative vector of length 4. Standard deviation of (additional)
  unit level shock in each condition, in order AB = 00, 01, 10, 11.

- args_to_fix:

  A character vector. Names of arguments to be args_to_fix in design.

## Value

A two-by-two factorial design.

## Details

Three types of estimand are declared. First, weighted averages of the
average treatment effects of each treatment, given the two conditions of
the other treatments. Second and third, the difference in treatment
effects of each treatment, given the conditions of the other treatment.

Units are assigned to treatment using complete random assignment.
Potential outcomes follow a normal distribution.

Treatment A is assigned first and then Treatment B within blocks defined
by treatment A. Thus, if there are 6 units 3 are guaranteed to receive
treatment A but the number receiving treatment B is stochastic.

See
[`multi_arm_designer`](https://declaredesign.org/r/designlibrary/reference/multi_arm_designer.md)
for a factorial design with non independent assignments.

## Author

[DeclareDesign Team](https://declaredesign.org/)

## Examples

``` r
design <- two_by_two_designer(outcome_means = c(0,0,0,1))

# A design biased for the specified estimands:
design <- two_by_two_designer(outcome_means = c(0,0,0,1), prob_A = .8, prob_B = .2)
if (FALSE) { # \dontrun{
diagnose_design(design)
} # }

# A design with estimands that "match" the assignment:
design <- two_by_two_designer(outcome_means = c(0,0,0,1), 
                                    prob_A = .8, prob_B = .2, 
                                    weight_A = .8, weight_B = .2)
if (FALSE) { # \dontrun{
diagnose_design(design)
} # }

# Compare power with and without interactions, given same average effects in each arm
designs <- expand_design(two_by_two_designer, 
                    outcome_means = list(c(0,0,0,1), c(0,.5,.5,1)))
if (FALSE) { # \dontrun{
diagnose_design(designs)
} # }
```
