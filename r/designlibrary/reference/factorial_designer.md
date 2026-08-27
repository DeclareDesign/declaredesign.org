# Create a factorial design

A `2^k` factorial designer with `k` factors assigned with independent
probabilities. Results in `2^k` treatment combinations, each with
independent, normally distributed shocks. Estimands are average effects
and average interactions of given conditions, averaged over other
conditions. Estimation uses regression of demeaned variables with
propensity weights.

## Usage

``` r
factorial_designer(
  N = 256,
  k = 3,
  outcome_means = rep(0, 2^k),
  sd = 1,
  outcome_sds = rep(sd, 2^k),
  assignment_probs = rep(0.5, k),
  outcome_name = "Y",
  treatment_names = NULL,
  args_to_fix = NULL
)
```

## Arguments

- N:

  An integer. Size of sample.

- k:

  An integer. The number of factors in the design.

- outcome_means:

  A numeric vector of length `2^k`. Means for each of the `2^k`
  treatment combinations. See \`Details\` for the correct order of
  values.

- sd:

  A nonnegative number. Standard deviation for outcomes when all
  outcomes have identical standard deviations. For outcome-specific
  standard deviations use `outcomes_sds`.

- outcome_sds:

  A non negative numeric vector of length `2^k`. Standard deviations for
  each of the treatment combinations. See \`Details\` for the correct
  order of values.

- assignment_probs:

  A numeric vector of length `k`. Independent probability of assignment
  to each treatment.

- outcome_name:

  A character. Name of outcome variable (defaults to "Y"). Must be
  provided without spacing inside the function
  [`c()`](https://rdrr.io/r/base/c.html) as in
  `outcome_name = c("War")`.

- treatment_names:

  A character vector of length `k`. Name of treatment factors variable
  (defaults to "T1", "T2", ..., "Tk"). Must be provided without spacing.

- args_to_fix:

  A character vector. Names of arguments to be args_to_fix in design. By
  default `k`, `probs`, `outcome_name`, and `treatment_names` are always
  args_to_fix.

## Value

A factorial design.

## Details

`factorial_designer` creates a factorial design with `2^k` treatment
combinations resulting from `k` factors, each with two conditions each
(`c(0,1)`). The order of the scalar arguments `outcome_means` and
`outcome_sds` must follow the one returned by
`expand.grid(rep(list(c(0,1)), k))`, where each of the columns is a
treatment.

Estimands are defined for each combination of treatment assignment as
linear combinations of potential outcomes, typically weighted averages
of differences. Note that the weighting for the estimand does not
reflect treatment assignment probabilities but rather weights each
possible condition equally.

For example, in a design with \\k = 3\\ factors, the treatment effect of
A, (TE_A), averaged over conditions defined by B and C, is given by:
\$\$TE_A = 1/4\*(Y\_{111} - Y\_{011}) + 1/4\*(Y\_{101} - Y\_{001}) +
1/4\*(Y\_{110} - Y\_{010}) + 1/4\*(Y\_{100} - Y\_{000}).\$\$ The
"average interaction of A and B" — that is the average effect (for a
single unit) of A on the effect of B across conditions defined by C —
is: \$\$TE\_{AB} = 1/2\*\[(Y\_{111} - Y\_{011}) - (Y\_{101} -
Y\_{001})\] + 1/2\*\[(Y\_{110} - Y\_{010}) - (Y\_{100} -
Y\_{000})\].\$\$ And the triple interaction—that is, the effect of C on
the the effect of B on the effect of A is: \$\$TE\_{ABC} = \[(Y\_{111} -
Y\_{011}) - (Y\_{101} - Y\_{001})\] - \[(Y\_{110} - Y\_{010}) -
(Y\_{100} - Y\_{000})\],\$\$ where \\Y\_{abc}\\ is short for the
potential outcome of Y when A is a, B is b, and C is c.

Estimates draw from a regression in which all treatments are demeaned
and weighted by the inverse probability of being in the condition they
are in. Note that in this demeaned regression the constant captures the
average outcome across all conditions — not the outcome when all units
are in the control condition. The coefficient on T1 captures the average
effect of T1 across other conditions—not the effect of T1 when other
conditions are at 0. And so on.

## Author

[DeclareDesign Team](https://declaredesign.org/)

## Examples

``` r

# A factorial design using default arguments
factorial_design <- factorial_designer()

# A 2 x 2 x 2 factorial design with unequal probabilities of assignment to 
# each treatment condition. In this case the estimator weights up by the 
# conditional probabilities of assignment.
factorial_design_2 <- factorial_designer(k = 3, 
                                         assignment_probs = c(1/2, 1/4, 1/8), 
                                         outcome_means = c(0,0,0,0,0,0,0,4))
if (FALSE) { # \dontrun{
diagnose_design(factorial_design_2)
} # }
# Mapping from outcomes to estimands 
# The mapping between the potential outcomes schedule and the estimands of
# interest is not always easy. To help with intuition consider a 2^3 
# factorial design. You might like to think of a data generating process as
# a collection of marginal effects and interaction effects mapping from
# treatments to outcomes. 
# For instance: Y = -.25 + .75*X1 - .25*X2 -.25*X3 + X1*X2*X3
# The vector of implied potential outcome means as a function of conditions  
# could then be generated like this:

X <- expand.grid(rep(list(c(0,1)), 3))
outcome_means =  -.25 + X[,1]*3/4 - X[,2]/4 - X[,3]/4 + X[,1]*X[,2]*X[,3]
outcomes <- cbind(X, outcome_means)
colnames(outcomes) <- c("X1", "X2", "X3", "mean")
outcomes
#>   X1 X2 X3  mean
#> 1  0  0  0 -0.25
#> 2  1  0  0  0.50
#> 3  0  1  0 -0.50
#> 4  1  1  0  0.25
#> 5  0  0  1 -0.50
#> 6  1  0  1  0.25
#> 7  0  1  1 -0.75
#> 8  1  1  1  1.00

# Examination of the outcomes in this table reveals that there is an 
# average outcome of 0 (over all conditions), an average effect of treatment
# X1 of 1,  an average effects for X2 and X3 of 0,  the two way interactions 
# are .5 (averaged over conditions of the third treatment) and the triple 
# interaction is 1.
# These are exactly the estimands calculated by the designer and returned in 
# diagnosis.
factorial_design_3 <- factorial_designer(k = 3, 
                                         outcome_means = rep(1, 8),
                                         outcome_sds = rep(.01, 8))
if (FALSE) { # \dontrun{
library(DeclareDesign)
diagnose_design(factorial_design_3, sims = 10)
} # }

```
