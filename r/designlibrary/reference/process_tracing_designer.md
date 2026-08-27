# Create a process-tracing design

Builds a design in which two pieces of evidence are sought and used to
update about whether X caused Y using Bayes' rule.

## Usage

``` r
process_tracing_designer(
  N = 100,
  prob_X = 0.5,
  process_proportions = c(0.25, 0.25, 0.25, 0.25),
  prior_H = 0.5,
  p_E1_H = 0.8,
  p_E1_not_H = 0.2,
  p_E2_H = 0.3,
  p_E2_not_H = 0,
  cor_E1E2_H = 0,
  cor_E1E2_not_H = 0,
  label_E1 = "Straw in the Wind",
  label_E2 = "Smoking Gun",
  args_to_fix = NULL
)
```

## Arguments

- N:

  An integer. Size of population of cases from which a single case is
  selected.

- prob_X:

  A number in \[0,1\]. Probability that X = 1 for a given case (equal
  throughout population of cases).

- process_proportions:

  A vector of numbers in \[0,1\] that sums to 1. Simplex denoting the
  proportion of cases in the population in which, respectively: 1) X
  causes Y; 2) Y occurs regardless of X; 3) X causes the absence of
  Y; 4) Y is absent regardless of X.

- prior_H:

  A number in \[0,1\]. Prior probability that X causes Y in a given case
  in which X and Y are both present.

- p_E1_H:

  A number in \[0,1\]. Probability of observing first piece of evidence
  given hypothesis that X caused Y is true.

- p_E1_not_H:

  A number in \[0,1\]. Probability of observing first piece of evidence
  given hypothesis that X caused Y is not true.

- p_E2_H:

  A number in \[0,1\]. Probability of observing second piece of evidence
  given hypothesis that X caused Y is true.

- p_E2_not_H:

  A number in \[0,1\]. Probability of observing second piece of evidence
  given hypothesis that X caused Y is not true.

- cor_E1E2_H:

  A number in \[-1,1\]. Correlation between first and second pieces of
  evidence given hypothesis that X caused Y is true.

- cor_E1E2_not_H:

  A number in \[-1,1\]. Correlation between first and second pieces of
  evidence given hypothesis that X caused Y is not true.

- label_E1:

  A string. Label for the first piece of evidence (e.g., "Straw in the
  Wind").

- label_E2:

  A string. Label for the second piece of evidence (e.g., "Smoking
  Gun").

- args_to_fix:

  A character vector. Names of arguments to be args_to_fix in design.

## Value

A process-tracing design.

## Details

The model posits a population of `N` cases, each of which does or does
not exhibit the presence of some outcome, Y. With probability `prob_X`,
each case also exhibits the presence or absence of some potential cause,
X. The outcome Y can be realized through four distinct causal relations,
distributed through the population of cases according to
`process_proportions`. First, the presence of X might cause Y. Second,
the absence of X might cause Y. Third, Y might be present irrespective
of X. Fourth, Y might be absent irrespective of X.

Our inquiry is a "cause of effects" question. We wish to know whether a
specific case was one in which the presence (absence) of X caused the
presence (absence) of Y.

Our data strategy consists of selecting one case at random in which both
X and Y are present. As part of the data strategy we seek two pieces of
evidence in favor of or against the hypothesized causal relationship, H,
in which X causes Y.

The first (second) piece of evidence is observed with probability
`p_E1_H` (`p_E2_H`) when H is true, and with probability `p_E1_not_H`
(`p_E2_not_H`) when H is false.

Conditional on H being true (false), the correlation between the two
pieces of evidence is given by `cor_E1E2_H` (`cor_E1E2_not_H`).

The researcher uses Bayes’ rule to update about the probability that X
caused Y given the evidence. In other words, they form a posterior
inference, Pr(H\|E). We specify four answer strategies for forming this
inference. The first simply ignores the evidence and is equivalent to
stating a prior belief without doing any causal process tracing. The
second conditions inferences only on the first piece of evidence, and
the third only on the second piece of evidence. The fourth strategy
conditions posterior inferences on both pieces of evidence
simultaneously.

We specify as diagnosands for this design the bias, RMSE,
mean(estimand), mean(estimate) and sd(estimate).

## Author

[DeclareDesign Team](https://declaredesign.org/)

## Examples

``` r
# Generate a process-tracing design using default arguments:
pt_1 <- process_tracing_designer()
draw_estimands(pt_1)
#>         inquiry estimand
#> 1 did_X_cause_Y     TRUE
draw_estimates(pt_1)
#>                           estimator posterior_H result       inquiry
#> 1                  No tests (Prior)   0.5000000   TRUE did_X_cause_Y
#> 2                 Straw in the Wind   0.2000000  FALSE did_X_cause_Y
#> 3                       Smoking Gun   0.4117647  FALSE did_X_cause_Y
#> 4 Straw in the Wind and Smoking Gun   0.1489362     00 did_X_cause_Y
draw_data(pt_1)
#>    ID causal_process    X    Y S test_results   E1    E2
#> 1 043   Y_regardless TRUE TRUE 1           10 TRUE FALSE
if (FALSE) { # \dontrun{
diagnose_design(pt_1, sims = 1000)
} # }

# A design in which the smoking gun and straw-in-the-wind are correlated
pt_2 <- process_tracing_designer(cor_E1E2_H = .32)
if (FALSE) { # \dontrun{
diagnose_design(pt_2, sims = 1000)
} # }

# A design with two doubly-decisive tests pointing in opposite directions
pt_3 <- process_tracing_designer(p_E1_H = .80,p_E1_not_H = .05,
                                 label_E1 = "Doubly-Decisive: H",
                                 p_E2_H = .05,p_E2_not_H = .80,
                                 label_E2 = "Doubly-Decisive: Not H")
draw_estimates(pt_3)                                
#>                                       estimator posterior_H result
#> 1                              No tests (Prior)  0.50000000   TRUE
#> 2                            Doubly-Decisive: H  0.17391304  FALSE
#> 3                        Doubly-Decisive: Not H  0.05882353   TRUE
#> 4 Doubly-Decisive: H and Doubly-Decisive: Not H  0.01298701     01
#>         inquiry
#> 1 did_X_cause_Y
#> 2 did_X_cause_Y
#> 3 did_X_cause_Y
#> 4 did_X_cause_Y
if (FALSE) { # \dontrun{
diagnose_design(pt_3, sims = 1000)
} # }
```
