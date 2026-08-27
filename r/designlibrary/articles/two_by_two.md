# Two-by-Two Factorial

Studying causal mechanisms is hard. One of the best ways we know to
study if one cause works better or worse when another one is present is
through a two-by-two experiment. As its name suggests, the design
involves two overlapping two-arm experiments.

Before conducting studies of this kind, researchers often want to
increase their confidence in their ability to detect significant
interactions when the effect of $`Z_1`$ on $`Y`$ really is a function of
$`Z_2`$. How many subjects are needed to raise the probability of
correctly inferring that there is an interaction 80%?

This is a surprisingly difficult question to answer, because answering
it accurately depends upon the specific model of the potential outcomes
the researcher has in mind. As a result, many researchers use rules of
thumb that may lead to systematic over- or under-confidence. Using the
MIDA framework and design simulation, however, we can provide a flexible
answer to this question that does not rely on rules of thumb.

## Design Declaration

- **M**odel:

  We specify $`Z_1`$ and $`Z_2`$ do not have any effect on the outcome
  when only one of the causal agents becomes present. When both are
  present the combination of the causal factors produces an increase of
  1/10th of a standard deviation in the outcome.

- **I**nquiry:

  We can express the effect of $`Z_A`$ when $`Z_B`$ is present as
  $`\tau_{Z_A \mid Z_B} = E[(Y \mid Z_A = 1, Z_B = 1) - (Y \mid Z_A = 0, Z_B = 1)]`$,
  and the effect of $`Z_1`$ when $`Z_2`$ is absent as
  $`\tau_{Z_A \mid \neg Z_B} = E[(Y \mid Z_A = 1, Z_B = 0) - (Y \mid Z_A = 0, Z_B = 0)]`$.
  Thus, our estimand is
  $`\tau_{Z_A \mid Z_B} - \tau_{Z_A \mid \neg Z_B}`$: the difference in
  the effect of $`Z_A`$ induced by moving $`Z_B`$ from 0 to 1. Our
  design also features estimands that involve a weighted average of
  $`\tau_{Z_A \mid Z_B}`$ and $`\tau_{Z_A \mid \neg Z_B}`$ (with
  equivalent expressions for the effect of $`B`$). We’re going to weight
  the average so that our non-interaction estimands are equivalent to
  the effect of each treatment when the other one is absent.

- **D**ata strategy:

  We randomly assign an equal number of subjects to one of four
  conditions, by blocking the assignment of $`B`$ on the assignment of
  $`A`$. In the first both causal factors are absent, in the second and
  third only $`A`$ or $`B`$ is present, respectively, and in the fourth
  both are present.

- **A**nswer strategy:

  We estimate the interaction effect using a linear regression model
  that focuses on the coefficient on the $`Z_A \times Z_B`$ term.

``` r

N <- 100
prob_A <- 0.5
prob_B <- 0.5
weight_A <- 0
weight_B <- 0
mean_A0B0 <- 0
mean_A0B1 <- 0
mean_A1B0 <- 0
mean_A1B1 <- 0.1
sd_i <- 1
outcome_sds <- c(0, 0, 0, 0)

population <- declare_population(N, u = rnorm(N, sd = sd_i))
potential_outcomes <- declare_potential_outcomes(Y_A_0_B_0 = mean_A0B0 + 
    u + rnorm(N, sd = outcome_sds[1]), Y_A_0_B_1 = mean_A0B1 + 
    u + rnorm(N, sd = outcome_sds[2]), Y_A_1_B_0 = mean_A1B0 + 
    u + rnorm(N, sd = outcome_sds[3]), Y_A_1_B_1 = mean_A1B1 + 
    u + rnorm(N, sd = outcome_sds[4]))
estimand_1 <- declare_inquiry(ate_A = weight_B * mean(Y_A_1_B_1 - 
    Y_A_0_B_1) + (1 - weight_B) * mean(Y_A_1_B_0 - Y_A_0_B_0))
estimand_2 <- declare_inquiry(ate_B = weight_A * mean(Y_A_1_B_1 - 
    Y_A_1_B_0) + (1 - weight_A) * mean(Y_A_0_B_1 - Y_A_0_B_0))
estimand_3 <- declare_inquiry(interaction = mean((Y_A_1_B_1 - 
    Y_A_1_B_0) - (Y_A_0_B_1 - Y_A_0_B_0)))
assign_A <- declare_assignment(A = complete_ra(N, prob = prob_A))
assign_B <- declare_assignment(B = block_ra(prob = prob_B, 
    blocks = A))
reveal_Y <- declare_reveal(Y_variables = Y, assignment_variables = c(A, 
    B))
estimator_1 <- declare_estimator(Y ~ A + B, .method = lm_robust, 
    term = c("A", "B"), inquiry = c("ate_A", "ate_B"), label = "No_Interaction")
estimator_2 <- declare_estimator(Y ~ A + B + A:B, .method = lm_robust, 
    term = "A:B", inquiry = "interaction", label = "Interaction")
two_by_two_design <- population + potential_outcomes + estimand_1 + 
    estimand_2 + estimand_3 + assign_A + assign_B + reveal_Y + 
    estimator_1 + estimator_2
```

### Takeaways

``` r

diagnosis <- diagnose_design(two_by_two_design, sims = 25)
```

    ## Warning: We recommend you choose a number of simulations higher than 30.

| Inquiry | Outcome | N Sims | Mean Estimand | Mean Estimate | Bias | SD Estimate | RMSE | Power | Coverage |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| ate_A | Y | 25 | 0.00 | 0.05 | 0.05 | 0.16 | 0.17 | 0.00 | 1.00 |
|  |  |  | (0.00) | (0.03) | (0.03) | (0.02) | (0.02) | (0.00) | (0.00) |
| ate_B | Y | 25 | 0.00 | 0.01 | 0.01 | 0.19 | 0.18 | 0.00 | 1.00 |
|  |  |  | (0.00) | (0.04) | (0.04) | (0.02) | (0.02) | (0.00) | (0.00) |
| interaction | Y | 25 | 0.10 | 0.01 | -0.09 | 0.44 | 0.44 | 0.04 | 0.92 |
|  |  |  | (0.00) | (0.09) | (0.09) | (0.06) | (0.07) | (0.05) | (0.06) |

- Wow, the power is really low for our interaction! It’s only 4%. That’s
  because our estimator has to take account of the variation in both
  effects when estimating their difference. Note that the standard
  deviation of the interaction estimates is twice that of the estimates
  of the main effects.

- We also see that our estimates of the main effects are biased: we set
  out to estimate the effect of each treatment when the other was
  absent, but half the time the other treatment was present, so we get a
  boost in the *estimated* effect size due to the interaction.

### Exercises

1.  Alter the answer strategy so that the estimates of the main effects
    are no longer biased.

2.  Use `expand_designs()` with the
    [`two_by_two_designer()`](https://declaredesign.org/r/designlibrary/reference/two_by_two_designer.md)
    to determine the minimal interaction that can be detected with 80%
    power, holding other parameters constant.

3.  Alter the template so that outcomes are binary instead of normally
    distibuted. What is the expected standard error for the interaction
    term for a sample size of 1000? Discuss the implications of your
    diagnosis for practice.

Murray, David M. 1998. *Design and Analysis of Group-Randomized Trials*.
Vol. 29. Monographs in Epidemiology & B.
