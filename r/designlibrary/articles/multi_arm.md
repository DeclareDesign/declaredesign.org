# Multi-Arm Experiment

Learning about effects of causes takes time, money, and effort.
Multi-arm designs can make this process somewhat more efficient:
multiple treatments are assigned in parallel and compared against the
same control group, which reduces sample size and hence costs.

Suppose that we want to learn whether there is racial discrimination in
the labor market by conducting an experiment. Companies from our
population of size $`30`$ are randomly assigned to receive a résumé from
a white, black, or Latino candidate. Companies are assigned to one of
these three conditions with equal probabilities, and résumés only vary
race and are otherwise identical. We define our outcome of interest as
the difference in callbacks between experimental conditions.

In settings of multiple treatment arms, we could do a number of pairwise
comparisons: across treatments and each treatment against control.

## Design Declaration

- **M**odel:

  We specify a population of size $`N`$ where a unit $`i`$ has a
  potential outcome, $`Y_i(Z = 0)`$, when it remains untreated and
  $`M`$$`(m = 1, 2, ..., M)`$ potential outcomes defined according to
  the treatment that it receives. The effect of each treatment on the
  outcome of unit $`i`$ is equal to the difference in the potential
  outcome under treatment condition $`m`$ and the control condition:
  $`Y_i(Z = m)  -Y_i(Z = 0)`$.

- **I**nquiry:

  We are interested in all of the pairwise comparisons between arms:
  $`E[Y(m) - Y(m')]`$, for all $`m \in \{1,...,M\}`$.

- **D**ata strategy:

  We randomly assign $`k/N`$ units to each of the treatment arms.

- **A**nswer strategy:

  We take every pairwise difference in means corresponding to the
  specific estimand.

``` r

N <- 30
m_arms <- 4
outcome_means <- c(0.5, 1, 2, 0.5)
sd_i <- 1
outcome_sds <- c(0, 0, 0, 0)

population <- declare_model(N = N, u_1 = rnorm(N, 0, outcome_sds[1L]), u_2 = rnorm(N, 
    0, outcome_sds[2L]), u_3 = rnorm(N, 0, outcome_sds[3L]), 
    u_4 = rnorm(N, 0, outcome_sds[4L]), u = rnorm(N) * sd_i)
po <- declare_potential_outcomes(formula = Y ~ (outcome_means[1L] + 
    u_1) * (Z == "1") + (outcome_means[2L] + u_2) * (Z == "2") + 
    (outcome_means[3L] + u_3) * (Z == "3") + (outcome_means[4L] + 
    u_4) * (Z == "4") + u, conditions = list(Z = c("1", "2", 
"3", "4")))
estimand <- declare_inquiries(ate_Y_2_1 = mean(Y_Z_2 - Y_Z_1), ate_Y_3_1 = mean(Y_Z_3 - 
    Y_Z_1), ate_Y_4_1 = mean(Y_Z_4 - Y_Z_1), ate_Y_3_2 = mean(Y_Z_3 - 
    Y_Z_2), ate_Y_4_2 = mean(Y_Z_4 - Y_Z_2), ate_Y_4_3 = mean(Y_Z_4 - 
    Y_Z_3))
assignment <- declare_assignment(Z = complete_ra(N, num_arms = 4, conditions = c("1", 
"2", "3", "4")))
reveal_Y <- declare_reveal(assignment_variables = Z)
estimator <- declare_estimator(handler = function(data) {
    estimates <- rbind.data.frame(ate_Y_2_1 = difference_in_means(formula = Y ~ 
        Z, data = data, condition1 = "1", condition2 = "2"), 
        ate_Y_3_1 = difference_in_means(formula = Y ~ Z, data = data, 
            condition1 = "1", condition2 = "3"), ate_Y_4_1 = difference_in_means(formula = Y ~ 
            Z, data = data, condition1 = "1", condition2 = "4"), 
        ate_Y_3_2 = difference_in_means(formula = Y ~ Z, data = data, 
            condition1 = "2", condition2 = "3"), ate_Y_4_2 = difference_in_means(formula = Y ~ 
            Z, data = data, condition1 = "2", condition2 = "4"), 
        ate_Y_4_3 = difference_in_means(formula = Y ~ Z, data = data, 
            condition1 = "3", condition2 = "4"))
    names(estimates)[names(estimates) == "N"] <- "N_DIM"
    estimates$estimator <- c("DIM (Z_2 - Z_1)", "DIM (Z_3 - Z_1)", 
    "DIM (Z_4 - Z_1)", "DIM (Z_3 - Z_2)", "DIM (Z_4 - Z_2)", 
    "DIM (Z_4 - Z_3)")
    estimates$inquiry <- rownames(estimates)
    estimates$estimate <- estimates$coefficients
    estimates$term <- NULL
    return(estimates)
})
multi_arm_design <- population + po + assignment + reveal_Y + 
    estimand + estimator
```

## Takeaways

``` r

diagnosis <- diagnose_design(multi_arm_design, sims = 25)
```

- The pairwise differences in means provide unbiased estimates of the
  average treatment effect (ATE) in each arm.

- These estimates, however, are not so precise. The estimated standard
  deviation is large yielding wide confidence intervals that contain the
  true value of the ATEs more than 95% of the time.
