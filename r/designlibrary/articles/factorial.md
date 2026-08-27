# Factorial Designs

A $`2^K`$ factorial design is an extension of the `two_by_two_designer`
that allows for $`k`$ number of factors (or treatments), each with two
levels, resulting in $`2^K`$ combinations of treatment conditions. You
might be interested in testing not just how two, but rather how three,
or how any number ($`k`$) of treatment conditions interact. The interest
in the effect of different treatments in the presence or absence of
other treatments is a key distinction from the `multi_arm_designer`, in
which we evaluate the effect of treatments where there is no overlap in
the assignment of different conditions.

Let’s say you set out to study how incentives and managerial practices
affect employee productivity *in combination*. Let’s imagine that
intervention $`A`$ is a salary increase conditional on performance
indicators, intervention $`B`$ is increased monitoring, and intervention
$`C`$ is a motivational activity. A key question might be the marginal
effect of conditional salary increases on productivity in the presence
of greater monitoring or in the presence of motivational interventions,
or both.

### Design Declaration

- **M**odel:

  We stipulate $`2^k`$ potential outcomes. In our example, with
  treatments $`\{A,B,C\} \in \{0,1\}`$, we have $`Y^{ABC}`$. We can
  denote the potential outcome of a unit who is in control for all
  treatments $`Y^{000}_i`$, for example, and that for a unit who is
  assigned to B and C only as $`Y^{011}`$. Each potential outcome is
  distributed normally with mean $`\mu_j`$ and standard deviation
  $`\sigma_j`$, where $`j \in \{1,...,2^k\}`$.

- **I**nquiry:

  Estimands are defined for each combination of treatment assignment as
  weighted averages of differences in potential outcomes. The effect of
  A in this design, $`\tau_A`$, averaged over conditions defined by B
  and C, is given by:
  $`\tau_A = 1/4*(Y^{111} - Y^{011}) + 1/4*(Y^{101} - Y^{001}) + 1/4*(Y^{110} - Y^{010}) + 1/4*(Y^{100} - Y^{000})`$.

- **D**ata strategy:

  We randomly assign units to one of the $`2^k`$ combinations of
  treatment using equal probabilities of assignment.

- **A**nswer strategy:

  Estimates draw from a regression in which all treatments are demeaned
  and weighted by the inverse probability of being in the condition they
  are in. In this demeaned regression, the constant captures the average
  outcome across all conditions — not the outcome when all units are in
  the control condition.

``` r

N <- 256
outcome_means <- c(0, 0, 0, 0, 0, 0, 0, 0)
outcome_sds <- c(1, 1, 1, 1, 1, 1, 1, 1)

population <- declare_population(N)
potential_outcomes <- declare_potential_outcomes(Y_0_0_0 = outcome_means[1L] + rnorm(N, 
    0, outcome_sds[1L]), Y_1_0_0 = outcome_means[2L] + rnorm(N, 
    0, outcome_sds[2L]), Y_0_1_0 = outcome_means[3L] + rnorm(N, 
    0, outcome_sds[3L]), Y_1_1_0 = outcome_means[4L] + rnorm(N, 
    0, outcome_sds[4L]), Y_0_0_1 = outcome_means[5L] + rnorm(N, 
    0, outcome_sds[5L]), Y_1_0_1 = outcome_means[6L] + rnorm(N, 
    0, outcome_sds[6L]), Y_0_1_1 = outcome_means[7L] + rnorm(N, 
    0, outcome_sds[7L]), Y_1_1_1 = outcome_means[8L] + rnorm(N, 
    0, outcome_sds[8L]))
reveal_Y <- declare_reveal(handler = function(data) {
    potential_cols <- mapply(paste, data[, c("T1", "T2", "T3"
    ), drop = FALSE], sep = "_", SIMPLIFY = FALSE)
    potential_cols <- do.call(paste, c("Y", potential_cols, sep = "_"))
    upoc <- unique(potential_cols)
    df <- data[, upoc, drop = FALSE]
    R <- seq_len(nrow(df))
    C <- match(potential_cols, colnames(df))
    data[, "Y"] <- df[cbind(R, C)]
    data
})
estimand <- declare_inquiry(Overall_average = mean(0.125 * Y_0_0_0 + 0.125 * 
    Y_0_0_1 + 0.125 * Y_0_1_0 + 0.125 * Y_0_1_1 + 0.125 * Y_1_0_0 + 
    0.125 * Y_1_0_1 + 0.125 * Y_1_1_0 + 0.125 * Y_1_1_1), te_T3 = mean(-0.25 * 
    Y_0_0_0 + 0.25 * Y_0_0_1 + -0.25 * Y_0_1_0 + 0.25 * Y_0_1_1 + 
    -0.25 * Y_1_0_0 + 0.25 * Y_1_0_1 + -0.25 * Y_1_1_0 + 0.25 * 
    Y_1_1_1), te_T2 = mean(-0.25 * Y_0_0_0 + -0.25 * Y_0_0_1 + 
    0.25 * Y_0_1_0 + 0.25 * Y_0_1_1 + -0.25 * Y_1_0_0 + -0.25 * 
    Y_1_0_1 + 0.25 * Y_1_1_0 + 0.25 * Y_1_1_1), `te_T2:T3` = mean(0.5 * 
    Y_0_0_0 + -0.5 * Y_0_0_1 + -0.5 * Y_0_1_0 + 0.5 * Y_0_1_1 + 
    0.5 * Y_1_0_0 + -0.5 * Y_1_0_1 + -0.5 * Y_1_1_0 + 0.5 * Y_1_1_1), 
    te_T1 = mean(-0.25 * Y_0_0_0 + -0.25 * Y_0_0_1 + -0.25 * 
        Y_0_1_0 + -0.25 * Y_0_1_1 + 0.25 * Y_1_0_0 + 0.25 * Y_1_0_1 + 
        0.25 * Y_1_1_0 + 0.25 * Y_1_1_1), `te_T1:T3` = mean(0.5 * 
        Y_0_0_0 + -0.5 * Y_0_0_1 + 0.5 * Y_0_1_0 + -0.5 * Y_0_1_1 + 
        -0.5 * Y_1_0_0 + 0.5 * Y_1_0_1 + -0.5 * Y_1_1_0 + 0.5 * 
        Y_1_1_1), `te_T1:T2` = mean(0.5 * Y_0_0_0 + 0.5 * Y_0_0_1 + 
        -0.5 * Y_0_1_0 + -0.5 * Y_0_1_1 + -0.5 * Y_1_0_0 + -0.5 * 
        Y_1_0_1 + 0.5 * Y_1_1_0 + 0.5 * Y_1_1_1), `te_T1:T2:T3` = mean(-1 * 
        Y_0_0_0 + 1 * Y_0_0_1 + 1 * Y_0_1_0 + -1 * Y_0_1_1 + 
        1 * Y_1_0_0 + -1 * Y_1_0_1 + -1 * Y_1_1_0 + 1 * Y_1_1_1), 
    label = "ATE")
assignment_factors <- declare_assignment(Z = complete_ra(N, conditions = 1:(2^3), prob_each = c(0.125, 
0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125)), Z_cond_prob = obtain_condition_probabilities(assignment = Z, 
    conditions = 1:(2^3), prob_each = c(0.125, 0.125, 0.125, 
    0.125, 0.125, 0.125, 0.125, 0.125)))
assignment <- declare_step(fabricate, T1 = as.numeric(Z %in% c(2L, 4L, 6L, 
8L)), T2 = as.numeric(Z %in% c(3L, 4L, 7L, 8L)), T3 = as.numeric(Z %in% 
    5:8))
estimator <- declare_estimator(handler = label_estimator(function(data) {
    data[, names(data) %in% c("T1", "T2", "T3")] <- data[, names(data) %in% 
        c("T1", "T2", "T3")] - 0.5
    mod <- lm_robust(formula = Y ~ T1 * T2 * T3, data = data, 
        weights = 1/Z_cond_prob)
    estimate_df <- tidy(mod)
    estimate_df$inquiry <- paste0("te_", estimate_df$term)
    estimate_df$inquiry[estimate_df$inquiry == "te_(Intercept)"] <- "Overall_average"
    estimate_df
}))
factorial_design <- population + potential_outcomes + assignment_factors + 
    assignment + reveal_Y + estimand + estimator
```

## Takeaways

``` r

diagnosis <- diagnose_design(factorial_design, sims = 25)
```

    ## Warning: We recommend you choose a number of simulations higher than 30.

| Design | Inquiry | Estimator | Outcome | Term | N Sims | Mean Estimand | Mean Estimate | Bias | SD Estimate | RMSE | Power | Coverage |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| factorial_design | Overall_average | estimator | Y | (Intercept) | 25 | 0.00 | 0.01 | 0.01 | 0.06 | 0.05 | 0.00 | 1.00 |
|  |  |  |  |  |  | (0.00) | (0.01) | (0.01) | (0.01) | (0.01) | (0.00) | (0.00) |
| factorial_design | te_T1 | estimator | Y | T1 | 25 | 0.02 | 0.01 | -0.00 | 0.15 | 0.15 | 0.16 | 0.88 |
|  |  |  |  |  |  | (0.01) | (0.03) | (0.03) | (0.02) | (0.02) | (0.07) | (0.06) |
| factorial_design | te_T1:T2 | estimator | Y | T1:T2 | 25 | 0.03 | -0.02 | -0.05 | 0.29 | 0.25 | 0.08 | 0.96 |
|  |  |  |  |  |  | (0.02) | (0.06) | (0.05) | (0.04) | (0.04) | (0.06) | (0.04) |
| factorial_design | te_T1:T2:T3 | estimator | Y | T1:T2:T3 | 25 | -0.05 | -0.13 | -0.08 | 0.32 | 0.34 | 0.00 | 1.00 |
|  |  |  |  |  |  | (0.04) | (0.06) | (0.06) | (0.05) | (0.06) | (0.00) | (0.00) |
| factorial_design | te_T1:T3 | estimator | Y | T1:T3 | 25 | -0.04 | 0.02 | 0.06 | 0.25 | 0.24 | 0.04 | 0.96 |
|  |  |  |  |  |  | (0.02) | (0.05) | (0.05) | (0.03) | (0.03) | (0.03) | (0.03) |
| factorial_design | te_T2 | estimator | Y | T2 | 25 | 0.00 | 0.02 | 0.02 | 0.10 | 0.09 | 0.00 | 1.00 |
|  |  |  |  |  |  | (0.01) | (0.02) | (0.02) | (0.01) | (0.01) | (0.00) | (0.00) |
| factorial_design | te_T2:T3 | estimator | Y | T2:T3 | 25 | 0.01 | -0.03 | -0.04 | 0.27 | 0.25 | 0.04 | 0.96 |
|  |  |  |  |  |  | (0.02) | (0.05) | (0.05) | (0.03) | (0.04) | (0.04) | (0.04) |
| factorial_design | te_T3 | estimator | Y | T3 | 25 | 0.00 | 0.01 | 0.01 | 0.15 | 0.15 | 0.04 | 1.00 |
|  |  |  |  |  |  | (0.01) | (0.03) | (0.03) | (0.02) | (0.01) | (0.04) | (0.00) |

- We see that the space of estimands we can estimate is amazingly rich,
  but our power to do so is very low.
