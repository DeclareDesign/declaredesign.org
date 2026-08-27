# Pre-Test Post-Test Design

Pre-test post-test designs are designs in which researchers estimate the
change in outcomes before and after an intervention. These designs are
often preferred to post-test-only designs (which simply compare outcomes
between control and treatment group *after* treatment assignment),
because they enable much more efficient estimation and more informed
assessment of imbalance.

Nevertheless, baseline measurement often comes at a cost: when faced
with budget constraints, researchers may be forced to decrease endline
sample size in order to facilitate a baseline. Whether it is worth doing
so often depends on how well the baseline will predict outcomes at
endline.

Furthermore, there is much debate about how best to estimate treatment
effects in such designs: when are researchers better off using change
scores versus conditioning on the baseline?

Below we consider the example of a pre-test post-test applied to a study
that seeks to evaluate the effect of a family-planning program on the
incidence of teenage pregnancy.

## Design Declaration

- **M**odel:

  We define a population of size $`N`$, where effect at time $`t = 1`$
  (pre-program) and $`t = 2`$ (post-program) are taken from a normal
  distribution of mean 0 and standard deviation smaller than 1. We
  assume pre- and post-test outcomes to be highly and positively
  correlated ($`\rho = 0.5`$). We also expect subjects to leave the
  study at a rate of 10%, meaning we do not observe post-treatment
  outcomes for a tenth of the sample.

- **I**nquiry:

  We wish to know the average effect of family pregnancy programs $`Z`$
  on rates of teenage pregnancy. Formally:
  $`E[Y(Z = 1) - Y(Z = 0) \mid t = 2]`$, where $`Z = 1`$ denotes
  assignment to the program.

- **D**ata strategy:

  We observe the incidence of teenage pregnancy ($`Y_i`$) for individual
  $`i`$ for a sample of 100 individuals at time $`t = 1`$ (just prior to
  treatment) and at time $`t = 2`$ (a year after treatment). We randomly
  assign 50 out of 100 women between the ages of 15 and 19 to receive
  treatment.

- **A**nswer strategy:

  We define three estimators. First, we estimate effects on the
  \`\`change score’’: the dependent variable is defined as the
  difference between observed post- and pre-treatment outcomes. The
  second estimator treats only the post-treatment outcome as the
  dependent variable, but conditions on the pre-treatment outcome on the
  righthand side of the regression. Finally, we also look at effects
  when we only use post-test outcome measures, so as to evaluate the
  gain from using a baseline.

``` r

N <- 100
ate <- 0.25
sd_1 <- 1
sd_2 <- 1
rho <- 0.5
attrition_rate <- 0.1

population <- declare_population(N = N, u_t1 = rnorm(N) * 
    sd_1, u_t2 = rnorm(N, rho * scale(u_t1), sqrt(1 - rho^2)) * 
    sd_2, Y_t1 = u_t1)
potential_outcomes <- declare_potential_outcomes(Y_t2 ~ u_t2 + 
    ate * Z)
estimand <- declare_inquiry(ATE = mean(Y_t2_Z_1 - Y_t2_Z_0))
assignment <- declare_assignment(Z = complete_ra(N))
report <- declare_assignment(R = complete_ra(N, prob = 1 - 
    attrition_rate))
reveal_t2 <- declare_reveal(Y_t2)
manipulation <- declare_step(difference = (Y_t2 - Y_t1), 
    handler = fabricate)
pretest_lhs <- declare_estimator(difference ~ Z, .method = lm_robust, 
    inquiry = estimand, subset = R == 1, label = "Change score")
pretest_rhs <- declare_estimator(Y_t2 ~ Z + Y_t1, .method = lm_robust, 
    inquiry = estimand, subset = R == 1, label = "Condition on pretest")
posttest_only <- declare_estimator(Y_t2 ~ Z, .method = lm_robust, 
    inquiry = estimand, label = "Posttest only")
pretest_posttest_design <- population + potential_outcomes + 
    estimand + assignment + reveal_t2 + report + manipulation + 
    pretest_lhs + pretest_rhs + posttest_only
```

## Takeaways

``` r

diagnosis <- diagnose_design(pretest_posttest_design, sims = 25)
```

    ## Warning: We recommend you choose a number of simulations higher than 30.

| Estimator | Term | N Sims | Mean Estimand | Mean Estimate | Bias | SD Estimate | RMSE | Power | Coverage |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| Change score | Z | 25 | 0.25 | 0.19 | -0.06 | 0.17 | 0.17 | 0.08 | 1.00 |
|  |  |  | (0.00) | (0.03) | (0.03) | (0.02) | (0.02) | (0.06) | (0.00) |
| Condition on pretest | Z | 25 | 0.25 | 0.22 | -0.03 | 0.12 | 0.12 | 0.08 | 1.00 |
|  |  |  | (0.00) | (0.02) | (0.02) | (0.01) | (0.01) | (0.05) | (0.00) |
| Posttest only | Z | 25 | 0.25 | 0.25 | 0.00 | 0.17 | 0.17 | 0.24 | 1.00 |
|  |  |  | (0.00) | (0.03) | (0.03) | (0.02) | (0.02) | (0.08) | (0.00) |

- We see that the change score approach is less powerful than even the
  naive estimator! That’s because it essentially sums the variances from
  both periods. Any time invariant noise is being compounded by summing.
