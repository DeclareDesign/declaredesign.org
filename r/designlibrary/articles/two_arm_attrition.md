# Two-Arm Experiment with Attrition

You don’t always get the data you want. Very often, individuals who
would be relevant to your inquiry don’t show up in the data: people
refuse to answer surveys, data gets lost, collection activities are
interrupted.

When data goes missing, two things happen. First, your power goes down
because you have less data to work with relative to a study with
complete data. Second, you have to worry about whether there exists any
systematic relationship between missingness and the outcomes you are
studying. If such a relationship exists, it can introduce bias.

These features of attrition are fairly well-known. But how much
attrition is too much attrition? How high does the correlation between
the propensity to go missing and the outcome you care about have to be,
in order to seriously jeapordize a study? Here, we declare a design that
allows us to study such questions.

## Design Declaration

- **M**odel:

  Our model of the world specifies a population of $`N`$ units that have
  three variables affected by the treatment: a response variable,
  $`R_i`$; our outcome of interest, $`Y_i`$, which is correlated with
  the response variable through $`\rho`$; and $`Y^{obs}_i`$, the
  *measured* version of the true $`Y_i`$, which is only observed when
  $`R_i = 1`$.

- **I**nquiry:

  We want to know the average of all units’ differences in treated and
  untreated potential outcomes – the average treatment effect on the
  outcome of interest: $`E[Y_i(Z = 1) - Y_i(Z = 0)]`$. But we also want
  to know the average treatment effect on reporting,
  $`E[R_i(Z = 1) - R_i(Z = 0)]`$, as well as the effect of the treatment
  among those who report, $`E[Y_i(Z = 1) - Y_i(Z = 0) \mid R_i = 1]`$.

- **D**ata strategy:

  We randomly assign half of the units to treatment.

- **A**nswer strategy:

  For $`R_i`$ and $`Y^{obs}_i`$, we subtract the mean of the control
  group’s values from the mean of the treatment group in order to
  estimate the average treatment effect.

``` r

N <- 100
a_R <- 0
b_R <- 1
a_Y <- 0
b_Y <- 1
rho <- 0

population <- declare_population(N = N, u_R = rnorm(N), u_Y = rnorm(N, 
    mean = rho * u_R, sd = sqrt(1 - rho^2)))
potential_outcomes_R <- declare_potential_outcomes(R ~ (a_R + 
    b_R * Z > u_R))
potential_outcomes_Y <- declare_potential_outcomes(Y ~ (a_Y + 
    b_Y * Z > u_Y))
estimand_1 <- declare_inquiry(mean(R_Z_1 - R_Z_0), label = "ATE on R")
estimand_2 <- declare_inquiry(mean(Y_Z_1 - Y_Z_0), label = "ATE on Y")
estimand_3 <- declare_inquiry(mean((Y_Z_1 - Y_Z_0)[R == 1]), 
    label = "ATE on Y (Given R)")
assignment <- declare_assignment(Z = complete_ra(N, prob = 0.5))
reveal <- declare_reveal(outcome_variables = c("R", "Y"))
observed <- declare_step(Y_obs = ifelse(R, Y, NA), handler = fabricate)
estimator_1 <- declare_estimator(R ~ Z, term = "Z", inquiry = estimand_1, 
    label = "DIM on R")
estimator_2 <- declare_estimator(Y_obs ~ Z, term = "Z", inquiry = c(estimand_2, 
    estimand_3), label = "DIM on Y_obs")
estimator_3 <- declare_estimator(Y ~ Z, term = "Z", inquiry = c(estimand_2, 
    estimand_3), label = "DIM on Y")
two_arm_attrition_design <- population + potential_outcomes_R + 
    potential_outcomes_Y + assignment + reveal + observed + 
    estimand_1 + estimand_2 + estimand_3 + estimator_1 + 
    estimator_2 + estimator_3
```

## Takeaways

``` r

designs <- expand_design(designer = two_arm_attrition_designer, 
                         rho = c(0,.2,.8))
diagnoses <- diagnose_designs(designs, sims = 25)
```

    ## Warning: We recommend you choose a number of simulations higher than 30.
    ## Warning: We recommend you choose a number of simulations higher than 30.
    ## Warning: We recommend you choose a number of simulations higher than 30.

``` r

kable(reshape_diagnosis(diagnoses, exclude = c("Mean Estimand", "Mean Estimate",
                                               "SD Estimate", "RMSE", "Coverage")),
      digits = 2)
```

| Design   | rho | Inquiry            | Estimator    | Outcome | Term | N Sims | Bias   | Power  |
|:---------|:----|:-------------------|:-------------|:--------|:-----|:-------|:-------|:-------|
| design_1 | 0   | ATE on R           | DIM on R     | R       | Z    | 25     | -0.01  | 0.92   |
|          |     |                    |              |         |      |        | (0.01) | (0.06) |
| design_1 | 0   | ATE on Y           | DIM on Y     | Y       | Z    | 25     | 0.00   | 1.00   |
|          |     |                    |              |         |      |        | (0.01) | (0.00) |
| design_1 | 0   | ATE on Y           | DIM on Y_obs | Y_obs   | Z    | 25     | -0.03  | 0.80   |
|          |     |                    |              |         |      |        | (0.01) | (0.08) |
| design_1 | 0   | ATE on Y (Given R) | DIM on Y     | Y       | Z    | 25     | 0.01   | 1.00   |
|          |     |                    |              |         |      |        | (0.01) | (0.00) |
| design_1 | 0   | ATE on Y (Given R) | DIM on Y_obs | Y_obs   | Z    | 25     | -0.03  | 0.80   |
|          |     |                    |              |         |      |        | (0.01) | (0.08) |
| design_2 | 0.2 | ATE on R           | DIM on R     | R       | Z    | 25     | -0.01  | 1.00   |
|          |     |                    |              |         |      |        | (0.01) | (0.00) |
| design_2 | 0.2 | ATE on Y           | DIM on Y     | Y       | Z    | 25     | -0.01  | 0.96   |
|          |     |                    |              |         |      |        | (0.02) | (0.04) |
| design_2 | 0.2 | ATE on Y           | DIM on Y_obs | Y_obs   | Z    | 25     | -0.04  | 0.88   |
|          |     |                    |              |         |      |        | (0.02) | (0.07) |
| design_2 | 0.2 | ATE on Y (Given R) | DIM on Y     | Y       | Z    | 25     | 0.00   | 0.96   |
|          |     |                    |              |         |      |        | (0.02) | (0.04) |
| design_2 | 0.2 | ATE on Y (Given R) | DIM on Y_obs | Y_obs   | Z    | 25     | -0.03  | 0.88   |
|          |     |                    |              |         |      |        | (0.02) | (0.07) |
| design_3 | 0.8 | ATE on R           | DIM on R     | R       | Z    | 25     | 0.02   | 0.96   |
|          |     |                    |              |         |      |        | (0.02) | (0.04) |
| design_3 | 0.8 | ATE on Y           | DIM on Y     | Y       | Z    | 25     | 0.02   | 0.96   |
|          |     |                    |              |         |      |        | (0.01) | (0.04) |
| design_3 | 0.8 | ATE on Y           | DIM on Y_obs | Y_obs   | Z    | 25     | -0.18  | 0.36   |
|          |     |                    |              |         |      |        | (0.01) | (0.10) |
| design_3 | 0.8 | ATE on Y (Given R) | DIM on Y     | Y       | Z    | 25     | 0.08   | 0.96   |
|          |     |                    |              |         |      |        | (0.01) | (0.04) |
| design_3 | 0.8 | ATE on Y (Given R) | DIM on Y_obs | Y_obs   | Z    | 25     | -0.12  | 0.36   |
|          |     |                    |              |         |      |        | (0.01) | (0.10) |

- The diagnosis illustrates that the effect on reporting can always be
  estimated with high power and no bias

- However, any strategy that conditions on $`Y_i^{obs}`$ is very biased,
  even for an estimand that is conditional on reporting. Even a small
  amount of correlation between missingness and outcomes can severely
  jeapordize inferences.
