# Two-Arm Experiment with a Covariate

We are often told to “control” for various different variables that
might be related to our independent or dependent variables. Does it
always make sense to control? Or are there instances when we better
estimate the effect of our explanatory variable on our dependent
variable by ignoring potential control variables?

We declare a design in which a researcher seeks to understand the causal
effect of explanatory variable, $`Z`$, on outcome variable $`Y`$. A
covariate, $`W`$, might be correlated with either or neither of these
variables.

In this particular setup, you do better by controlling – even in an
experiment – except when the covariate is correlated with the
explanatory variable but not with the outcome.

## Design Declaration

- **M**odel:

  Our model of the world specifies a population of $`N`$ units that have
  three observable variables. The binary treatment variable, $`Z`$, is
  potentially correlated with the covariate, $`W`$. The outcome
  variable, $`Y`$, is a function of the treatment variable, and may also
  be correlated with $`W`$. We refer to the correlations between $`W`$
  and $`Z`$ and between $`W`$ and $`Y`$ as $`\rho_{WZ}`$ and
  $`\rho_{WY}`$, respectively.

- **I**nquiry:

  We want to know the average of all units’ differences in treated and
  untreated potential outcomes – the average treatment effect on the
  outcome of interest: $`E[Y_i(Z = 1) - Y_i(Z = 0)]`$.

- **D**ata strategy:

  The variable $`Z`$ is not assigned to units by researchers – rather,
  it is assigned by the unobservable process, $`U_Z`$, which may be
  correlated with $`U_W`$.

- **A**nswer strategy:

  We consider three answer strategies. The first does not control for
  $`W`$ when estimating the effect of $`Z`$ on $`Y`$. The second
  controls for the average effect of $`W`$ on $`Y`$ when estimating the
  effect of $`Z`$ on $`Y`$. The third uses an estimator that averages
  over differences in the effect of $`Z`$ on $`Y`$ for different levels
  of $`W`$.

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

designs <- expand_design(
  designer = two_arm_covariate_designer,
  N = 30, rho_WY = c(0,.8), rho_WZ = c(0,.8), h = .5)
diagnoses <- diagnose_designs(designs, sims = 25)
```

    ## Warning: We recommend you choose a number of simulations higher than 30.
    ## Warning: We recommend you choose a number of simulations higher than 30.
    ## Warning: We recommend you choose a number of simulations higher than 30.
    ## Warning: We recommend you choose a number of simulations higher than 30.

| estimator     | rho_WZ | rho_WY |  bias | rmse |
|:--------------|-------:|-------:|------:|-----:|
| No controls   |    0.0 |    0.0 | -0.06 | 0.33 |
| With controls |    0.0 |    0.0 | -0.05 | 0.36 |
| Lin           |    0.0 |    0.0 | -0.05 | 0.37 |
| No controls   |    0.0 |    0.8 | -0.07 | 0.37 |
| With controls |    0.0 |    0.8 | -0.03 | 0.23 |
| Lin           |    0.0 |    0.8 | -0.04 | 0.21 |
| No controls   |    0.8 |    0.0 | -0.36 | 0.49 |
| With controls |    0.8 |    0.0 | -0.16 | 0.58 |
| Lin           |    0.8 |    0.0 | -0.16 | 0.57 |
| No controls   |    0.8 |    0.8 | -1.27 | 1.34 |
| With controls |    0.8 |    0.8 |  0.02 | 0.29 |
| Lin           |    0.8 |    0.8 |  0.01 | 0.27 |

- When $`W`$ is independent of both $`Z`$ and $`Y`$, it really doesn’t
  make much of a difference if you control or not

- When $`W`$ is predictive of $`Y`$ but not correlated with $`Z`$, you
  do strictly better by controlling for $`W`$. This is the case, for
  example, of experiments with prognostic covariates.

- When $`W`$ is correlated with $`Z`$ but not with $`Y`$, we can
  actually minimize root mean square error by not controlling. We’re
  better off leaving $`W`$ out because, while controlling provides no
  information on $`Y`$, it introduces colinearity between $`W`$ and
  $`Z`$.

- In the final three rows, we have a case of confounding: $`W`$ is
  correlated with both sides of the regression equation. Here the bias
  when we don’t control is very high; controlling helps a lot.
