# Mediation Analysis Design

By randomly assigning units to treatment we can determine whether a
treatment affects an outcome but not *why* or *how* it might affect it.
Identifying causal mechanisms is not a simple task. It involves complex
potential outcomes and a mediating variable that is generally not
assigned at random and is not a pre-treatment covariate (since it’s
affected by the treatment). Researchers often use regression-based
approaches to identify causal mechanisms but these rely on assumptions
that sometimes can’t be met.

For this analysis we assume that there is a non-zero average treatment
effect (ATE) of $`Z`$ on $`Y`$. Our main interest lies in decomposing
the ATE into *direct* and *indirect effects*. The indirect effect is
channeled from the treatment $`Z`$ to the outcome $`Y`$ through a
mediator $`M`$ and the direct effect runs directly from $`Z`$ to $`Y`$.

## Design Declaration

- **M**odel:

  We specify a population of size $`N`$. Individuals from this
  population have two potential outcomes related to the mediator.

  - $`M_i(Z_i=0):`$ The value for the mediator $`M`$ when the unit $`i`$
    is in the control group, and

  - $`M_i(Z_i=1):`$ the value for the mediator $`M`$ when the unit $`i`$
    is treated.

  Additionally, individuals have four potential outcomes related to
  $`Y`$.

  Two can be observed under treatment or control conditions.

  - $`Y_i(Z_i=0 , M_i(Z_i=0)):`$ the outcome of unit $`i`$ when the
    treatment is absent and the mediator takes the value it would when
    the treatment is absent.

  - $`Y_i(Z_i=1 , M_i(Z_i=1)):`$ the outcome of unit $`i`$ when the
    treatment is present and the mediator takes the value it would when
    the treatment is present.

  And two complex potential outcomes.

  - $`Y_i(Z_i=1 , M_i(Z_i=0)):`$ the outcome of unit $`i`$ when the
    treatment is present *but* the mediator takes the value it would
    when the treatment is *absent*.

  - $`Y_i(Z_i=0 , M_i(Z_i=1)):`$ the outcome of unit $`i`$ when the
    treatment is absent *but* the mediator takes the value it would when
    the treatment is *present*.

Thus the data generating process we specify defines $`Y`$ as a function
of $`M`$ and $`Z`$ and $`M`$ as a function of $`Z`$.

- **I**nquiry:

  We are interested in the average effects of the treatment on the
  mediator, $`M`$, the direct average effects of the treatment on $`Y`$
  and the effects on $`Y`$ from $`Z`$ that run through $`M`$.

- **D**ata strategy:

  We use assign units to treatment using complete random assignment.

- **A**nswer strategy:

  First, we regress $`M`$ on $`Z`$. Then we regress $`Y`$ on $`M`$ and
  $`Z`$.

``` r

N <- 200
a <- 1
b <- 0.4
c <- 0
d <- 0.5
rho <- 0

population <- declare_population(N = N, e1 = rnorm(N), e2 = rnorm(n = N, 
    mean = rho * e1, sd = sqrt(1 - rho^2)))
POs_M <- declare_potential_outcomes(M ~ 1 * (a * Z + e1 > 
    0))
POs_Y <- declare_potential_outcomes(Y ~ d * Z + b * M + c * 
    M * Z + e2, conditions = list(M = 0:1, Z = 0:1))
POs_Y_nat_0 <- declare_potential_outcomes(Y_nat0_Z_0 = b * 
    M_Z_0 + e2, Y_nat0_Z_1 = d + b * M_Z_0 + c * M_Z_0 + 
    e2)
POs_Y_nat_1 <- declare_potential_outcomes(Y_nat1_Z_0 = b * 
    M_Z_1 + e2, Y_nat1_Z_1 = d + b * M_Z_1 + c * M_Z_1 + 
    e2)
estimands <- declare_inquiries(FirstStage = mean(M_Z_1 - 
    M_Z_0), Indirect_0 = mean(Y_M_1_Z_0 - Y_M_0_Z_0), Indirect_1 = mean(Y_M_1_Z_1 - 
    Y_M_0_Z_1), Controlled_Direct_0 = mean(Y_M_0_Z_1 - Y_M_0_Z_0), 
    Controlled_Direct_1 = mean(Y_M_1_Z_1 - Y_M_1_Z_0), Natural_Direct_0 = mean(Y_nat0_Z_1 - 
        Y_nat0_Z_0), Natural_Direct_1 = mean(Y_nat1_Z_1 - 
        Y_nat1_Z_0))
assignment <- declare_assignment(Z = complete_ra(N, prob = 0.5))
reveal_M <- declare_reveal(M, Z)
reveal_Y <- declare_reveal(Y, assignment_variable = c("M", 
    "Z"))
reveal_nat0 <- declare_reveal(Y_nat0)
reveal_nat1 <- declare_reveal(Y_nat1)
manipulation <- declare_step(Not_M = 1 - M, handler = fabricate)
mediator_regression <- declare_estimator(M ~ Z, .method = lm_robust, 
    inquiry = "FirstStage", label = "Stage 1")
stage2_1 <- declare_estimator(Y ~ Z * M, .method = lm_robust, 
    term = c("M"), inquiry = c("Indirect_0"), label = "Stage 2")
stage2_2 <- declare_estimator(Y ~ Z * M, .method = lm_robust, 
    term = c("Z"), inquiry = c("Controlled_Direct_0", "Natural_Direct_0"), 
    label = "Direct_0")
stage2_3 <- declare_estimator(Y ~ Z * Not_M, .method = lm_robust, 
    term = c("Z"), inquiry = c("Controlled_Direct_1", "Natural_Direct_1"), 
    label = "Direct_1")
mediation_analysis_design <- population + POs_M + POs_Y + 
    POs_Y_nat_0 + POs_Y_nat_1 + estimands + assignment + 
    reveal_M + reveal_Y + reveal_nat0 + reveal_nat1 + manipulation + 
    mediator_regression + stage2_1 + stage2_2 + stage2_3
```

## Takeaways

We diagnose two versions of this design: one in which the correlation
between the error term of the mediator regression and one of the outcome
regression ($`\rho`$) is greater than zero, and another in which
$`\rho`$ equals zero.

``` r

designs <- expand_design(mediation_analysis_designer, rho = c(0,.5))
diagnosis <- diagnose_design(designs, sims = 25)
```

    ## Warning: We recommend you choose a number of simulations higher than 30.
    ## Warning: We recommend you choose a number of simulations higher than 30.

| rho | Inquiry | Estimator | Outcome | Term | N Sims | Mean Estimand | Mean Estimate | Bias | SD Estimate | RMSE | Power | Coverage |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| 0 | Controlled_Direct_0 | Direct_0 | Y | Z | 25 | 0.50 | 0.49 | -0.01 | 0.25 | 0.25 | 0.40 | 0.96 |
|  |  |  |  |  |  | (0.00) | (0.04) | (0.04) | (0.04) | (0.04) | (0.07) | (0.04) |
| 0 | Controlled_Direct_1 | Direct_1 | Y | Z | 25 | 0.50 | 0.45 | -0.05 | 0.15 | 0.16 | 0.80 | 0.96 |
|  |  |  |  |  |  | (0.00) | (0.03) | (0.03) | (0.03) | (0.03) | (0.08) | (0.04) |
| 0 | FirstStage | Stage 1 | M | Z | 25 | 0.35 | 0.35 | 0.00 | 0.04 | 0.05 | 1.00 | 1.00 |
|  |  |  |  |  |  | (0.00) | (0.01) | (0.01) | (0.01) | (0.01) | (0.00) | (0.00) |
| 0 | Indirect_0 | Stage 2 | Y | M | 25 | 0.40 | 0.43 | 0.03 | 0.18 | 0.18 | 0.60 | 1.00 |
|  |  |  |  |  |  | (0.00) | (0.04) | (0.04) | (0.02) | (0.02) | (0.10) | (0.00) |
| 0 | Indirect_1 | NA | NA | NA | 25 | 0.40 | NA | NA | NA | NA | NA | NA |
|  |  |  |  |  |  | (0.00) | NA | NA | NA | NA | NA | NA |
| 0 | Natural_Direct_0 | Direct_0 | Y | Z | 25 | 0.50 | 0.49 | -0.01 | 0.25 | 0.25 | 0.40 | 0.96 |
|  |  |  |  |  |  | (0.00) | (0.04) | (0.04) | (0.04) | (0.04) | (0.07) | (0.04) |
| 0 | Natural_Direct_1 | Direct_1 | Y | Z | 25 | 0.50 | 0.45 | -0.05 | 0.15 | 0.16 | 0.80 | 0.96 |
|  |  |  |  |  |  | (0.00) | (0.03) | (0.03) | (0.03) | (0.03) | (0.08) | (0.04) |
| 0.5 | Controlled_Direct_0 | Direct_0 | Y | Z | 25 | 0.50 | 0.07 | -0.43 | 0.31 | 0.53 | 0.08 | 0.68 |
|  |  |  |  |  |  | (0.00) | (0.06) | (0.06) | (0.05) | (0.07) | (0.05) | (0.09) |
| 0.5 | Controlled_Direct_1 | Direct_1 | Y | Z | 25 | 0.50 | 0.28 | -0.22 | 0.15 | 0.26 | 0.36 | 0.72 |
|  |  |  |  |  |  | (0.00) | (0.03) | (0.03) | (0.02) | (0.02) | (0.09) | (0.10) |
| 0.5 | FirstStage | Stage 1 | M | Z | 25 | 0.33 | 0.33 | 0.00 | 0.04 | 0.04 | 1.00 | 0.96 |
|  |  |  |  |  |  | (0.01) | (0.01) | (0.01) | (0.01) | (0.01) | (0.00) | (0.04) |
| 0.5 | Indirect_0 | Stage 2 | Y | M | 25 | 0.40 | 1.15 | 0.75 | 0.16 | 0.77 | 1.00 | 0.00 |
|  |  |  |  |  |  | (0.00) | (0.03) | (0.03) | (0.02) | (0.03) | (0.00) | (0.00) |
| 0.5 | Indirect_1 | NA | NA | NA | 25 | 0.40 | NA | NA | NA | NA | NA | NA |
|  |  |  |  |  |  | (0.00) | NA | NA | NA | NA | NA | NA |
| 0.5 | Natural_Direct_0 | Direct_0 | Y | Z | 25 | 0.50 | 0.07 | -0.43 | 0.31 | 0.53 | 0.08 | 0.68 |
|  |  |  |  |  |  | (0.00) | (0.06) | (0.06) | (0.05) | (0.07) | (0.05) | (0.09) |
| 0.5 | Natural_Direct_1 | Direct_1 | Y | Z | 25 | 0.50 | 0.28 | -0.22 | 0.15 | 0.26 | 0.36 | 0.72 |
|  |  |  |  |  |  | (0.00) | (0.03) | (0.03) | (0.02) | (0.02) | (0.09) | (0.10) |

Our diagnosis indicates that when the error terms are not correlated,
the direct and indirect effects can be estimated without bias. By
contrast, when $`\rho`$ does not equal zero, the regression
underestimates the effect of the mediator on $`Y`$ and overstates the
direct effects of $`Z`$ on $`Y`$.

Unfortunately, the assumption of no correlation is not always
guaranteed, since $`M`$ is not assigned at random and might be
correlated with $`Y`$.

## Further Reading

Gerber, Alan S., and Donald P. Green. 2012. *Field Experiments: Design,
Analysis, and Interpretation*. W.W. Norton.

Imai, Kosuke, Luke Keele, Dustin Tingley, and Teppei Yamamoto. 2011.
“Unpacking the Black Box of Causality: Learning about Causal Mechanisms
from Experimental and Observational Studies.” *American Political
Science Review* 105 (4): 765–89.
