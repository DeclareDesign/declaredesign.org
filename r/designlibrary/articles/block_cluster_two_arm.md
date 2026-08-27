# Block-Cluster-Two-Arm Design

Blocked cluster designs involve units that are grouped into clusters
which are in turn grouped into blocks.

The clusters are assigned as a group to either treatment or control.
When the clusters are of equal size the difference-in-means estimator
yields unbiased but possibly imprecise estimates, especially when
individuals are very similar within clusters and share characteristics
that can potentially influence outcomes.

In addition (optionally) clusters are assigned within blocks. Ideally
clusters within a block are similar. For instance blocks might be formed
on the basis of pre-treatment covariates. Treatment is then assigned
*within* each block. Two main objectives can be achieved with block
randomization: first, we can reduce sampling variability when
individuals that belong to a specific block have similar potential
outcomes. Secondly, we can ensure that certain characteristics that we
care about are present at a particular rate in our treatment and control
samples. For instance, policymakers might be interested in having an
equal number of women and men in treatment and control. By defining one
block as men and another as women and conducting the randomization
separately for each gender, we can ensure that exactly half of the men
and half of the women are treated.

As a rule of thumb having blocks improves precision but having clusters
weakens precision. Despite their drawbacks, cluster designs are
sometimes unavoidable. For instance, imagine that we want to evaluate
the effects of radio advertisements on donations to aid relief
campaigns. It is, of course, impossible to target such advertising to a
single individual, and the treatment must instead be assigned at a
higher level; for instance at the city level.

## Design Declaration

- **M**odel:

  We specify a population of `N` units partitioned into `N_blocks`
  (regions, say). Each block has `N_clusters_in_block` clusters (cities,
  say) and within each cluster there are `N_in_cluster` individuals. The
  variance of an individual’s outcomes has three components: the block’s
  variance, the cluster’s variance and the subject’s variance.

- **I**nquiry:

  We are interested in the average difference in individuals’ treatment
  and control outcomes, $`E[Y_i(Z = 1) - Y_i(Z = 0)]`$.

- **D**ata strategy:

  The units of randomization are cities (clusters). Cities are blocked
  by region so that we ensure that equal numbers of cities from each
  region are included in control and in treatment. All individuals
  belonging to the same city are either treated or assigned to control.

- **A**nswer strategy:

  We estimate the average treatment effect using a linear model that
  accounts for block structure and clusters.

``` r

N_blocks <- 4
N_clusters_in_block <- 4
N_i_in_cluster <- 5
sd_block <- 0.5773
sd_cluster <- 2
sd_i_0 <- 0
sd_i_1 <- 0
rho <- 1
assignment_probs <- 0.5
control_mean <- 0
treatment_mean <- 0
verbose <- TRUE

population <- declare_population(blocks = add_level(N = N_blocks, 
    u_b = rnorm(N) * sd_block), clusters = add_level(N = N_clusters_in_block, 
    u_c = rnorm(N) * sd_cluster, cluster_size = N_i_in_cluster), 
    i = add_level(N = N_i_in_cluster, u_0 = rnorm(N) * sd_i_0, 
        u_1 = rnorm(n = N, mean = rho * scale(u_0), sd = sqrt(1 - 
            rho^2)) * sd_i_1))
potential_outcomes <- declare_potential_outcomes(Y ~ (1 - 
    Z) * (control_mean + u_0 + u_b + u_c) + Z * (treatment_mean + 
    u_1 + u_b + u_c))
estimand <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
assignment <- declare_assignment(Z = block_and_cluster_ra(block_prob = assignment_probs, 
    blocks = blocks, clusters = clusters))
reveal <- declare_reveal(Y, Z)
estimator <- declare_estimator(Y ~ Z, inquiry = estimand, 
    .method = lm_robust, fixed_effects = ~blocks, clusters = clusters)
block_cluster_two_arm_design <- population + potential_outcomes + 
    estimand + assignment + reveal + estimator
```

## Takeaways

To better illustrate how different sources of variance differentially
affect our power, we compare three designs. Each of them features a
design with 50 blocks featuring two clusters each, with 10 individuals
per cluster. In each design the effect size is .25 and the standard
deviation in the outcome is 1 (when possible the designer defaults to a
variance for the individual shock such that total variance =1). In the
first, all of the variance derives from differences between individuals,
in the second it derives from differences between clusters, and in the
third it derives from variance between blocks.

``` r

individual_var <- block_cluster_two_arm_designer(N_blocks = 50, 
                                                 N_clusters_in_block = 2, 
                                                 N_i_in_cluster = 10, 
                                                 sd_block = 0,
                                                 sd_cluster = 0,
                                                 ate = .25)
```

    ## [1] "The implied ICC in (control) is 0"
    ## [1] "The implied ICC in (control) conditional on block is  0"

``` r

cluster_var   <- block_cluster_two_arm_designer(N_blocks = 50, 
                                                 N_clusters_in_block = 2,
                                                 N_i_in_cluster = 10, 
                                                 sd_block = 0,
                                                 sd_cluster = .99,
                                                 ate = .25)
```

    ## [1] "The implied ICC in (control) is 0.98"
    ## [1] "The implied ICC in (control) conditional on block is  0.98"

``` r

block_var     <- block_cluster_two_arm_designer(N_blocks = 50, 
                                                 N_clusters_in_block = 2,
                                                 N_i_in_cluster = 10, 
                                                 sd_block = .99,
                                                 sd_cluster = 0,
                                                 ate = .25)
```

    ## [1] "The implied ICC in (control) is 0.98"
    ## [1] "The implied ICC in (control) conditional on block is  0"

``` r

diagnosis <- diagnose_design(individual_var, cluster_var, block_var, sims = 25)
```

    ## Warning: We recommend you choose a number of simulations higher than 30.
    ## Warning: We recommend you choose a number of simulations higher than 30.
    ## Warning: We recommend you choose a number of simulations higher than 30.

| Estimator | Mean Estimand | Mean Estimate | Bias   | SD Estimate | RMSE   | Power  |
|:----------|:--------------|:--------------|:-------|:------------|:-------|:-------|
| estimator | 0.25          | 0.24          | -0.01  | 0.08        | 0.07   | 0.96   |
|           | (0.01)        | (0.02)        | (0.01) | (0.02)      | (0.01) | (0.04) |
| estimator | 0.25          | 0.26          | 0.01   | 0.21        | 0.21   | 0.24   |
|           | (0.00)        | (0.04)        | (0.04) | (0.03)      | (0.03) | (0.08) |
| estimator | 0.25          | 0.25          | 0.00   | 0.01        | 0.01   | 1.00   |
|           | (0.00)        | (0.00)        | (0.00) | (0.00)      | (0.00) | (0.00) |

Diagnosis reveals that the efficiency is greatest when variance comes
from blocks, and lowest when it comes from clusters. The reason for this
is that our estimator models block-level variance, and can estimate the
treatment effect independently from block-level effects because
treatment assignment is orthogonal to blocks. However, any variance at
the cluster level covaries with our treatment, and thus makes it harder
to estimate the treatment effect.

## References

Gerber, Alan S., and Donald P. Green. 2012. *Field Experiments: Design,
Analysis, and Interpretation*. W.W. Norton.
