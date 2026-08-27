# Create a two-arm design with blocks and clusters

Builds a two-arm design with blocks and clusters.

## Usage

``` r
block_cluster_two_arm_designer(
  N = NULL,
  N_blocks = 1,
  N_clusters_in_block = ifelse(is.null(N), 100, round(N/N_blocks)),
  N_i_in_cluster = ifelse(is.null(N), 1, round(N/mean(N_blocks * N_clusters_in_block))),
  sd = 1,
  sd_block = 0.5773 * sd,
  sd_cluster = max(0, (sd^2 - sd_block^2)/2)^0.5,
  sd_i_0 = max(0, sd^2 - sd_block^2 - sd_cluster^2)^0.5,
  sd_i_1 = sd_i_0,
  rho = 1,
  assignment_probs = 0.5,
  control_mean = 0,
  ate = 0,
  treatment_mean = control_mean + ate,
  verbose = TRUE,
  args_to_fix = NULL
)
```

## Arguments

- N:

  An integer. Total number of units. Usually not specified as `N` is
  determined by `N_blocks`, `N_clusters_in_block`, and `N_i_in_cluster`.
  If `N_blocks`, and `N_clusters_in_block`, and `N_i_in_cluster` are
  specified then `N` is overridden. If these are not specified and `N`
  is specified then designer attempts to guess sizes of levels to
  approximate `N`, with preference for a design without blocks or
  clusters.

- N_blocks:

  An integer. Number of blocks. Defaults to 1 for no blocks.

- N_clusters_in_block:

  An integer or vector of integers of length `N_blocks`. Number of
  clusters in each block. This is the total `N` when `N_blocks` and
  `N_i_in_cluster` are at default values.

- N_i_in_cluster:

  An integer or vector of integers of length `sum(N_clusters_in_block)`.
  Individuals per cluster. Defaults to 1 for no clusters.

- sd:

  A nonnegative number. Overall standard deviation (combining individual
  level, cluster level, and block level shocks). Defaults to 1.
  Overridden if incompatible with other user-specified shocks.

- sd_block:

  A nonnegative number. Standard deviation of block level shocks.

- sd_cluster:

  A nonnegative number. Standard deviation of cluster level shock.

- sd_i_0:

  A nonnegative number. Standard deviation of individual level shock in
  control. If not specified, and when possible given `sd_block` and
  `sd_cluster`, `sd_i_0` defaults to make total variance = sd.

- sd_i_1:

  A nonnegative number. Standard deviation of individual level shock in
  treatment. Defaults to `sd_i_0`.

- rho:

  A number in \[-1,1\]. Correlation in individual shock between
  potential outcomes for treatment and control.

- assignment_probs:

  A number or vector of numbers in (0,1). Treatment assignment
  probability for each block (specified in order of
  `N_clusters_in_block`).

- control_mean:

  A number. Average outcome in control.

- ate:

  A number. Average treatment effect. Alternative to specifying
  `treatment_mean`. Note that `ate` is an argument for the designer but
  it does not appear as an argument in design code (design code uses
  `control_mean` and `treatment_mean` only).

- treatment_mean:

  A number. Average outcome in treatment. If `treatment_mean` is not
  provided then it is calculated as `control_mean + ate`. If both `ate`
  and `treatment_mean` are provided then only `treatment_mean` is used.

- verbose:

  Logical. If TRUE, prints intra-cluster correlation implied by design
  parameters.

- args_to_fix:

  A character vector. Names of arguments to be args_to_fix in design.

## Value

A block cluster two-arm design.

## Details

Units are assigned to treatment using complete block cluster random
assignment. Treatment effects can be specified either by providing
`control_mean` and `treatment_mean` or by specifying an `ate`.
Estimation uses differences in means accounting for blocks and clusters.

In the usual case `N` is not provided by the user but is determined by
`N_blocks`, `N_clusters_in_block`, `N_i_in_cluster` (when these are
integers `N` is the product of these three numbers).

Normal shocks can be specified at the individual, cluster, and block
levels. If individual level shocks are not specified and cluster and
block level variances sum to less than 1, then individual level shocks
are set such that total variance in outcomes equals 1.

Key limitations: The designer assumes covariance between potential
outcomes at the individual level only.

See [vignette
online](https://declaredesign.org/r/designlibrary/articles/block_cluster_two_arm.html).

## Author

[DeclareDesign Team](https://declaredesign.org/)

## Examples

``` r
# Generate a design using default arguments:
block_cluster_two_arm_design <- block_cluster_two_arm_designer()
#> [1] "The implied ICC in (control) is 0.667"
#> [1] "The implied ICC in (control) conditional on block is  0.5"
block_cluster_uneven <- block_cluster_two_arm_designer(
       N_blocks = 3, N_clusters_in_block = 2:4, N_i_in_cluster = 1:9)
#> [1] "The implied ICC in (control) is 0.667"
#> [1] "The implied ICC in (control) conditional on block is  0.5"
# A design in which number of clusters of cluster size is not specified
# but N and block size are:        
block_cluster_guess <- block_cluster_two_arm_designer(N = 24, N_blocks = 3)
#> [1] "The implied ICC in (control) is 0.667"
#> [1] "The implied ICC in (control) conditional on block is  0.5"
```
