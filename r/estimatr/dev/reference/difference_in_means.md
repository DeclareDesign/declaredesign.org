# Design-Based Difference-in-Means Estimator

Estimates an average treatment effect as a difference in means, choosing
the point estimator, the variance, and the degrees of freedom that match
the randomization rather than requiring you to name them. Unit, cluster,
blocked, block-cluster, matched-pair, and matched-pair cluster designs
are recognised, and the `design` element of the result reports which
case applied.

## Usage

``` r
difference_in_means(
  formula,
  data,
  blocks,
  clusters,
  weights,
  subset,
  se_type = c("default", "none"),
  condition1 = NULL,
  condition2 = NULL,
  ci = TRUE,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater")
)
```

## Arguments

- formula:

  (required) An object of class formula with one variable on the RHS

- data:

  (optional) A `data.frame`

- blocks:

  (optional) A bare (unquoted) name of the block variable

- clusters:

  (optional) A bare (unquoted) name of the cluster variable: the units
  to which treatment was assigned, so every unit in a cluster must share
  a treatment condition. To cluster standard errors above the level of
  assignment, use
  [`lm_robust()`](https://declaredesign.org/r/estimatr/dev/reference/lm_robust.md)
  with `clusters` and `se_type = "CR2"`.

- weights:

  (optional) The bare (unquoted) name of the weights variable

- subset:

  (optional) A bare (unquoted) expression specifying a subset

- se_type:

  (optional) `"default"` or `"none"`

- condition1:

  (optional) Value in treatment for the control condition

- condition2:

  (optional) Value in treatment for the treatment condition

- ci:

  (optional) Logical. Whether to compute p-values and confidence
  intervals.

- alpha:

  (optional) The significance level, 0.05 by default.

- alternative:

  (optional) The alternative hypothesis: `"two.sided"` (default),
  `"greater"` (the difference, `condition2` minus `condition1`, exceeds
  zero), or `"less"`. A one-sided test puts all of `alpha` in one tail,
  so its p-value is from that tail and its confidence interval is
  unbounded on the other side, as in
  [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html). Decide the
  direction before seeing the data.

## Value

An object of class `"difference_in_means"`, a list holding
`coefficients`, `std.error`, `df`, `statistic`, `p.value`, `conf.low`,
`conf.high`, `term`, `outcome`, `condition1`, `condition2`, `vcov`,
`nobs`, `alpha`, `alternative`, `call`, and `design`, a string naming
the case that applied: `"Standard"`, `"Blocked"`, `"Small blocks"`,
`"Hybrid blocked"`, `"Matched-pair"`, `"Clustered"`,
`"Block-clustered"`, or `"Matched-pair clustered"`.

## Details

Selects the appropriate point estimate, standard errors, and degrees of
freedom for unit randomized, cluster randomized, block randomized,
block-cluster randomized, matched-pairs, and matched-pair cluster
randomized designs.

**Blocks of different sizes.** For unit randomized blocks, blocks are
classified by how many units each arm holds rather than by how large the
block is. A block with at least two treated and two control units has
its own Neyman variance. A block with a single treated or single control
unit has no estimable within-block variance, so the variation across
such blocks stands in for it. A design containing both kinds combines
the two parts by squared share of the sample, following Pashley and
Miratrix (2021). The `design` element of the returned object reports
which case applied: `"Blocked"`, `"Matched-pair"`, `"Small blocks"`, or
`"Hybrid blocked"`.

Two designs are refused, because the variance genuinely cannot be
estimated: exactly one block with a singleton arm, and a set of
different-sized such blocks in which one holds half or more of their
units. Both messages suggest merging blocks or using
[`lm_robust()`](https://declaredesign.org/r/estimatr/dev/reference/lm_robust.md)
with block fixed effects.

Without blocks or clusters, weighted or not, the fit is
`lm_robust(y ~ z, se_type = "HC2", df_type = "welch")` on the two
conditions, in every returned number. The variance is \\\sum_g \sum\_{i
\in g} w_i^2 e_i^2 / (1 - w_i / W_g) / W_g^2\\, where \\W_g\\ is the
total weight in condition \\g\\ and \\e_i\\ the deviation from that
condition's weighted mean, and the degrees of freedom are
Welch-Satterthwaite's on its two terms; with constant weights these are
the unweighted Neyman variance and Welch's degrees of freedom. With
clusters and no blocks, the fit is `lm_robust(se_type = "CR2")`. Weights
are not supported with matched pairs.

**Blocks of clusters.** Pashley and Miratrix treat treatment assigned
within blocks, not blocks of clusters, so blocked designs with
`clusters` use the earlier estimators. Every block must have at least
two treated and two control clusters, unless the design is matched-pair
clustered, where the variance is estimated across blocks. A block with a
single treated or control cluster is refused: its within-block variance
is not estimable, and estimating it anyway understates the standard
error by roughly the block's cluster count.

## References

Gerber, Alan S. and Donald P. Green. 2012. *Field Experiments: Design,
Analysis, and Interpretation*. New York: W.W. Norton.

Imai, Kosuke, Gary King, and Clayton Nall. 2009. "The Essential Role of
Pair Matching in Cluster-Randomized Experiments." *Statistical Science*
24(1): 29-53. [doi:10.1214/08-STS274](https://doi.org/10.1214/08-STS274)
.

Pashley, Nicole E. and Luke W. Miratrix. 2021. "Insights on Variance
Estimation for Blocked and Matched Pairs Designs." *Journal of
Educational and Behavioral Statistics* 46(3): 271-296.
[doi:10.3102/1076998620946272](https://doi.org/10.3102/1076998620946272)
.

## Examples

``` r
set.seed(30)
dat <- data.frame(y = rnorm(100), z = rep(0:1, 50))

# Unblocked, unclustered: the Welch-corrected two-sample difference
fit <- difference_in_means(y ~ z, data = dat)
fit
#> Design:  Standard 
#>     Estimate Std. Error    t value  Pr(>|t|)   CI Lower  CI Upper       DF
#> z -0.1864076  0.2119164 -0.8796279 0.3812186 -0.6069631 0.2341479 97.73532
fit$design
#> [1] "Standard"

# Blocked designs use the Neyman variance within each block
dat_bl <- data.frame(
  bl = rep(1:10, each = 10),
  z  = rep(rep(0:1, each = 5), times = 10)
)
dat_bl$y <- rnorm(100) + 0.3 * dat_bl$z
difference_in_means(y ~ z, data = dat_bl, blocks = bl)
#> Design:  Blocked 
#>     Estimate Std. Error   t value  Pr(>|t|)   CI Lower  CI Upper DF
#> z 0.03347738  0.2101101 0.1593326 0.8738084 -0.3846551 0.4516098 80

# Matched pairs are recognised as such
dat_pr <- data.frame(pr = rep(1:50, each = 2), z = rep(c(0, 1), 50))
dat_pr$y <- rnorm(100) + 0.3 * dat_pr$z
difference_in_means(y ~ z, data = dat_pr, blocks = pr)$design
#> [1] "Matched-pair"

# Blocks of unequal shape, which earlier versions refused, use the
# Pashley and Miratrix (2021) estimators. `design` reports which case
# applied rather than leaving it to be inferred from the block sizes.
dat_hy <- rbind(dat_bl[c("bl", "z", "y")],
                transform(dat_pr[c("pr", "z", "y")], bl = pr + 100)[c("bl", "z", "y")])
difference_in_means(y ~ z, data = dat_hy, blocks = bl)$design
#> [1] "Hybrid blocked"

# Clustered assignment
dat_cl <- data.frame(cl = rep(1:20, each = 5))
dat_cl$z <- rep(rep(0:1, each = 5), times = 10)
dat_cl$y <- rnorm(100) + 0.3 * dat_cl$z
difference_in_means(y ~ z, data = dat_cl, clusters = cl)
#> Design:  Clustered 
#>    Estimate Std. Error  t value   Pr(>|t|)  CI Lower CI Upper DF
#> z 0.6286243  0.2326784 2.701688 0.01459924 0.1397852 1.117463 18
```
