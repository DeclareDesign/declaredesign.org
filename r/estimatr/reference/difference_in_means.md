# Design-based difference-in-means estimator

Difference-in-means estimators that selects the appropriate point
estimate, standard errors, and degrees of freedom for a variety of
designs: unit randomized, cluster randomized, block randomized,
block-cluster randomized, matched-pairs, and matched-pair cluster
randomized designs

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
  alpha = 0.05
)
```

## Arguments

- formula:

  an object of class formula, as in
  [`lm`](https://rdrr.io/r/stats/lm.html), such as `Y ~ Z` with only one
  variable on the right-hand side, the treatment.

- data:

  A `data.frame`.

- blocks:

  An optional bare (unquoted) name of the block variable. Use for
  blocked designs only.

- clusters:

  An optional bare (unquoted) name of the variable that corresponds to
  the clusters in the data; used for cluster randomized designs. For
  blocked designs, clusters must nest within blocks.

- weights:

  the bare (unquoted) names of the weights variable in the supplied
  data.

- subset:

  An optional bare (unquoted) expression specifying a subset of
  observations to be used.

- se_type:

  An optional string that can be one of `c("default", "none")`. If
  "default" (the default), it will use the default standard error
  estimator for the design, and if "none" then standard errors will not
  be computed which may speed up run time if only the point estimate is
  required.

- condition1:

  value in the treatment vector of the condition to be the control.
  Effects are estimated with `condition1` as the control and
  `condition2` as the treatment. If unspecified, `condition1` is the
  "first" condition and `condition2` is the "second" according to levels
  if the treatment is a factor or according to a sortif it is a numeric
  or character variable (i.e if unspecified and the treatment is 0s and
  1s, `condition1` will by default be 0 and `condition2` will be 1). See
  the examples for more.

- condition2:

  value in the treatment vector of the condition to be the treatment.
  See `condition1`.

- ci:

  logical. Whether to compute and return p-values and confidence
  intervals, TRUE by default.

- alpha:

  The significance level, 0.05 by default.

## Value

Returns an object of class `"difference_in_means"`.

The post-estimation commands functions `summary` and
[`tidy`](https://generics.r-lib.org/reference/tidy.html) return results
in a `data.frame`. To get useful data out of the return, you can use
these data frames, you can use the resulting list directly, or you can
use the generic accessor functions `coef` and `confint`.

An object of class `"difference_in_means"` is a list containing at least
the following components:

- coefficients:

  the estimated difference in means

- std.error:

  the estimated standard error

- statistic:

  the t-statistic

- df:

  the estimated degrees of freedom

- p.value:

  the p-value from a two-sided t-test using `coefficients`, `std.error`,
  and `df`

- conf.low:

  the lower bound of the `1 - alpha` percent confidence interval

- conf.high:

  the upper bound of the `1 - alpha` percent confidence interval

- term:

  a character vector of coefficient names

- alpha:

  the significance level specified by the user

- N:

  the number of observations used

- outcome:

  the name of the outcome variable

- design:

  the name of the design learned from the arguments passed

## Details

This function implements a difference-in-means estimator, with support
for blocked, clustered, matched-pairs, block-clustered, and matched-pair
clustered designs. One specifies their design by passing the blocks and
clusters in their data and this function chooses which estimator is most
appropriate.

If you pass only `blocks`, if all blocks are of size two, we will infer
that the design is a matched-pairs design. If they are all size four or
larger, we will infer that it is a regular blocked design. If you pass
both `blocks` and `clusters`, we will similarly infer whether it is a
matched-pairs clustered design or a block-clustered design the number of
clusters per block. If the user passes only `clusters`, we will infer
that the design was cluster-randomized. If the user specifies neither
the `blocks` nor the `clusters`, a regular Welch's t-test will be
performed.

Importantly, if the user specifies weights, the estimation is handed off
to
[`lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
with the appropriate robust standard errors as weighted
difference-in-means estimators are not implemented here. More details of
the about each of the estimators can be found in the [mathematical
notes](https://declaredesign.org/r/estimatr/articles/mathematical-notes.html).

## References

Gerber, Alan S, and Donald P Green. 2012. Field Experiments: Design,
Analysis, and Interpretation. New York: W.W. Norton.

Imai, Kosuke, Gary King, Clayton Nall. 2009. "The Essential Role of Pair
Matching in Cluster-Randomized Experiments, with Application to the
Mexican Universal Health Insurance Evaluation." Statistical Science 24
(1). Institute of Mathematical Statistics: 29-53.
[doi:10.1214/08-STS274](https://doi.org/10.1214/08-STS274) .

## See also

[`lm_lin`](https://declaredesign.org/r/estimatr/reference/lm_lin.md)

## Examples

``` r

library(fabricatr)
library(randomizr)
# Get appropriate standard errors for unit-randomized designs

# ----------
# 1. Unit randomized
# ----------
dat <- fabricate(
  N = 100,
  Y = rnorm(100),
  Z_comp = complete_ra(N, prob = 0.4),
)

table(dat$Z_comp)
#> 
#>  0  1 
#> 60 40 
difference_in_means(Y ~ Z_comp, data = dat)
#> Design:  Standard 
#>          Estimate Std. Error t value  Pr(>|t|)   CI Lower  CI Upper       DF
#> Z_comp 0.02822768  0.2128302 0.13263 0.8947893 -0.3947386 0.4511939 87.83788

# ----------
# 2. Cluster randomized
# ----------
# Accurates estimates and standard errors for clustered designs
dat$clust <- sample(20, size = nrow(dat), replace = TRUE)
dat$Z_clust <- cluster_ra(dat$clust, prob = 0.6)

table(dat$Z_clust, dat$clust)
#>    
#>     1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
#>   0 5 0 4 0 5 0 0 2 7  0  0  0  4  0  0  0  0  0  5  3
#>   1 0 5 0 5 0 7 5 0 0  7  6  6  0  7  5  5  3  4  0  0
summary(difference_in_means(Y ~ Z_clust, clusters = clust, data = dat))
#> $coefficients
#>            Estimate Std. Error     t value  Pr(>|t|)   CI Lower  CI Upper
#> Z_clust -0.01514775  0.2606826 -0.05810801 0.9545717 -0.5799884 0.5496929
#>               DF
#> Z_clust 12.63249
#> 
#> $design
#> [1] "Clustered"
#> 

# ----------
# 3. Block randomized
# ----------
dat$block <- rep(1:10, each = 10)
dat$Z_block <- block_ra(dat$block, prob = 0.5)

table(dat$Z_block, dat$block)
#>    
#>     1 2 3 4 5 6 7 8 9 10
#>   0 5 5 5 5 5 5 5 5 5  5
#>   1 5 5 5 5 5 5 5 5 5  5
difference_in_means(Y ~ Z_block, blocks = block, data = dat)
#> Design:  Blocked 
#>            Estimate Std. Error    t value  Pr(>|t|)   CI Lower  CI Upper DF
#> Z_block -0.08359831  0.2077124 -0.4024715 0.6884105 -0.4969591 0.3297625 80

# ----------
# 4. Block cluster randomized
# ----------
# Learns this design if there are two clusters per block
dat$small_clust <- rep(1:50, each = 2)
dat$big_blocks <- rep(1:5, each = 10)

dat$Z_blcl <- block_and_cluster_ra(
  blocks = dat$big_blocks,
  clusters = dat$small_clust
 )

difference_in_means(
  Y ~ Z_blcl,
  blocks = big_blocks,
  clusters = small_clust,
  data = dat
 )
#> Design:  Block-clustered 
#>          Estimate Std. Error   t value  Pr(>|t|)   CI Lower  CI Upper DF
#> Z_blcl 0.09845335  0.2098115 0.4692467 0.6414411 -0.3255915 0.5224982 40

# ----------
# 5. Matched-pairs
# ----------
# Matched-pair estimates and standard errors are also accurate
# Specified same as blocked design, function learns that
# it is matched pair from size of blocks!
dat$pairs <- rep(1:50, each = 2)
dat$Z_pairs <- block_ra(dat$pairs, prob = 0.5)

table(dat$pairs, dat$Z_pairs)
#>     
#>      0 1
#>   1  1 1
#>   2  1 1
#>   3  1 1
#>   4  1 1
#>   5  1 1
#>   6  1 1
#>   7  1 1
#>   8  1 1
#>   9  1 1
#>   10 1 1
#>   11 1 1
#>   12 1 1
#>   13 1 1
#>   14 1 1
#>   15 1 1
#>   16 1 1
#>   17 1 1
#>   18 1 1
#>   19 1 1
#>   20 1 1
#>   21 1 1
#>   22 1 1
#>   23 1 1
#>   24 1 1
#>   25 1 1
#>   26 1 1
#>   27 1 1
#>   28 1 1
#>   29 1 1
#>   30 1 1
#>   31 1 1
#>   32 1 1
#>   33 1 1
#>   34 1 1
#>   35 1 1
#>   36 1 1
#>   37 1 1
#>   38 1 1
#>   39 1 1
#>   40 1 1
#>   41 1 1
#>   42 1 1
#>   43 1 1
#>   44 1 1
#>   45 1 1
#>   46 1 1
#>   47 1 1
#>   48 1 1
#>   49 1 1
#>   50 1 1
difference_in_means(Y ~ Z_pairs, blocks = pairs, data = dat)
#> Design:  Matched-pair 
#>            Estimate Std. Error    t value  Pr(>|t|)   CI Lower  CI Upper DF
#> Z_pairs -0.09532749   0.212814 -0.4479381 0.6561722 -0.5229932 0.3323383 49

# ----------
# 6. Matched-pair cluster randomized
# ----------
# Learns this design if there are two clusters per block
dat$small_clust <- rep(1:50, each = 2)
dat$cluster_pairs <- rep(1:25, each = 4)
table(dat$cluster_pairs, dat$small_clust)
#>     
#>      1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#>   1  2 2 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   2  0 0 2 2 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   3  0 0 0 0 2 2 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   4  0 0 0 0 0 0 2 2 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   5  0 0 0 0 0 0 0 0 2  2  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   6  0 0 0 0 0 0 0 0 0  0  2  2  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   7  0 0 0 0 0 0 0 0 0  0  0  0  2  2  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   8  0 0 0 0 0 0 0 0 0  0  0  0  0  0  2  2  0  0  0  0  0  0  0  0  0  0  0  0
#>   9  0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  2  2  0  0  0  0  0  0  0  0  0  0
#>   10 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  2  2  0  0  0  0  0  0  0  0
#>   11 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  2  2  0  0  0  0  0  0
#>   12 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  2  2  0  0  0  0
#>   13 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  2  2  0  0
#>   14 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  2  2
#>   15 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   16 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   17 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   18 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   19 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   20 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   21 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   22 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   23 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   24 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   25 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>     
#>      29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
#>   1   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   2   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   3   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   4   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   5   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   6   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   7   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   8   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   9   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   10  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   11  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   12  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   13  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   14  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   15  2  2  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   16  0  0  2  2  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   17  0  0  0  0  2  2  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   18  0  0  0  0  0  0  2  2  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   19  0  0  0  0  0  0  0  0  2  2  0  0  0  0  0  0  0  0  0  0  0  0
#>   20  0  0  0  0  0  0  0  0  0  0  2  2  0  0  0  0  0  0  0  0  0  0
#>   21  0  0  0  0  0  0  0  0  0  0  0  0  2  2  0  0  0  0  0  0  0  0
#>   22  0  0  0  0  0  0  0  0  0  0  0  0  0  0  2  2  0  0  0  0  0  0
#>   23  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  2  2  0  0  0  0
#>   24  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  2  2  0  0
#>   25  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  2  2

dat$Z_mpcl <- block_and_cluster_ra(
  blocks = dat$cluster_pairs,
  clusters = dat$small_clust
 )

difference_in_means(
  Y ~ Z_mpcl,
  blocks = cluster_pairs,
  clusters = small_clust,
  data = dat
 )
#> Design:  Matched-pair clustered 
#>         Estimate Std. Error   t value  Pr(>|t|)   CI Lower  CI Upper DF
#> Z_mpcl 0.1760208  0.2335122 0.7537972 0.4583032 -0.3059246 0.6579662 24

# ----------
# Other examples
# ----------

# Also works with multi-valued treatments if users specify
# comparison of interest
dat$Z_multi <- simple_ra(
  nrow(dat),
  conditions = c("Treatment 2", "Treatment 1", "Control"),
  prob_each = c(0.4, 0.4, 0.2)
)

# Only need to specify which condition is treated `condition2` and
# which is control `condition1`
difference_in_means(
  Y ~ Z_multi,
  condition1 = "Treatment 2",
  condition2 = "Control",
  data = dat
)
#> Design:  Standard 
#>                Estimate Std. Error   t value  Pr(>|t|) CI Lower  CI Upper
#> Z_multiControl -0.36965  0.3302006 -1.119471 0.2731941 -1.04845 0.3091496
#>                      DF
#> Z_multiControl 25.95082
difference_in_means(
  Y ~ Z_multi,
  condition1 = "Treatment 1",
  condition2 = "Control",
  data = dat
)
#> Design:  Standard 
#>                   Estimate Std. Error    t value  Pr(>|t|)   CI Lower  CI Upper
#> Z_multiControl -0.04375527   0.330429 -0.1324196 0.8956773 -0.7231372 0.6356266
#>                      DF
#> Z_multiControl 25.86285

# Specifying weights will result in estimation via lm_robust()
dat$w <- runif(nrow(dat))
difference_in_means(Y ~ Z_comp, weights = w, data = dat)
#> Design:  Standard (weighted) 
#>          Estimate Std. Error    t value  Pr(>|t|)   CI Lower CI Upper DF
#> Z_comp -0.1020411  0.2483971 -0.4107981 0.6821174 -0.5949771 0.390895 98
lm_robust(Y ~ Z_comp, weights = w, data = dat)
#>               Estimate Std. Error    t value  Pr(>|t|)   CI Lower  CI Upper DF
#> (Intercept)  0.2047595  0.1670486  1.2257484 0.2232309 -0.1267430 0.5362621 98
#> Z_comp      -0.1020411  0.2483971 -0.4107981 0.6821174 -0.5949771 0.3908950 98
```
