# Horvitz-Thompson estimator for two-armed trials

Horvitz-Thompson estimators that are unbiased for designs in which the
randomization scheme is known

## Usage

``` r
horvitz_thompson(
  formula,
  data,
  blocks,
  clusters,
  simple = NULL,
  condition_prs,
  condition_pr_mat = NULL,
  ra_declaration = NULL,
  subset,
  condition1 = NULL,
  condition2 = NULL,
  se_type = c("youngs", "constant", "none"),
  ci = TRUE,
  alpha = 0.05,
  return_condition_pr_mat = FALSE
)
```

## Arguments

- formula:

  an object of class formula, as in
  [`lm`](https://rdrr.io/r/stats/lm.html), such as `Y ~ Z` with only one
  variable on the right-hand side, the treatment.

- data:

  A data.frame.

- blocks:

  An optional bare (unquoted) name of the block variable. Use for
  blocked designs only. See details.

- clusters:

  An optional bare (unquoted) name of the variable that corresponds to
  the clusters in the data; used for cluster randomized designs. For
  blocked designs, clusters must be within blocks.

- simple:

  logical, optional. Whether the randomization is simple (TRUE) or
  complete (FALSE). This is ignored if `blocks` are specified, as all
  blocked designs use complete randomization, or either `ra_declaration`
  or `condition_pr_mat` are passed. Otherwise, it defaults to `TRUE`.

- condition_prs:

  An optional bare (unquoted) name of the variable with the condition 2
  (treatment) probabilities. See details. May also use a single number
  for the condition 2 probability if it is constant.

- condition_pr_mat:

  An optional 2n \* 2n matrix of marginal and joint probabilities of all
  units in condition1 and condition2. See details.

- ra_declaration:

  An object of class `"ra_declaration"`, from the
  [`declare_ra`](https://declaredesign.org/r/randomizr/reference/declare_ra.html)
  function in the randomizr package. This is the third way that one can
  specify a design for this estimator. Cannot be used along with any of
  `condition_prs`, `blocks`, `clusters`, or `condition_pr_mat`. See
  details.

- subset:

  An optional bare (unquoted) expression specifying a subset of
  observations to be used.

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

- se_type:

  can be one of `c("youngs", "constant", "none")` and corresponds the
  estimator of the standard errors. Default estimator uses Young's
  inequality (and is conservative) while the other uses a constant
  treatment effects assumption and only works for simple randomized
  designs at the moment. If "none" then standard errors will not be
  computed which may speed up run time if only the point estimate is
  required.

- ci:

  logical. Whether to compute and return p-values and confidence
  intervals, TRUE by default.

- alpha:

  The significance level, 0.05 by default.

- return_condition_pr_mat:

  logical. Whether to return the condition probability matrix. Returns
  NULL if the design is simple randomization, FALSE by default.

## Value

Returns an object of class `"horvitz_thompson"`.

The post-estimation commands functions `summary` and
[`tidy`](https://generics.r-lib.org/reference/tidy.html) return results
in a `data.frame`. To get useful data out of the return, you can use
these data frames, you can use the resulting list directly, or you can
use the generic accessor functions `coef` and `confint`.

An object of class `"horvitz_thompson"` is a list containing at least
the following components:

- coefficients:

  the estimated difference in totals

- std.error:

  the estimated standard error

- statistic:

  the z-statistic

- df:

  the estimated degrees of freedom

- p.value:

  the p-value from a two-sided z-test using `coefficients` and
  `std.error`

- conf.low:

  the lower bound of the `1 - alpha` percent confidence interval

- conf.high:

  the upper bound of the `1 - alpha` percent confidence interval

- term:

  a character vector of coefficient names

- alpha:

  the significance level specified by the user

- nobs:

  the number of observations used

- outcome:

  the name of the outcome variable

- condition_pr_mat:

  the condition probability matrix if `return_condition_pr_mat` is TRUE

## Details

This function implements the Horvitz-Thompson estimator for treatment
effects for two-armed trials. This estimator is useful for estimating
unbiased treatment effects given any randomization scheme as long as the
randomization scheme is known.

In short, the Horvitz-Thompson estimator essentially reweights each unit
by the probability of it being in its observed condition. Pivotal to the
estimation of treatment effects using this estimator are the marginal
condition probabilities (i.e., the probability that any one unit is in a
particular treatment condition). Pivotal to estimating the variance
whenever the design is more complicated than simple randomization are
the joint condition probabilities (i.e., the probabilities that any two
units have a particular set of treatment conditions, either the same or
different). The estimator we provide here considers the case with two
treatment conditions.

Users interested in more details can see the [mathematical
notes](https://declaredesign.org/r/estimatr/articles/mathematical-notes.html)
for more information and references, or see the references below.

There are three distinct ways that users can specify the design to the
function. The preferred way is to use the
[`declare_ra`](https://declaredesign.org/r/randomizr/reference/declare_ra.html)
function in the randomizr package. This function takes several
arguments, including blocks, clusters, treatment probabilities, whether
randomization is simple or not, and more. Passing the outcome of that
function, an object of class `"ra_declaration"` to the `ra_declaration`
argument in this function, will lead to a call of the
[`declaration_to_condition_pr_mat`](https://declaredesign.org/r/estimatr/reference/declaration_to_condition_pr_mat.md)
function which generates the condition probability matrix needed to
estimate treatment effects and standard errors. We provide many examples
below of how this could be done.

The second way is to pass the names of vectors in your `data` to
`condition_prs`, `blocks`, and `clusters`. You can further specify
whether the randomization was simple or complete using the `simple`
argument. Note that if `blocks` are specified the randomization is
always treated as complete. From these vectors, the function learns how
to build the condition probability matrix that is used in estimation.

In the case where `condition_prs` is specified, this function assumes
those probabilities are the marginal probability that each unit is in
condition2 and then uses the other arguments (`blocks`, `clusters`,
`simple`) to learn the rest of the design. If users do not pass
`condition_prs`, this function learns the probability of being in
condition2 from the data. That is, none of these arguments are
specified, we assume that there was a simple randomization where the
probability of each unit being in condition2 was the average of all
units in condition2. Similarly, we learn the block-level probability of
treatment within `blocks` by looking at the mean number of units in
condition2 if `condition_prs` is not specified.

The third way is to pass a `condition_pr_mat` directly. One can see more
about this object in the documentation for
[`declaration_to_condition_pr_mat`](https://declaredesign.org/r/estimatr/reference/declaration_to_condition_pr_mat.md)
and
[`permutations_to_condition_pr_mat`](https://declaredesign.org/r/estimatr/reference/permutations_to_condition_pr_mat.md).
Essentially, this 2n \* 2n matrix allows users to specify marginal and
joint marginal probabilities of units being in conditions 1 and 2 of
arbitrary complexity. Users should only use this option if they are
certain they know what they are doing.

## References

Aronow, Peter M, and Joel A Middleton. 2013. "A Class of Unbiased
Estimators of the Average Treatment Effect in Randomized Experiments."
Journal of Causal Inference 1 (1): 135-54.
[doi:10.1515/jci-2012-0009](https://doi.org/10.1515/jci-2012-0009) .

Aronow, Peter M, and Cyrus Samii. 2017. "Estimating Average Causal
Effects Under Interference Between Units." Annals of Applied Statistics,
forthcoming. <https://arxiv.org/abs/1305.6156v3>.

Middleton, Joel A, and Peter M Aronow. 2015. "Unbiased Estimation of the
Average Treatment Effect in Cluster-Randomized Experiments." Statistics,
Politics and Policy 6 (1-2): 39-75.
[doi:10.1515/spp-2013-0002](https://doi.org/10.1515/spp-2013-0002) .

## See also

[`declare_ra`](https://declaredesign.org/r/randomizr/reference/declare_ra.html)

## Examples

``` r

# Set seed
set.seed(42)

# Simulate data
n <- 10
dat <- data.frame(y = rnorm(n))

library(randomizr)

#----------
# 1. Simple random assignment
#----------
dat$p <- 0.5
dat$z <- rbinom(n, size = 1, prob = dat$p)

# If you only pass condition_prs, we assume simple random sampling
horvitz_thompson(y ~ z, data = dat, condition_prs = p)
#>     Estimate Std. Error    t value Pr(>|t|)  CI Lower  CI Upper DF
#> z -0.2532128   0.609167 -0.4156706 0.677651 -1.447158 0.9407325 NA
# Assume constant effects instead
horvitz_thompson(y ~ z, data = dat, condition_prs = p, se_type = "constant")
#>     Estimate Std. Error    t value  Pr(>|t|)  CI Lower CI Upper DF
#> z -0.2532128  0.6038814 -0.4193088 0.6749904 -1.436799 0.930373 NA

# Also can use randomizr to pass a declaration
srs_declaration <- declare_ra(N = nrow(dat), prob = 0.5, simple = TRUE)
horvitz_thompson(y ~ z, data = dat, ra_declaration = srs_declaration)
#>     Estimate Std. Error    t value Pr(>|t|)  CI Lower  CI Upper DF
#> z -0.2532128   0.609167 -0.4156706 0.677651 -1.447158 0.9407325 NA

#----------
# 2. Complete random assignment
#----------

dat$z <- sample(rep(0:1, each = n/2))
# Can use a declaration
crs_declaration <- declare_ra(N = nrow(dat), prob = 0.5, simple = FALSE)
horvitz_thompson(y ~ z, data = dat, ra_declaration = crs_declaration)
#>     Estimate Std. Error   t value  Pr(>|t|)  CI Lower  CI Upper DF
#> z -0.2312303  0.5310475 -0.435423 0.6632554 -1.272064 0.8096037 NA
# Can precompute condition_pr_mat and pass it
# (faster for multiple runs with same condition probability matrix)
crs_pr_mat <- declaration_to_condition_pr_mat(crs_declaration)
horvitz_thompson(y ~ z, data = dat, condition_pr_mat = crs_pr_mat)
#>     Estimate Std. Error   t value  Pr(>|t|)  CI Lower  CI Upper DF
#> z -0.2312303  0.5310475 -0.435423 0.6632554 -1.272064 0.8096037 NA

#----------
# 3. Clustered treatment, complete random assigment
#-----------
# Simulating data
dat$cl <- rep(1:4, times = c(2, 2, 3, 3))
dat$prob <- 0.5
clust_crs_decl <- declare_ra(N = nrow(dat), clusters = dat$cl, prob = 0.5)
dat$z <- conduct_ra(clust_crs_decl)
# Easiest to specify using declaration
ht_cl <- horvitz_thompson(y ~ z, data = dat, ra_declaration = clust_crs_decl)
# Also can pass the condition probability and the clusters
ht_cl_manual <- horvitz_thompson(
  y ~ z,
  data = dat,
  clusters = cl,
  condition_prs = prob,
  simple = FALSE
)
ht_cl
#>    Estimate Std. Error   t value  Pr(>|t|)   CI Lower  CI Upper DF
#> z 0.0482231   0.230729 0.2090032 0.8344458 -0.4039975 0.5004437 NA
ht_cl_manual
#>    Estimate Std. Error   t value  Pr(>|t|)   CI Lower  CI Upper DF
#> z 0.0482231   0.230729 0.2090032 0.8344458 -0.4039975 0.5004437 NA

# Blocked estimators specified similarly

#----------
# More complicated assignment
#----------

# arbitrary permutation matrix
possible_treats <- cbind(
  c(1, 1, 0, 1, 0, 0, 0, 1, 1, 0),
  c(0, 1, 1, 0, 1, 1, 0, 1, 0, 1),
  c(1, 0, 1, 1, 1, 1, 1, 0, 0, 0)
)
arb_pr_mat <- permutations_to_condition_pr_mat(possible_treats)
# Simulating a column to be realized treatment
dat$z <- possible_treats[, sample(ncol(possible_treats), size = 1)]
horvitz_thompson(y ~ z, data = dat, condition_pr_mat = arb_pr_mat)
#>    Estimate Std. Error   t value  Pr(>|t|)   CI Lower CI Upper DF
#> z 0.7576713  0.7715048 0.9820695 0.3260656 -0.7544502 2.269793 NA
```
