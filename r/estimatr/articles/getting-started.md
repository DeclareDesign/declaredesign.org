# Getting started using estimatr

**estimatr** is a package in R dedicated to providing
[fast](https://declaredesign.org/r/estimatr/articles/benchmarking-estimatr.md)
estimators that take into consideration designs often used by social
scientists. Estimators are statistical methods for estimating quantities
of interest like treatment effects or regression parameters. Many of the
estimators included with the R programming language or popular R
packages are slow and have default settings that lead to statistically
inappropriate estimates. Certain estimators that reflect cutting-edge
advances in statistics are not yet implemented in R packages for
convenient use. **estimatr** is designed to solve these problems and
provide estimators tuned for design-based inference.

The most up-to-date version of this vignette can be found on the
[DeclareDesign website
here](https://declaredesign.org/r/estimatr/articles/getting-started.html).

## Estimators

The current estimators we provide are:

- [`lm_robust`](#lm_robust) - for fitting linear models with
  heteroskedasticity/cluster-robust standard errors
- [`lm_lin`](#lm_lin) - a wrapper for
  [`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
  to simplify interacting centered pre-treatment covariates with a
  treatment variable
- [`iv_robust`](#iv_robust) - two stage least squares estimation of
  instrumental variables regression
- [`difference_in_means`](#difference_in_means) - for estimating
  differences in means with appropriate standard errors for
  unit-randomized, cluster-randomized, block-randomized, matched-pair
  randomized, and matched-pair clustered designs
- [`horvitz_thompson`](#horvitz_thompson) - for estimating average
  treatment effects taking into consideration treatment probabilities or
  sampling probabilities for simple and cluster randomized designs

I first create some sample data to demonstrate how to use each of these
estimators.

``` r

library(estimatr)

# Example dataset to be used throughout built using fabricatr and randomizr
set.seed(42)
library(fabricatr)
library(randomizr)
dat <- fabricate(
  N = 100,                        # sample size
  x = runif(N, 0, 1),             # pre-treatment covariate
  y0 = rnorm(N, mean = x),        # control potential outcome
  y1 = y0 + 0.35,                 # treatment potential outcome
  z = complete_ra(N),             # complete random assignment to treatment
  y = ifelse(z, y1, y0),          # observed outcome

  # We will also consider clustered data
  clust = sample(rep(letters[1:20], each = 5)),
  z_clust = cluster_ra(clust),
  y_clust = ifelse(z_clust, y1, y0)
)

head(dat)
```

| ID  |    x |   y0 |  y1 |   z |    y | clust | z_clust | y_clust |
|:----|-----:|-----:|----:|----:|-----:|:------|--------:|--------:|
| 001 | 0.91 | 1.24 | 1.6 |   1 | 1.59 | s     |       1 |    1.59 |
| 002 | 0.94 | 0.15 | 0.5 |   1 | 0.50 | q     |       0 |    0.15 |
| 003 | 0.29 | 1.86 | 2.2 |   1 | 2.21 | b     |       1 |    2.21 |
| 004 | 0.83 | 1.47 | 1.8 |   1 | 1.82 | k     |       1 |    1.82 |
| 005 | 0.64 | 0.73 | 1.1 |   0 | 0.73 | b     |       1 |    1.08 |
| 006 | 0.52 | 0.80 | 1.1 |   0 | 0.80 | o     |       0 |    0.80 |

### `lm_robust`

The `estimatr` package provides
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
to quickly fit linear models with the most common variance estimators
and degrees of freedom corrections used in social science. You can
easily estimate heteroskedastic standard errors, clustered standard
errors, and classical standard errors.

Usage largely mimics [`lm()`](https://rdrr.io/r/stats/lm.html), although
it defaults to using Eicker-Huber-White robust standard errors,
specifically “HC2” standard errors. More about the exact specifications
used can be found in the [mathematical
notes](https://declaredesign.org/r/estimatr/articles/mathematical-notes.html#lm_robust-notes)
and more about the estimator can be found on its reference page:
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md).

``` r

lmout <- lm_robust(y ~ z + x, data = dat)
summary(lmout)
#> 
#> Call:
#> lm_robust(formula = y ~ z + x, data = dat)
#> 
#> Standard error type:  HC2 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> (Intercept)   -0.183      0.172   -1.07 2.89e-01   -0.524    0.158 97
#> z              0.206      0.185    1.11 2.70e-01   -0.163    0.574 97
#> x              1.439      0.287    5.01 2.40e-06    0.869    2.008 97
#> 
#> Multiple R-squared:  0.192 , Adjusted R-squared:  0.176 
#> F-statistic: 13.6 on 2 and 97 DF,  p-value: 6.05e-06
```

Users can also easily get the output as a data.frame by using
[`tidy()`](https://generics.r-lib.org/reference/tidy.html).

``` r

tidy(lmout)
```

| term        | estimate | std.error | statistic | p.value | conf.low | conf.high |  df | outcome |
|:------------|---------:|----------:|----------:|--------:|---------:|----------:|----:|:--------|
| (Intercept) |    -0.18 |      0.17 |      -1.1 |    0.29 |    -0.52 |      0.16 |  97 | y       |
| z           |     0.21 |      0.19 |       1.1 |    0.27 |    -0.16 |      0.57 |  97 | y       |
| x           |     1.44 |      0.29 |       5.0 |    0.00 |     0.87 |      2.01 |  97 | y       |

It is straightforward to do cluster-robust inference, by passing the
name of your cluster variable to the `clusters =` argument. The default
variance estimator with clusters is dubbed ‘CR2’ because it is analogous
to ‘HC2’ for the clustered case, and utilizes recent advances proposed
by Pustejovsky and Tipton ([2018](#ref-pustejovskytipton2016)) to
correct hypotheses tests for small samples and work with commonly
specified fixed effects and weights. Note that
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
is quicker if your cluster variable is a factor!

``` r

# Standard estimator with clustered assignment 'z_clust'
lmout <- lm_robust(
  y_clust ~ z_clust + x,
  data = dat
)
```

| term        | estimate | std.error | statistic | p.value | conf.low | conf.high |  df | outcome |
|:------------|---------:|----------:|----------:|--------:|---------:|----------:|----:|:--------|
| (Intercept) |    -0.30 |      0.17 |      -1.7 |    0.09 |    -0.64 |      0.04 |  97 | y_clust |
| z_clust     |     0.45 |      0.19 |       2.4 |    0.02 |     0.08 |      0.82 |  97 | y_clust |
| x           |     1.42 |      0.28 |       5.0 |    0.00 |     0.86 |      1.99 |  97 | y_clust |

``` r

# With clustered standard errors
lmout_cl <- lm_robust(
  y_clust ~ z_clust + x,
  data = dat,
  clusters = clust
)
tidy(lmout_cl)
```

| term        | estimate | std.error | statistic | p.value | conf.low | conf.high |  df | outcome |
|:------------|---------:|----------:|----------:|--------:|---------:|----------:|----:|:--------|
| (Intercept) |    -0.30 |      0.20 |      -1.5 |    0.16 |    -0.72 |      0.13 |  14 | y_clust |
| z_clust     |     0.45 |      0.18 |       2.5 |    0.02 |     0.06 |      0.83 |  18 | y_clust |
| x           |     1.42 |      0.27 |       5.4 |    0.00 |     0.86 |      1.99 |  16 | y_clust |

Researchers can also replicate Stata’s standard errors by using the
`se_type =` argument both with and without clusters:

``` r

lmout_stata <- lm_robust(
  y_clust ~ z_clust + x,
  data = dat,
  clusters = clust,
  se_type = "stata"
)
tidy(lmout_stata)
```

| term        | estimate | std.error | statistic | p.value | conf.low | conf.high |  df | outcome |
|:------------|---------:|----------:|----------:|--------:|---------:|----------:|----:|:--------|
| (Intercept) |    -0.30 |      0.20 |      -1.5 |    0.15 |    -0.71 |      0.12 |  19 | y_clust |
| z_clust     |     0.45 |      0.18 |       2.5 |    0.02 |     0.07 |      0.82 |  19 | y_clust |
| x           |     1.42 |      0.27 |       5.3 |    0.00 |     0.87 |      1.98 |  19 | y_clust |

Furthermore, users can take advantage of the margins package to get
marginal effects, average marginal effects and their standard errors,
and more. Similarly, the prediction package from the same author also
provides a suite of software for different kinds of predictions.

``` r

library(margins)

lmout_int <- lm_robust(y ~ x * z, data = dat)
mar_int <- margins(lmout_int, vce = "delta")
summary(mar_int)
#>  factor    AME     SE      z      p   lower  upper
#>       x 1.4401 0.2886 4.9905 0.0000  0.8745 2.0057
#>       z 0.2056 0.1861 1.1048 0.2692 -0.1592 0.5704

library(prediction)
prediction(lmout_int)
#> Data frame with 100 predictions from
#>  lm_robust(formula = y ~ x * z, data = dat)
#> with average prediction: 0.6742
prediction(lmout_int, at = list(x = c(-0.5, 0.5)))
#> Warning in check_values(data, at): A 'at' value for 'x' is outside observed
#> data range (0.000238896580412984,0.988891728920862)!
#> Data frame with 200 predictions from
#>  lm_robust(formula = y ~ x * z, data = dat)
#> with average predictions:
#>     x       x
#>  -0.5 -0.8006
#>   0.5  0.6395
```

Users who want their regression output in LaTeX or HTML can use the
[`texreg`](https://github.com/leifeld/texreg) package, which we extend
for the output of both the
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
and
[`lm_lin()`](https://declaredesign.org/r/estimatr/reference/lm_lin.md)
functions.

``` r

library(texreg)

texreg(lmout)
```

### `lm_lin`

Adjusting for pre-treatment covariates when using regression to estimate
treatment effects is common practice across scientific disciplines.
However, Freedman ([2008](#ref-freedman2008)) demonstrated that
pre-treatment covariate adjustment biases estimates of average treatment
effects. In response, Lin ([2013](#ref-lin2013)) proposed an alternative
estimator that would reduce this bias and improve precision. Lin
([2013](#ref-lin2013)) proposes centering all pre-treatment covariates,
interacting them with the treatment variable, and regressing the outcome
on the treatment, the centered pre-treatment covariates, and all of the
interaction terms. This can require a non-trivial amount of data
pre-processing.

To facilitate this, we provide a wrapper that processes the data and
estimates the model. We dub this estimator the Lin estimator and it can
be accessed using
[`lm_lin()`](https://declaredesign.org/r/estimatr/reference/lm_lin.md).
This function is a wrapper for
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md),
and all arguments that work for
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
work here. The only difference is in the second argument `covariates`,
where one specifies a right-sided formula with all of your pre-treatment
covariates. Below is an example, and more can be seen on the function
reference page [`lm_lin`](#lm_lin) and some formal notation can be seen
in the [mathematical
notes](https://declaredesign.org/r/estimatr/articles/mathematical-notes.html#lm_lin-notes).

``` r

lml_out <- lm_lin(
  y ~ z,
  covariates = ~ x,
  data = dat
)
tidy(lml_out)
```

| term        | estimate | std.error | statistic | p.value | conf.low | conf.high |  df | outcome |
|:------------|---------:|----------:|----------:|--------:|---------:|----------:|----:|:--------|
| (Intercept) |     0.57 |      0.12 |      4.79 |    0.00 |     0.33 |      0.81 |  96 | y       |
| z           |     0.21 |      0.19 |      1.10 |    0.27 |    -0.16 |      0.58 |  96 | y       |
| x_c         |     1.59 |      0.41 |      3.84 |    0.00 |     0.77 |      2.41 |  96 | y       |
| z:x_c       |    -0.30 |      0.58 |     -0.52 |    0.60 |    -1.45 |      0.84 |  96 | y       |

The output of a
[`lm_lin()`](https://declaredesign.org/r/estimatr/reference/lm_lin.md)
call can be used with the same methods as
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md),
including the [`margins`](https://github.com/leeper/margins) package.

### `iv_robust`

We also implement a two-stage least squares instrumental variables
estimator. This estimator provides a simple syntax and fast estimation
of standard errors (users can select from the same set of standard error
estimators as in
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)).

``` r

# `x` is endogenous variable and `z` is the instrument
iv_out <- iv_robust(y ~ x | z, data = dat)
summary(iv_out)
#> 
#> Call:
#> iv_robust(formula = y ~ x | z, data = dat)
#> 
#> Standard error type:  HC2 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> (Intercept)    -14.6        120  -0.122    0.903     -253      223 98
#> x               29.2        229   0.128    0.899     -425      483 98
#> 
#> Multiple R-squared:  -67.1 , Adjusted R-squared:  -67.8 
#> F-statistic: 0.0163 on 1 and 98 DF,  p-value: 0.899
```

### `difference_in_means`

While estimating differences in means may seem straightforward, it can
become more complicated in designs with blocks or clusters. In these
cases, estimators need to average over within-block effects and
estimates of variance have to appropriately adjust for features of a
design. We provide support for unit-randomized, cluster-randomized,
block-randomized, matched-pair randomized, and matched-pair clustered
designs. Usage is similar to usage in regression functions. More
examples can be seen on the function reference page,
[`difference_in_means()`](https://declaredesign.org/r/estimatr/reference/difference_in_means.md),
and the actual estimators used can be found in the [mathematical
notes](https://declaredesign.org/r/estimatr/articles/mathematical-notes.html#lm_robust-notes).

``` r

# Simple version
dim_out <- difference_in_means(
  y ~ z,
  data = dat
)
tidy(dim_out)
```

| term | estimate | std.error | statistic | p.value | conf.low | conf.high |  df | outcome |
|:-----|---------:|----------:|----------:|--------:|---------:|----------:|----:|:--------|
| z    |     0.22 |       0.2 |       1.1 |    0.29 |    -0.19 |      0.62 |  97 | y       |

``` r

# Clustered version
dim_out_cl <- difference_in_means(
  y_clust ~ z_clust,
  data = dat,
  clusters = clust
)
tidy(dim_out_cl)
```

| term    | estimate | std.error | statistic | p.value | conf.low | conf.high |  df | outcome |
|:--------|---------:|----------:|----------:|--------:|---------:|----------:|----:|:--------|
| z_clust |     0.51 |      0.19 |       2.8 |    0.01 |     0.12 |       0.9 |  18 | y_clust |

You can check which design was learned and which kind of estimator used
by examining the `design` in the output.

``` r

data(sleep)
dim_mps <- difference_in_means(extra ~ group, data = sleep, blocks = ID)
dim_mps$design
#> [1] "Matched-pair"
```

### `horvitz_thompson`

Horvitz-Thompson estimators yield unbiased treatment effect estimates
when the randomization is known. This is particularly useful when there
are clusters of different sizes being randomized into treatment or when
the treatment assignment is complex and there are dependencies across
units in the probability of being treated. Horvitz-Thompson estimators
require information about the probability each unit is in treatment and
control, as well as the joint probability each unit is in the treatment,
in the control, and in opposite treatment conditions.

The estimator we implement here,
[`horvitz_thompson()`](https://declaredesign.org/r/estimatr/reference/horvitz_thompson.md)
estimates treatment effects for two-armed trials. The easiest way to
specify your design and recover the full set of joint and marginal
probabilities is to declare your randomization scheme by using
[`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md)
from the [`randomizr`](https://declaredesign.org/r/randomizr/) package.
I show some examples of how to do that below. Again, the technical
details for this estimator can be found
[here](https://declaredesign.org/r/estimatr/articles/mathematical-notes.html#horvitz_thompson-notes)
and in references in those notes.

``` r

# Complete random assignment declaration
crs_decl <- declare_ra(
  N = nrow(dat),
  prob = 0.5,
  simple = FALSE
)

ht_comp <- horvitz_thompson(
  y ~ z,
  data = dat,
  ra_declaration = crs_decl
)
tidy(ht_comp)
```

| term | estimate | std.error | statistic | p.value | conf.low | conf.high |  df | outcome |
|:-----|---------:|----------:|----------:|--------:|---------:|----------:|----:|:--------|
| z    |     0.22 |       0.2 |       1.1 |    0.29 |    -0.18 |      0.62 |  NA | y       |

We can also easily estimate treatment effects from a cluster randomized
experiment. Letting `horvitz_thompson` know that the design is clustered
means it uses a collapsed estimator for the variance, described in
Aronow and Middleton ([2013](#ref-aronowmiddleton2013)).

``` r

# Clustered random assignment declaration
crs_clust_decl <- declare_ra(
  N = nrow(dat),
  clusters = dat$clust,
  prob = 0.5,
  simple = FALSE
)

ht_clust <- horvitz_thompson(
  y_clust ~ z_clust,
  data = dat,
  ra_declaration = crs_clust_decl
)
tidy(ht_clust)
```

| term    | estimate | std.error | statistic | p.value | conf.low | conf.high |  df | outcome |
|:--------|---------:|----------:|----------:|--------:|---------:|----------:|----:|:--------|
| z_clust |     0.51 |      0.21 |       2.4 |    0.02 |     0.09 |      0.93 |  NA | y_clust |

You can also build the condition probability matrix
(`condition_prob_mat =`) that
[`horvitz_thompson()`](https://declaredesign.org/r/estimatr/reference/horvitz_thompson.md)
needs from a declaration from the
[`randomizr`](https://declaredesign.org/r/randomizr) package—using
`declaration_to_conditional_pr_mat()`—or from a matrix of permutations
of the treatment vector—using `permutations_to_conditional_pr_mat()`.
**This is largely intended for use by experienced users. Note, that if
one passes a `condition_prob_mat` that indicates clustering, but does
not specify the `clusters` argument, then the collapsed estimator will
not be used.**

``` r

# arbitrary permutation matrix
possible_treats <- cbind(
  c(1, 1, 0, 1, 0, 0, 0, 1, 1, 0),
  c(0, 1, 1, 0, 1, 1, 0, 1, 0, 1),
  c(1, 0, 1, 1, 1, 1, 1, 0, 0, 0)
)
arb_pr_mat <- permutations_to_condition_pr_mat(possible_treats)

# Simulating a column to be realized treatment
dat <- data.frame(
  z = possible_treats[, sample(ncol(possible_treats), size = 1)],
  y = rnorm(nrow(possible_treats))
)

ht_arb <- horvitz_thompson(
  y ~ z,
  data = dat,
  condition_pr_mat = arb_pr_mat
)
tidy(ht_arb)
```

| term | estimate | std.error | statistic | p.value | conf.low | conf.high |  df | outcome |
|:-----|---------:|----------:|----------:|--------:|---------:|----------:|----:|:--------|
| z    |     0.08 |      0.47 |      0.16 |    0.87 |    -0.85 |         1 |  NA | y       |

## References

Aronow, Peter M, and Joel A Middleton. 2013. “A Class of Unbiased
Estimators of the Average Treatment Effect in Randomized Experiments.”
*Journal of Causal Inference* 1 (1): 135–54.
<https://doi.org/10.1515/jci-2012-0009>.

Freedman, David A. 2008. “On Regression Adjustments in Experiments with
Several Treatments.” *The Annals of Applied Statistics*, 176–96.
<https://doi.org/10.1214/07-AOAS143>.

Lin, Winston. 2013. “Agnostic Notes on Regression Adjustments to
Experimental Data: Reexamining Freedman’s Critique.” *The Annals of
Applied Statistics* 7 (1): 295–318.
<https://doi.org/10.1214/12-AOAS583>.

Pustejovsky, James E, and Elizabeth Tipton. 2018. “Small-Sample Methods
for Cluster-Robust Variance Estimation and Hypothesis Testing in Fixed
Effects Models.” *Journal of Business & Economic Statistics* 36 (4).
<https://doi.org/10.1080/07350015.2016.1247004>.
