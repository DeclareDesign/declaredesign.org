# Getting started with estimatr

estimatr provides a small set of estimators for design-based inference,
with the variance estimators and degrees-of-freedom corrections that
social scientists actually use, and it fits them quickly. Base R and
most packages default to classical standard errors, which are rarely if
ever justified by the research design; getting robust or cluster-robust
standard errors usually means fitting a model in one package and
correcting it in another. Here the correction is an argument directly in
the estimation function.

The package supports six estimators:

| function | what it estimates |
|----|----|
| [`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md) | linear regression with heteroskedasticity-robust or cluster-robust standard errors |
| [`lm_lin()`](https://declaredesign.org/r/estimatr/reference/lm_lin.md) | a treatment effect adjusted for pre-treatment covariates, following Lin ([2013](#ref-lin2013)) |
| [`iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.md) | two-stage least squares |
| [`difference_in_means()`](https://declaredesign.org/r/estimatr/reference/difference_in_means.md) | a difference in means, with the variance that matches the randomization |
| [`horvitz_thompson()`](https://declaredesign.org/r/estimatr/reference/horvitz_thompson.md) | an average treatment effect by inverse probability weighting |
| [`lh_robust()`](https://declaredesign.org/r/estimatr/reference/lh_robust.md) | a linear combination of coefficients, or several tested jointly |

This vignette shows each of them on one running example.
[`vignette("mathematical-notes")`](https://declaredesign.org/r/estimatr/articles/mathematical-notes.md)
gives the definitions and the citations;
[`vignette("estimatr2.0")`](https://declaredesign.org/r/estimatr/articles/estimatr2.0.md)
is for readers coming from estimatr 1.x, and lists what changed.

### The example data

One hundred units in twenty clusters of five, with a pre-treatment
covariate `X`, a sampling weight `W`, ten blocks formed from `X`, and
two potential outcomes. The same experiment gets run three ways
(complete, clustered, and blocked random assignment), and a fourth time
with noncompliance for
[`iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.md).
Each assignment is made in the section that uses it. The assignments
come from randomizr, whose declarations
[`horvitz_thompson()`](https://declaredesign.org/r/estimatr/reference/horvitz_thompson.md)
reads later on.

``` r

library(estimatr)
library(randomizr)
library(dplyr)
```

``` r

set.seed(343)
N <- 100

dat <- tibble(
  X = runif(N),
  cluster = rep(letters[1:20], each = 5),
  block = cut(X, breaks = quantile(X, seq(0, 1, 0.1)), labels = FALSE, include.lowest = TRUE),
  W = runif(N, 0.5, 1.5), # sampling weights, used further down
  Y_Z_0 = rnorm(N, mean = X) + rep(rnorm(20, sd = 0.5), each = 5), # control potential outcome, with a shock shared within each cluster
  Y_Z_1 = Y_Z_0 + 0.35 # treatment potential outcome (constant effects)
)

print(dat, n = 5)
#> # A tibble: 100 × 6
#>        X cluster block     W  Y_Z_0  Y_Z_1
#>    <dbl> <chr>   <int> <dbl>  <dbl>  <dbl>
#> 1 0.0812 a           1 0.669 -0.690 -0.340
#> 2 0.467  a           5 1.44   0.415  0.765
#> 3 0.700  a           7 1.44  -0.858 -0.508
#> 4 0.799  a           9 1.12  -0.774 -0.424
#> 5 0.557  a           6 0.917  1.78   2.13 
#> # ℹ 95 more rows
```

### `lm_robust()`

Usage follows [`lm()`](https://rdrr.io/r/stats/lm.html). The difference
is the default standard error: HC2 rather than classical. Start with
complete random assignment of half the units, and reveal the potential
outcome that corresponds to each unit’s assignment.

``` r

dat <- dat |>
  mutate(
    Z = complete_ra(N, m = N / 2),
    Y = if_else(Z == 1, Y_Z_1, Y_Z_0)
  )

fit <- lm_robust(Y ~ Z + X, data = dat)
fit
#>             Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> (Intercept)   0.2956     0.2924   1.011   0.3146 -0.28476   0.8759 97
#> Z             0.2908     0.2204   1.319   0.1902 -0.14670   0.7284 97
#> X             0.7088     0.3732   1.899   0.0605 -0.03189   1.4496 97
```

[`summary()`](https://rdrr.io/r/base/summary.html) adds the R-squared
and the F statistic;
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) returns the
coefficient table as a tibble, which is usually what you want next.

``` r

tidy(fit)
#> # A tibble: 3 × 9
#>   term        estimate std.error statistic p.value conf.low conf.high    df outcome
#>   <chr>          <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 (Intercept)    0.296     0.292      1.01  0.315   -0.285      0.876    97 Y      
#> 2 Z              0.291     0.220      1.32  0.190   -0.147      0.728    97 Y      
#> 3 X              0.709     0.373      1.90  0.0605  -0.0319     1.45     97 Y
```

#### Clustered standard errors

Now assign treatment by cluster instead: ten of the twenty clusters,
with every unit in a cluster sharing its cluster’s assignment.

``` r

dat <- dat |>
  mutate(
    Z_cl = cluster_ra(clusters = cluster, m = 10),
    Y_cl = if_else(Z_cl == 1, Y_Z_1, Y_Z_0)
  )
```

Pass the name of the cluster variable. The default becomes CR2, the
cluster analogue of HC2, using the small-sample correction of
Pustejovsky and Tipton ([2018](#ref-pustejovskytipton2018)).

``` r

# Ignoring the clustered assignment understates the standard error
fit_naive <-
  lm_robust(Y_cl ~ Z_cl + X, data = dat)
tidy(fit_naive)
#> # A tibble: 3 × 9
#>   term        estimate std.error statistic p.value conf.low conf.high    df outcome
#>   <chr>          <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 (Intercept)    0.204     0.232     0.878  0.382   -0.257      0.665    97 Y_cl   
#> 2 Z_cl           0.421     0.200     2.11   0.0378   0.0242     0.818    97 Y_cl   
#> 3 X              0.762     0.338     2.26   0.0264   0.0914     1.43     97 Y_cl

# Accounting for it
fit_cluster_aware <-
  lm_robust(Y_cl ~ Z_cl + X, data = dat, clusters = cluster)
tidy(fit_cluster_aware)
#> # A tibble: 3 × 9
#>   term        estimate std.error statistic p.value conf.low conf.high    df outcome
#>   <chr>          <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 (Intercept)    0.204     0.291     0.701  0.495   -0.421      0.829  13.8 Y_cl   
#> 2 Z_cl           0.421     0.226     1.87   0.0787  -0.0537     0.896  17.6 Y_cl   
#> 3 X              0.762     0.446     1.71   0.106   -0.182      1.71   16.2 Y_cl
```

Notice the degrees of freedom as well as the standard error. Under CR2
they are computed per coefficient by a Satterthwaite approximation, so
they are not the residual degrees of freedom and they differ across the
terms.

#### Choosing a different variance estimator

Without clusters, `se_type` takes `"classical"`, `"HC0"`, `"HC1"`,
`"HC2"`, and `"HC3"`; with clusters, `"CR0"` and `"CR2"`. `"stata"` is
accepted in both cases and reproduces what Stata’s `robust` or `cluster`
option gives, which is HC1 without clusters and CR0 with Stata’s two
small-sample corrections with them.

``` r

fit_HC2 <- lm_robust(Y ~ Z + X, data = dat, se_type = "HC2")

fit_classical <- lm_robust(Y ~ Z + X, data = dat, se_type = "classical")

fit_stata <- lm_robust(Y ~ Z + X, data = dat, se_type = "stata")

# The standard error on Z under each
c(
  HC2 = fit_HC2$std.error[["Z"]],
  classical = fit_classical$std.error[["Z"]],
  stata = fit_stata$std.error[["Z"]]
)
#>       HC2 classical     stata 
#>    0.2204    0.2095    0.2202
```

#### Fixed effects

We can demonstrate how to include fixed effects in an lm_robust call
with a block-randomized experiment:

``` r

dat <- dat |>
  mutate(
    Z_bl = block_ra(blocks = block, prob = 0.5),
    Y_bl = if_else(Z_bl == 1, Y_Z_1, Y_Z_0)
  )
```

`fixed_effects` takes a right-sided formula and absorbs those groups
rather than adding them as dummy columns. The estimates and standard
errors are identical to writing the dummies out. Because `lm_robust`
absorbs rather than building the dummy matrix, the computation is very
fast, even with many fixed effects.

``` r

lm_robust(Y_bl ~ Z_bl, data = dat, fixed_effects = ~ block)
#>      Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> Z_bl   0.5869     0.2052    2.86 0.005278   0.1792   0.9947 89

# The same numbers, using the slower dummy matrix way.
lm_robust(Y_bl ~ Z_bl + factor(block), data = dat)
#>                 Estimate Std. Error  t value Pr(>|t|)  CI Lower CI Upper DF
#> (Intercept)     0.066203     0.3151 0.210077 0.834087 -0.559969   0.6924 89
#> Z_bl            0.586928     0.2052 2.859997 0.005278  0.179161   0.9947 89
#> factor(block)2  0.322272     0.3697 0.871733 0.385700 -0.412297   1.0568 89
#> factor(block)3  0.396278     0.4840 0.818676 0.415158 -0.565513   1.3581 89
#> factor(block)4  0.003426     0.4743 0.007224 0.994252 -0.938962   0.9458 89
#> factor(block)5  0.464038     0.4989 0.930106 0.354832 -0.527283   1.4554 89
#> factor(block)6  0.506161     0.4049 1.249995 0.214578 -0.298427   1.3107 89
#> factor(block)7  0.554212     0.4234 1.308923 0.193930 -0.287096   1.3955 89
#> factor(block)8  0.439560     0.3519 1.248941 0.214962 -0.259749   1.1389 89
#> factor(block)9  0.785039     0.4207 1.866041 0.065329 -0.050878   1.6210 89
#> factor(block)10 0.846151     0.4222 2.004132 0.048096  0.007242   1.6851 89
```

The absorbed levels are kept in the `felevels` element of the result.
One caution: `fixed_effects` combined with `clusters` defaults to
`se_type = "CR0"`, because CR2 is the one estimator that still has to
expand the dummies, and it warns once per session to say so. Asking for
`se_type = "CR2"` by name gets it. The full menu by case is in
[`?lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.md).

#### Weights

`weights` takes a column of the data, and the fit is weighted least
squares with the matching robust variance.

``` r

weighted_fit <- lm_robust(Y ~ Z + X, data = dat, weights = W)
tidy(weighted_fit)
#> # A tibble: 3 × 9
#>   term        estimate std.error statistic p.value conf.low conf.high    df outcome
#>   <chr>          <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 (Intercept)    0.180     0.323     0.556  0.580   -0.462      0.821    97 Y      
#> 2 Z              0.384     0.243     1.58   0.117   -0.0978     0.865    97 Y      
#> 3 X              0.824     0.401     2.06   0.0426   0.0282     1.62     97 Y
```

Weighted HC2 and HC3 differ from Stata’s, because the two use different
definitions of the hat matrix.
[`vignette("mathematical-notes")`](https://declaredesign.org/r/estimatr/articles/mathematical-notes.md)
sets out the difference and why estimatr follows the R convention.

### `lm_lin()`

Adjusting for pre-treatment covariates in an experimental context can
increase precision, but Freedman ([2008](#ref-freedman2008)) showed that
doing it with an ordinary regression can bias the estimated treatment
effect and can in some cases *reduce* precision. Lin
([2013](#ref-lin2013)) proposed a fix for the precision problem: center
every covariate, interact each with treatment, and regress the outcome
on treatment, the centered covariates, and the interactions.

[`lm_lin()`](https://declaredesign.org/r/estimatr/reference/lm_lin.md)
does the pre-processing for you. It takes the treatment in `formula` and
the covariates in a right-sided `covariates` formula; everything else is
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)’s.

``` r

lin_fit <- lm_lin(Y ~ Z, covariates = ~ X, data = dat)
tidy(lin_fit)
#> # A tibble: 4 × 9
#>   term        estimate std.error statistic  p.value conf.low conf.high    df outcome
#>   <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 (Intercept)    0.642     0.172     3.72  0.000333    0.300     0.984    96 Y      
#> 2 Z              0.290     0.221     1.31  0.193      -0.149     0.730    96 Y      
#> 3 X_c            0.759     0.513     1.48  0.142      -0.259     1.78     96 Y      
#> 4 Z:X_c         -0.107     0.757    -0.142 0.888      -1.61      1.40     96 Y
```

The `Z` row is the estimate of the average treatment effect, since when
the covariates are at their centered means of zero, the interaction
terms drop out. The centers used are returned in `scaled_center`, and
centering happens after any function in the formula is evaluated, so
`~ log(X)` centers the log.

``` r

lin_fit$scaled_center
#>      X 
#> 0.4943
```

### `iv_robust()`

Instrumental variables estimation comes up most often in experiments
with noncompliance. The random assignment `Z` says who was encouraged to
take the treatment, and `D` records who actually took it. Here 60
percent of units are compliers, who take the treatment if and only if
assigned; the rest never take it, whatever their assignment (one-sided
noncompliance). Outcomes follow treatment receipt, not assignment.

``` r

dat <- dat |>
  mutate(
    Y_D_0 = Y_Z_0, # potential outcome if treatment is not received
    Y_D_1 = Y_D_0 + 0.35, # potential outcome if treatment is received
    complier = rbinom(N, size = 1, prob = 0.6),
    D = Z * complier,
    Y_nc = if_else(D == 1, Y_D_1, Y_D_0)
  )
```

Because `Z` is randomized, it is an instrument for `D`, and two-stage
least squares estimates the complier average causal effect ([Gerber and
Green 2012](#ref-gerbergreen2012)), which is 0.35 here because the
effect is constant.
[`iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.md)
takes the same `se_type` menu as
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md).
The instruments go after a vertical bar, and a covariate goes on both
sides of it.

``` r

iv_fit <- iv_robust(Y_nc ~ D + X | Z + X, data = dat)
summary(iv_fit)
#> 
#> Call:
#> iv_robust(formula = Y_nc ~ D + X | Z + X, data = dat)
#> 
#> Standard error type:  HC2 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> (Intercept)    0.310      0.337   0.922    0.359   -0.358    0.979 97
#> D              0.257      0.349   0.736    0.464   -0.436    0.950 97
#> X              0.683      0.430   1.586    0.116   -0.172    1.537 97
#> 
#> Multiple R-squared:  0.0757 ,    Adjusted R-squared:  0.0567 
#> F-statistic: 1.31 on 2 and 97 DF,  p-value: 0.274
```

`diagnostics = TRUE` adds the weak-instrument, Wu-Hausman, and
overidentification tests; the last is Sargan’s under
`se_type = "classical"` and Wooldridge’s robust score test otherwise.
[`residuals()`](https://rdrr.io/r/stats/residuals.html) returns the
structural residuals, $`y - X\widehat{\beta}`$, rather than the
second-stage ones.

### `difference_in_means()`

A difference in means is simple until the design is blocked or
clustered, at which point the point estimate has to average over blocks
and the variance has to reflect how units were actually assigned.
[`difference_in_means()`](https://declaredesign.org/r/estimatr/reference/difference_in_means.md)
picks the estimator that matches the design and tells you which one it
picked.

``` r

dim_fit <- difference_in_means(Y ~ Z, data = dat)
dim_fit
#> Design:  Standard 
#>   Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper    DF
#> Z   0.1801     0.2034  0.8853   0.3783  -0.2238    0.584 95.12
dim_fit$design
#> [1] "Standard"
```

With no blocks or clusters, the variance and degrees of freedom are what
[`t.test()`](https://rdrr.io/r/stats/t.test.html) computes. Pass
`clusters` and the design changes, and so does the estimator:

``` r

dim_cl <- difference_in_means(Y_cl ~ Z_cl, data = dat, clusters = cluster)
dim_cl$design
#> [1] "Clustered"
tidy(dim_cl)
#> # A tibble: 1 × 9
#>   term  estimate std.error statistic p.value conf.low conf.high    df outcome
#>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 Z_cl     0.347     0.221      1.57   0.134   -0.118     0.811    18 Y_cl
```

`blocks` works the same way, and blocks need not all be the same shape.
Blocks with at least two units in each arm carry their own variance;
blocks with a singleton arm have their variance estimated across such
blocks, following Pashley and Miratrix
([2021](#ref-pashleymiratrix2021)). Check `design` rather than assuming,
and see
[`?difference_in_means`](https://declaredesign.org/r/estimatr/reference/difference_in_means.md)
for the full classification and the two designs it refuses.

``` r

# The blocked version of the experiment
dim_bl <- difference_in_means(Y_bl ~ Z_bl, data = dat, blocks = block)
dim_bl$design
#> [1] "Blocked"
tidy(dim_bl)
#> # A tibble: 1 × 9
#>   term  estimate std.error statistic p.value conf.low conf.high    df outcome
#>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 Z_bl     0.587     0.203      2.90 0.00488    0.183     0.990    80 Y_bl

# R's sleep data, ten patients each measured under two drugs, is a
# matched-pairs design, and is recognised as such
difference_in_means(extra ~ group, data = sleep, blocks = ID)$design
#> [1] "Matched-pair"
```

### `horvitz_thompson()`

When you know how treatment was assigned, you can estimate the average
treatment effect without a model, by weighting each observed outcome by
the inverse of the probability of the condition it was observed under.
The estimator is unbiased, and it handles designs that regression
handles awkwardly: clusters of unequal size, per-unit probabilities that
differ, assignment schemes with dependence across units.

`condition_prs` is the one argument that describes the design, and what
you pass decides which variance you get. A named vector of condition
probabilities gives the conservative bound, valid for any design and
tight only for Bernoulli assignment:

``` r

horvitz_thompson(Y ~ Z, data = dat, condition_prs = c("0" = 0.5, "1" = 0.5))
#> Horvitz-Thompson estimator
#>   Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> 1   0.1801     0.2568  0.7014   0.4831  -0.3232   0.6834 NA
```

Passing the randomization declaration instead is what buys the
design-aware variance, because a declaration already carries the block
structure, the cluster structure, the per-unit probabilities and whether
assignment was simple or complete. Clustered and blocked designs need no
further arguments: describe the design once, in the declaration.

``` r

# declare_ra is from the randomizr package
decl <- declare_ra(N = N, prob = 0.5, simple = FALSE)
horvitz_thompson(Y ~ Z, data = dat, condition_prs = decl)
#> Horvitz-Thompson estimator
#>   Estimate Std. Error t value Pr(>|t|) CI Lower CI Upper DF
#> 1   0.1801     0.2032  0.8862   0.3755  -0.2182   0.5784 NA
```

[`?horvitz_thompson`](https://declaredesign.org/r/estimatr/reference/horvitz_thompson.md)
covers the rest: per-unit probability matrices, designs supplied as a
permutation matrix, and contrasting two arms of a multi-arm design.

### `lh_robust()`

To test a linear combination of coefficients, or several at once,
[`lh_robust()`](https://declaredesign.org/r/estimatr/reference/lh_robust.md)
fits the model and applies
[`car::linearHypothesis()`](https://rdrr.io/pkg/car/man/linearHypothesis.html)
to it, keeping the robust variance and the degrees of freedom of the
fit.

``` r

lh_fit <- lh_robust(Y ~ Z + X, data = dat, linear_hypothesis = "Z + 2*X = 0")
tidy(lh_fit)
#> # A tibble: 4 × 9
#>   term        estimate std.error statistic p.value conf.low conf.high    df outcome
#>   <chr>          <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 (Intercept)    0.296     0.292      1.01  0.315   -0.285      0.876    97 Y      
#> 2 Z              0.291     0.220      1.32  0.190   -0.147      0.728    97 Y      
#> 3 X              0.709     0.373      1.90  0.0605  -0.0319     1.45     97 Y      
#> 4 Z + 2*X = 0    1.71      0.869      1.97  0.0521  -0.0159     3.43     97 Y
```

Several restrictions give a joint Wald test as well.

``` r

joint <- lh_robust(Y ~ Z + X, data = dat, linear_hypothesis = c("Z = 0", "X = 0"))
joint$joint_hypothesis
#>   value   numdf   dendf p.value 
#>  1.9360  2.0000 97.0000  0.1498
```

### Working with the output

Every estimator returns an object supporting the S3 methods you would
expect: [`summary()`](https://rdrr.io/r/base/summary.html),
[`print()`](https://rdrr.io/r/base/print.html),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`glance()`](https://generics.r-lib.org/reference/glance.html),
[`coef()`](https://rdrr.io/r/stats/coef.html),
[`confint()`](https://rdrr.io/r/stats/confint.html),
[`vcov()`](https://rdrr.io/r/stats/vcov.html),
[`nobs()`](https://rdrr.io/r/stats/nobs.html), and, for the regression
estimators, [`predict()`](https://rdrr.io/r/stats/predict.html),
[`residuals()`](https://rdrr.io/r/stats/residuals.html) and
[`update()`](https://rdrr.io/r/stats/update.html).

``` r

glance(fit)
#> # A tibble: 1 × 7
#>   r.squared adj.r.squared statistic p.value df.residual  nobs se_type
#>       <dbl>         <dbl>     <dbl>   <dbl>       <int> <int> <chr>  
#> 1    0.0423        0.0226      1.94   0.150          97   100 HC2
confint(fit)
#>                2.5 % 97.5 %
#> (Intercept) -0.28476 0.8759
#> Z           -0.14670 0.7284
#> X           -0.03189 1.4496
```

Regression tables work through the usual packages. `texreg` and
`modelsummary` both take `lm_robust` objects directly.

``` r

texreg::htmlreg(list(fit, lin_fit), include.ci = FALSE, caption = "")
```

|   | Model 1 | Model 2 |
|----|----|----|
| (Intercept) | 0.30 | 0.64^(\*\*\*) |
|   | (0.29) | (0.17) |
| Z | 0.29 | 0.29 |
|   | (0.22) | (0.22) |
| X | 0.71 |   |
|   | (0.37) |   |
| X_c |   | 0.76 |
|   |   | (0.51) |
| Z:X_c |   | -0.11 |
|   |   | (0.76) |
| R² | 0.04 | 0.04 |
| Adj. R² | 0.02 | 0.01 |
| Num. obs. | 100 | 100 |
| RMSE | 1.00 | 1.01 |
| ^(\*\*\*)p \< 0.001; ^(\*\*)p \< 0.01; ^(\*)p \< 0.05 |  |  |

`emmeans` also has methods for `lm_robust` fits, for estimated marginal
means and contrasts, computed with the fit’s robust variance.

``` r

emmeans::emmeans(fit, "Z", at = list(Z = 0:1))
#>  Z emmean    SE df lower.CL upper.CL
#>  0  0.646 0.163 97    0.322     0.97
#>  1  0.937 0.133 97    0.672     1.20
#> 
#> Confidence level used: 0.95
```

### In a simulation

Design-based work means fitting the same model many times over, which is
what estimatr is built to do quickly.
[DeclareDesign](https://declaredesign.org) handles the repetition:
declare the model, the inquiry, the assignment, and the estimator once,
and
[`diagnose_design()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.html)
re-runs the whole experiment and reports how the estimator does against
the truth.

``` r

library(DeclareDesign)

declaration_lin <-
  declare_model(N = 100, 
                X = runif(N), 
                U = rnorm(N, mean = X), 
                potential_outcomes(Y ~ 0.35 * Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, covariates = ~ X, 
                    .method = lm_lin, inquiry = "ATE")

set.seed(343)
diagnose_design(declaration_lin, sims = 500)
#> 
#> Research design diagnosis based on 500 simulations. Diagnosis completed in 4 secs. Diagnosand estimates with bootstrapped standard errors in parentheses (100 replicates).
#> 
#>           Design Inquiry Estimator Outcome Term N Sims Mean Estimand Mean Estimate   Bias
#>  declaration_lin     ATE estimator       Y    Z    500          0.35          0.35  -0.00
#>                                                               (0.00)        (0.01) (0.01)
#>  SD Estimate   RMSE  Power Coverage
#>         0.20   0.20   0.40     0.94
#>       (0.01) (0.01) (0.02)   (0.01)
```

Adding a second
[`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.html)
step, say
`declare_estimator(Y ~ Z, .method = lm_robust, inquiry = "ATE", label = "unadjusted")`,
is how you compare two estimators on one design.

### Where to go next

- [`vignette("mathematical-notes")`](https://declaredesign.org/r/estimatr/articles/mathematical-notes.md)
  for the definition of every estimator and variance above, each with
  its citation and with a live check that estimatr computes what the
  definition says.
- [`vignette("estimatr2.0")`](https://declaredesign.org/r/estimatr/articles/estimatr2.0.md)
  if you are porting code from estimatr 1.x.
- The
  [Performance](https://declaredesign.org/r/estimatr/articles/performance.html)
  page for what each estimator costs at scale.
- The
  [tidyverse](https://declaredesign.org/r/estimatr/articles/estimatr-in-the-tidyverse.html)
  and [regression
  table](https://declaredesign.org/r/estimatr/articles/regression-tables.html)
  pages for what to do with a fit once you have one.

## References

Freedman, David A. 2008. “On Regression Adjustments in Experiments with
Several Treatments.” *The Annals of Applied Statistics* 2 (1): 176–96.
<https://doi.org/10.1214/07-AOAS143>.

Gerber, Alan S., and Donald P. Green. 2012. *Field Experiments: Design,
Analysis, and Interpretation*. W.W. Norton.

Lin, Winston. 2013. “Agnostic Notes on Regression Adjustments to
Experimental Data: Reexamining Freedman’s Critique.” *The Annals of
Applied Statistics* 7 (1): 295–318.
<https://doi.org/10.1214/12-AOAS583>.

Pashley, Nicole E., and Luke W. Miratrix. 2021. “Insights on Variance
Estimation for Blocked and Matched Pairs Designs.” *Journal of
Educational and Behavioral Statistics* 46 (3): 271–96.
<https://doi.org/10.3102/1076998620946272>.

Pustejovsky, James E., and Elizabeth Tipton. 2018. “Small-Sample Methods
for Cluster-Robust Variance Estimation and Hypothesis Testing in Fixed
Effects Models.” *Journal of Business & Economic Statistics* 36 (4):
672–83. <https://doi.org/10.1080/07350015.2016.1247004>.
