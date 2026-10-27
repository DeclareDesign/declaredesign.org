# Horvitz-Thompson Estimator with Inverse Probability Weighting

Estimates treatment effects via inverse probability weighting when
treatment assignment probabilities are known. Supports all `randomizr`
designs as well as arbitrary designs supplied via a permutation matrix.

## Usage

``` r
horvitz_thompson(
  formula,
  data,
  condition_prs = NULL,
  condition1 = NULL,
  condition2 = NULL,
  se_type = "youngs",
  ci = TRUE,
  alpha = 0.05
)
```

## Arguments

- formula:

  (required) A formula `Y ~ Z`, with one outcome.

- data:

  (optional) A `data.frame` with one row per unit of the design.

- condition_prs:

  (required) Treatment probability specification. One of:

  - An `ra_declaration` from `randomizr`: **strongly preferred.** All
    standard designs (simple/Bernoulli, complete, blocked, clustered,
    blocked-and-clustered, and arbitrary permutation matrices) are
    supported, and the variance estimator uses exact design-aware joint
    inclusion probabilities. Any design for which you know the block
    structure, cluster structure, marginal treatment probabilities, and
    whether randomization is simple or complete can be expressed as
    `declare_ra(blocks = bl, clusters = cl, prob = pi, simple = FALSE)`.
    There is no parametric design that requires the alternatives below.
    For fully custom designs, use
    `declare_ra(permutation_matrix = perm)`.

  - A named numeric vector of marginal condition probabilities, e.g.
    `c("0" = 0.4, "1" = 0.6)`. Uses the conservative Young's
    simple-randomization variance bound, which is valid for any design
    but exact only for Bernoulli (simple) randomization. For complete or
    blocked designs this overstates uncertainty; use an `ra_declaration`
    to get the tighter design-aware variance.

  - A two-column matrix or data frame of per-unit probabilities with
    columns named by condition labels. Same conservative Young's bound
    as the named vector above.

- condition1:

  (optional) Label of the control condition (first sorted condition by
  default).

- condition2:

  (optional) Label of the treatment condition (second sorted condition
  by default).

- se_type:

  (optional) `"youngs"` (default) or `"none"`.

- ci:

  (optional) Logical; whether to compute p-values and confidence
  intervals.

- alpha:

  (optional) Significance level, 0.05 by default.

## Value

An object of class `"horvitz_thompson"` with fields `coefficients`,
`std.error`, `statistic`, `p.value`, `conf.low`, `conf.high`, `df`,
`nobs` (the number of units in the design, including any arms outside
the contrast), `vcov`, `se_type`, `condition1`, `condition2`, `outcome`,
and `term`.

## Details

With more than two arms, `condition1` and `condition2` select the
contrast, and the estimand remains the average treatment effect over all
N units the design covers. The estimator therefore divides by N, not by
the number of units landing in the two conditions, and the variance uses
the joint assignment probabilities implied by the arm sizes. `data` must
hold one row per unit of the design, in the design's order, including
units assigned to arms outside the contrast.

## Examples

``` r
set.seed(40)
dat <- data.frame(y = rnorm(100), z = rep(0:1, 50))

# A named vector of condition probabilities gives the conservative
# simple-randomization bound, valid for any design
horvitz_thompson(y ~ z, data = dat, condition_prs = c("0" = 0.5, "1" = 0.5))
#> Horvitz-Thompson estimator
#>     Estimate Std. Error     t value  Pr(>|t|)   CI Lower  CI Upper DF
#> 1 -0.0179913  0.2011512 -0.08944168 0.9287309 -0.4122405 0.3762578 NA

# Passing the randomization declaration instead is what buys the
# design-aware variance, and it is the recommended form
if (requireNamespace("randomizr", quietly = TRUE)) {
  decl <- randomizr::declare_ra(N = 100, m = 50)
  dat$z2 <- randomizr::conduct_ra(decl)
  print(horvitz_thompson(y ~ z2, data = dat, condition_prs = decl))

  # Blocked and clustered designs need no extra arguments: the declaration
  # already carries the structure
  bl <- rep(1:4, each = 25)
  decl_bl <- randomizr::declare_ra(blocks = bl, prob = 0.4)
  dat$z3 <- randomizr::conduct_ra(decl_bl)
  print(horvitz_thompson(y ~ z3, data = dat, condition_prs = decl_bl))

  # Any two arms of a multi-arm design can be contrasted, with the estimand
  # still defined over all N units
  decl3 <- randomizr::declare_ra(N = 100, conditions = c("a", "b", "c"))
  dat$z4 <- randomizr::conduct_ra(decl3)
  print(horvitz_thompson(y ~ z4, data = dat, condition_prs = decl3,
                         condition1 = "a", condition2 = "c"))
}
#> Horvitz-Thompson estimator
#>     Estimate Std. Error   t value     Pr(>|t|) CI Lower   CI Upper DF
#> 1 -0.7391882  0.2015963 -3.666676 0.0002457241 -1.13431 -0.3440667 NA
#> Horvitz-Thompson estimator
#>      Estimate Std. Error   t value  Pr(>|t|)   CI Lower  CI Upper DF
#> 1 -0.05154834  0.2070688 -0.248943 0.8034049 -0.4573957 0.3542991 NA
#> Horvitz-Thompson estimator
#>     Estimate Std. Error    t value  Pr(>|t|)  CI Lower  CI Upper DF
#> c -0.1516154  0.2629791 -0.5765304 0.5642568 -0.667045 0.3638141 NA
```
