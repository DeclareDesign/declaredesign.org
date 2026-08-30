# Absorbing Fixed Effects with estimatr

Whether analyzing a block-randomized experiment or adding fixed effects
for a panel model, absorbing group means can speed up estimation time.
The `fixed_effects` argument in both `lm_robust` and `iv_robust` allows
you to do just that, although the speed gains are greatest with “HC1”
standard errors. Specifying fixed effects is really simple.

``` r

library(estimatr)
lmr_out <- lm_robust(mpg ~ hp, data = mtcars, fixed_effects = ~ cyl)
lmr_out
```

    ##       Estimate Std. Error   t value  Pr(>|t|)    CI Lower    CI Upper DF
    ## hp -0.02403883 0.01503818 -1.598521 0.1211523 -0.05484314 0.006765475 28

``` r

lmr_out$fixed_effects
```

    ##     cyl4     cyl6     cyl8 
    ## 28.65012 22.68246 20.12927

Before proceeding, three quick notes:

- Most of the speed gains occur when estimating “HC1” robust standard
  errors, or “stata” standard errors when there is clustering. This is
  because most of the speed gains come from avoiding inverting a large
  matrix of group dummies, but this step is still necessary for “HC2”,
  “HC3”, and “CR2” standard errors.
- While you can specify multiple sets of fixed effects, such as
  `fixed_effects = ~ year + country`, please ensure that your model is
  well-specified if you do so. If there are dependencies or overlapping
  groups across multiple sets of fixed effects, we cannot guarantee the
  correct degrees of freedom.
- For now, weighted “CR2” estimation is not possible with fixed_effects.

## Speed gains

In general, our speed gains will be greatest as the number of
groups/fixed effects is large relative to the number of observations.
Imagine we have 300 matched-pairs in an experiment.

``` r

# Load packages for comparison
library(microbenchmark)
library(sandwich)
library(lmtest)

# Create matched-pairs dataset using fabricatr
set.seed(40)
library(fabricatr)
dat <- fabricate(
  blocks = add_level(N = 300),
  indiv = add_level(N = 2, z = sample(0:1), y = rnorm(N) + z)
)
head(dat)
```

    ##   blocks indiv z          y
    ## 1    001   001 1  1.4961828
    ## 2    001   002 0 -0.8595843
    ## 3    002   003 1  0.1709400
    ## 4    002   004 0 -0.3215731
    ## 5    003   005 1 -0.3037704
    ## 6    003   006 0 -1.4214866

``` r

# With HC2
microbenchmark(
  `base + sandwich` = {
    lo <- lm(y ~ z + factor(blocks), dat)
    coeftest(lo, vcov = vcovHC(lo, type = "HC2"))
  },
  `lm_robust` = lm_robust(y ~ z + factor(blocks), dat),
  `lm_robust + fes` = lm_robust(y ~ z, data = dat, fixed_effects = ~ blocks),
  times = 50
)
```

    ## Warning in microbenchmark(`base + sandwich` = {: less accurate nanosecond times
    ## to avoid potential integer overflows

    ## Unit: milliseconds
    ##             expr       min        lq      mean    median        uq       max
    ##  base + sandwich 149.29908 162.17780 195.54200 170.86039 215.65500 429.47176
    ##        lm_robust  35.08530  39.60358  45.61251  42.70697  46.51507  87.13607
    ##  lm_robust + fes  22.46497  25.62639  41.79731  27.10043  30.61257 250.98203
    ##  neval cld
    ##     50  a 
    ##     50   b
    ##     50   b

Speed gains are *considerably* greater with HC1 standard errors. This is
because we need to get the hat matrix for HC2, HC3, and CR2 standard
errors, which requires inverting that large matrix of dummies we
previously avoided doing. HC0, HC1, CR0, and CRstata standard errors do
not require this inversion.

``` r

# With HC1
microbenchmark(
  `base + sandwich` = {
    lo <- lm(y ~ z + factor(blocks), dat)
    coeftest(lo, vcov = vcovHC(lo, type = "HC1"))
  },
  `lm_robust` = lm_robust(
    y ~ z + factor(blocks),
    dat,
    se_type = "HC1"
  ),
  `lm_robust + fes` = lm_robust(
    y ~ z, 
    data = dat,
    fixed_effects = ~ blocks,
    se_type = "HC1"
  ),
  times = 50
)
```

    ## Unit: milliseconds
    ##             expr        min         lq       mean     median         uq
    ##  base + sandwich 144.887850 156.892281 169.934668 163.651951 174.805550
    ##        lm_robust  28.209189  32.428417  38.379638  33.873134  36.266673
    ##  lm_robust + fes   2.590093   3.292997   4.923895   3.483217   3.830015
    ##        max neval cld
    ##  265.22839    50 a  
    ##  133.95573    50  b 
    ##   49.54194    50   c
