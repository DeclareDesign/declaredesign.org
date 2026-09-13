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
    ##             expr       min        lq      mean    median        uq      max
    ##  base + sandwich 167.05302 202.44525 231.72770 218.86577 243.37592 420.6880
    ##        lm_robust  43.59637  51.81625  60.75319  56.87819  67.14468 115.4420
    ##  lm_robust + fes  27.37931  31.35897  49.78195  36.24769  44.40767 214.2752
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
    ##  base + sandwich 194.147259 209.667481 226.149932 216.155731 229.103326
    ##        lm_robust  39.302518  43.881808  56.623258  48.915194  57.790812
    ##  lm_robust + fes   3.410175   4.002215   6.519763   5.072458   9.120983
    ##        max neval cld
    ##  364.66183    50 a  
    ##  162.66574    50  b 
    ##   21.45382    50   c
