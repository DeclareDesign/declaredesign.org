# Glance at an estimatr object

Glance at an estimatr object

## Usage

``` r
# S3 method for class 'lm_robust'
glance(x, ...)

# S3 method for class 'lh_robust'
glance(x, ...)

# S3 method for class 'iv_robust'
glance(x, ...)

# S3 method for class 'difference_in_means'
glance(x, ...)

# S3 method for class 'horvitz_thompson'
glance(x, ...)
```

## Arguments

- x:

  An object returned by one of the estimators

- ...:

  extra arguments (not used)

## Value

For `glance.lm_robust`, a data.frame with columns:

- r.squared:

  the \\R^2\\, \$\$R^2 = 1 - Sum(e\[i\]^2) / Sum((y\[i\] - y^\*)^2),\$\$
  where \\y^\*\\ is the mean of \\y\[i\]\\ if there is an intercept and
  zero otherwise, and \\e\[i\]\\ is the ith residual.

- adj.r.squared:

  the \\R^2\\ but penalized for having more parameters, `rank`

- se_type:

  the standard error type specified by the user

- statistic:

  the value of the F-statistic

- p.value:

  p-value from the F test

- df.residual:

  residual degrees of freedom

- nobs:

  the number of observations used

For `glance.lh_robust`, we glance the `lm_robust` component only. You
can access the linear hypotheses as a data.frame directy from the `lh`
component of the `lh_robust` object

For `glance.iv_robust`, a data.frame with columns:

- r.squared:

  The \\R^2\\ of the second stage regression

- adj.r.squared:

  The \\R^2\\ but penalized for having more parameters, `rank`

- df.residual:

  residual degrees of freedom

- N:

  the number of observations used

- se_type:

  the standard error type specified by the user

- statistic:

  the value of the F-statistic

- p.value:

  p-value from the F test

- statistic.weakinst:

  the value of the first stage F-statistic, useful for the weak
  instruments test; only reported if there is only one endogenous
  variable

- p.value.weakinst:

  p-value from the first-stage F test, a test of weak instruments; only
  reported if there is only one endogenous variable

- statistic.endogeneity:

  the value of the F-statistic for the test of endogeneity; often called
  the Wu-Hausman statistic, with robust standard errors, we employ the
  regression based test

- p.value.endogeneity:

  p-value from the F-test for endogeneity

- statistic.overid:

  the value of the chi-squared statistic for the test of instrument
  correlation with the error term; only reported with overidentification

- p.value.overid:

  p-value from the chi-squared test; only reported with
  overidentification

For `glance.difference_in_means`, a data.frame with columns:

- design:

  the design used, and therefore the estimator used

- df:

  the degrees of freedom

- nobs:

  the number of observations used

- nblocks:

  the number of blocks, if used

- nclusters:

  the number of clusters, if used

- condition2:

  the second, "treatment", condition

- condition1:

  the first, "control", condition

For `glance.horvitz_thompson`, a data.frame with columns:

- nobs:

  the number of observations used

- se_type:

  the type of standard error estimator used

- condition2:

  the second, "treatment", condition

- condition1:

  the first, "control", condition

## See also

[`generics::glance()`](https://generics.r-lib.org/reference/glance.html),
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md),
[`lm_lin()`](https://declaredesign.org/r/estimatr/reference/lm_lin.md),
[`iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.md),
[`difference_in_means()`](https://declaredesign.org/r/estimatr/reference/difference_in_means.md),
[`horvitz_thompson()`](https://declaredesign.org/r/estimatr/reference/horvitz_thompson.md)
